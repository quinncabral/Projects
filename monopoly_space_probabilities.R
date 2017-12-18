library(ggplot2)
library(dplyr)

# 1. function that simulates n turns by a player in a game of Monopoly
#   using two d-sided dice. Outputs the space the player lands on each turn (space 0-39)
simulate_monopoly = function(d, n){
  #simulate n turns with d sided dice (1/d chance of value for each die)
  #die rolls simulation
  die1 = sample(1:d, n, replace = TRUE)
  die2 = sample(1:d, n, replace = TRUE) 
  #total spaces player moves forward
  num_spaces = die1+die2 
  position_before_roll = num_spaces
  position_after_roll = num_spaces
  go_jail = num_spaces
  monopoly_data = data.frame(position_before_roll, die1,die2, num_spaces, position_after_roll, go_jail)
  #monopoly_data$go_jail = factor(monopoly_data$go_jail)
  monopoly_data[1,1] = 0
  #monopoly_data = position_before_after_turn(monopoly_data,n, 1)
  i = 2
  m = n
  for (i in 2:m){
    # See where player lands
    # while the spaces are between 0 and 39
    if (monopoly_data[i, 1] < 40){
      monopoly_data[i, 5]= monopoly_data[i,1] + monopoly_data[i,4]
      monopoly_data[i+1, 1] = monopoly_data[i,5]
    }
    else {
      # if the space is greater than 39 simulate going through the first space again  (start at 0 and go until 39 again)
      monopoly_data[i,1] = 0 + (monopoly_data[i-1,5]-39)
      monopoly_data[i, 5]= monopoly_data[i,1] + monopoly_data[i,4]
      monopoly_data[i+1, 1] = monopoly_data[i,5]
      
    }
    # triple double rolls go to jail
    if (monopoly_data[i-2, 2] == monopoly_data[i-2, 3] && monopoly_data[i-1, 2] == monopoly_data[i-1, 3]  &&
        monopoly_data[i, 2] == monopoly_data[i, 3] && monopoly_data[i+1, 2] != monopoly_data[i+1, 3]){
      monopoly_data[i, 6] = "jail"
      monopoly_data[i+1,1] = 10 
      monopoly_data[i+1, 5]= monopoly_data[i+1,1] + monopoly_data[i+1,4]
      monopoly_data[i+2, 1] = monopoly_data[i+1,5]
    }
    else {
      monopoly_data[i,6] = "no jail"
    }
    
    # Community Chest 
    if (monopoly_data[i,1] == 2 || monopoly_data[i,1] == 17 || monopoly_data[i,1] == 33){
      monopoly_data[i, 7] = sample(c("go","jail","stay"), size = 1,prob = c(1/16, 1/16, 14/16))
      # GO
      if (monopoly_data[i,7] == "go"){
        monopoly_data[i+1,1] = 0 
        monopoly_data[i+1, 5]= monopoly_data[i+1,1] + monopoly_data[i+1,4]
        monopoly_data[i+2, 1] = monopoly_data[i+1,5]
      }
      # Jail
      else if (monopoly_data[i,7] == "jail"){
        monopoly_data[i+1,1] = 10 
        monopoly_data[i+1, 5]= monopoly_data[i+1,1] + monopoly_data[i+1,4]
        monopoly_data[i+2, 1] = monopoly_data[i+1,5]
      }
      # Stay
      else if (monopoly_data[i,7] == "stay"){
        monopoly_data[i, 5]= monopoly_data[i,1] + monopoly_data[i,4]
        monopoly_data[i+1, 1] = monopoly_data[i,5]
      }
    }
 
    # Chance 
    if (monopoly_data[i,1] == 7 || monopoly_data[i,1] == 22 || monopoly_data[i,1] == 36){
      monopoly_data[i, 7] = sample(c("go","jail","C1","E3","H2","RR1","next_RR","UT","back_three","stay"), 
                                   size = 1,prob = c(1/16, 1/16, 1/16, 1/16, 1/16, 1/16, 2/16, 1/16, 1/16, 6/16))
      # Go
      if (monopoly_data[i,7] == "go"){
        monopoly_data[i+1,1] = 0 
        monopoly_data[i+1, 5]= monopoly_data[i+1,1] + monopoly_data[i+1,4]
        monopoly_data[i+2, 1] = monopoly_data[i+1,5]
      }
      # Jail
      else if (monopoly_data[i,7] == "jail"){
        monopoly_data[i+1,1] = 10 
        monopoly_data[i+1, 5]= monopoly_data[i+1,1] + monopoly_data[i+1,4]
        monopoly_data[i+2, 1] = monopoly_data[i+1,5]
      }
      # C1
      else if (monopoly_data[i,7] == "C1"){
        monopoly_data[i+1,1] = 11 
        monopoly_data[i+1, 5]= monopoly_data[i+1,1] + monopoly_data[i+1,4]
        monopoly_data[i+2, 1] = monopoly_data[i+1,5]
      }
      # E3
      else if (monopoly_data[i,7] == "E3"){
        monopoly_data[i+1,1] = 24 
        monopoly_data[i+1, 5]= monopoly_data[i+1,1] + monopoly_data[i+1,4]
        monopoly_data[i+2, 1] = monopoly_data[i+1,5]
      }
      # H2
      else if (monopoly_data[i,7] == "H2"){
        monopoly_data[i+1,1] = 39
        monopoly_data[i+1, 5]= monopoly_data[i+1,1] + monopoly_data[i+1,4]
        monopoly_data[i+2, 1] = monopoly_data[i+1,5]
      }
      # Railroad 1
      else if (monopoly_data[i,7] == "RR1"){
        monopoly_data[i+1,1] = 5
        monopoly_data[i+1, 5]= monopoly_data[i+1,1] + monopoly_data[i+1,4]
        monopoly_data[i+2, 1] = monopoly_data[i+1,5]
      }
      # Next railroad
      else if (monopoly_data[i,7] == "next_RR"){
        # If at CH1 go to RR2
        if (monopoly_data[i,1] == 7){
          monopoly_data[i+1,1] = 15
          monopoly_data[i+1, 5]= monopoly_data[i+1,1] + monopoly_data[i+1,4]
          monopoly_data[i+2, 1] = monopoly_data[i+1,5]
        }
        # If at CH2 go to RR3
        else if (monopoly_data[i,1] == 22){
          monopoly_data[i+1,1] = 25
          monopoly_data[i+1, 5]= monopoly_data[i+1,1] + monopoly_data[i+1,4]
          monopoly_data[i+2, 1] = monopoly_data[i+1,5]
        }
        # If at CH3 go to RR1
        else if (monopoly_data[i,1] == 36){
          monopoly_data[i+1,1] = 5
          monopoly_data[i+1, 5]= monopoly_data[i+1,1] + monopoly_data[i+1,4]
          monopoly_data[i+2, 1] = monopoly_data[i+1,5]
        }
      
      }
      # UT
      else if (monopoly_data[i,7] == "UT"){
        # If at CH1 or CH3 go to UT1
        if (monopoly_data[i,1] == 7 || monopoly_data[i,1] == 36){
          monopoly_data[i+1,1] = 12
          monopoly_data[i+1, 5]= monopoly_data[i+1,1] + monopoly_data[i+1,4]
          monopoly_data[i+2, 1] = monopoly_data[i+1,5]
        }
        # If at CH2 go to UT2
        if (monopoly_data[i,1] == 22){
          monopoly_data[i+1,1] = 28
          monopoly_data[i+1, 5]= monopoly_data[i+1,1] + monopoly_data[i+1,4]
          monopoly_data[i+2, 1] = monopoly_data[i+1,5]
        }
      }
      # back_three
      else if (monopoly_data[i,7] == "back_three"){
        monopoly_data[i+1,1] = monopoly_data[i,5] - 3
        monopoly_data[i+1, 5]= monopoly_data[i+1,1] + monopoly_data[i+1,4]
        monopoly_data[i+2, 1] = monopoly_data[i+1,5]
      }
      # Stay
      else if (monopoly_data[i,7] == "stay"){
        monopoly_data[i, 5]= monopoly_data[i,1] + monopoly_data[i,4]
        monopoly_data[i+1, 1] = monopoly_data[i,5]
      }
    }
    
   } #end of main for loop
  
  monopoly_data[,1]
}


# 2. Function that estimates the long-term probabilities of ending on a turn on
#     a specific Monopoly board space
#Functions to find probabilities of landing on spaces

# return all info of probabilities (data frame: Space, Freq, probability) 
estimate_monopoly = function(spaces){
  simulation_probs = data.frame(table(spaces))
  # find probabilities of landing on each space
  simulation_probs$probability = simulation_probs$Freq/sum(simulation_probs$Freq)
  simulation_probs
}
# return just probabilities (vector)
estimate_monopoly2 = function(spaces){
  simulation_probs = data.frame(table(spaces))
  simulation_probs$probability1 = simulation_probs$Freq/sum(simulation_probs$Freq)
  simulation_probs$probability1
}
# 3 most likely squares to end a turn on if play monopoly with 6 sided die
# probabilities for 3 side dice
die_3_side = simulate_monopoly(3,500)
die_3_prob = estimate_monopoly(die_3_side)
die_3_prob = select(die_3_prob, spaces, probability)
die_3_prob$die = rep("3-sided dice", nrow(die_3_prob))
# probabilities for 4 side dice
die_4_side = simulate_monopoly(4,500)
die_4_prob = estimate_monopoly(die_4_side)
# find most likely squares to land on
head(arrange(die_4_prob, desc(probability)))
die_4_prob = die_4_prob[-c(41),]
die_4_prob = select(die_4_prob, spaces, probability)
die_4_prob$die = rep("4-sided dice", nrow(die_4_prob))
# probabilities for 5 side dice
die_5_side = simulate_monopoly(5,500)
die_5_prob = estimate_monopoly(die_5_side)
die_5_prob = select(die_5_prob, spaces, probability)
die_5_prob$die = rep("5-sided dice", nrow(die_5_prob))
# probabilities for 6 side dice
die_6_side = simulate_monopoly(6,500)
die_6_prob = estimate_monopoly(die_6_side)
# find most likely squares to land on
head(arrange(die_6_prob, desc(probability)))
die_6_prob = select(die_6_prob, spaces, probability)
die_6_prob$die = rep("6-sided dice", nrow(die_6_prob))

compare_die_prob = rbind(die_3_prob,die_4_prob)
compare_die_prob = rbind(compare_die_prob,die_5_prob)
compare_die_prob = rbind(compare_die_prob,die_6_prob)

# Graph
# Help from R Cookbook
ggplot(data = compare_die_prob, aes(x = spaces, y = probability, fill = die)) + geom_bar(stat = "identity", position = position_dodge(), colour = "black") + ggtitle("Probabilities of Landing on Monopoly Board Spaces with Different Dice") + xlab("Spaces Player Could Land On (0 = GO space)") + ylab ("Probability of Landing on Space")
ggplot(data = compare_die_prob, aes(x = spaces, y = probability, fill = die)) + geom_bar(stat = "identity") + ggtitle("Probabilities of Landing on Monopoly Board Spaces with Different Dice") + xlab("Spaces Player Could Land On (0 = GO space)") + ylab ("Probability of Landing on Space")


# 3. Estimate standard error for long term probability of ending on the "jail" space
# Help from Nick Ulle
# Simulation estimation
# standard error: find sd of probabilities for each space
i = 1
k = 1000
n = 10000
# replicate simulation
sim_probs = replicate(k, simulate_monopoly(6,n)) 
# apply estimate_monopoly function to find probabilities for each sample found in replicated simulations
test = apply(sim_probs,2,estimate_monopoly2)
# find standard deviation of probabilities for the "jail" space
test_vec = c(rep(0,k))
i = 1
for (i in 1:k){ 
  test_vec[i] = test[[i]][11]
  i = i+1
}
sd(test_vec)
# Long-Term Probability of going to Jail = 0.002991792


# 4. Bootstrap estimation to estimate standard error for the long term probability of
#     landing on the "jail" space
# Help from Nick Ulle

b = 1000
n = 10000
# get one sample of spaces from simulation
sim = simulate_monopoly(6,n) 
# get more samples of the simulation just from the results of the first simulation
bootstrap_n = replicate(b,sample(sim, length(sim), replace = TRUE))
# use estimate_monopoly() to get the long-term probability for each bootstrap sample
test1 = apply(bootstrap_n, 2,estimate_monopoly2)
# find standard deviation of probabilities for "jail" space
test_vec1 = c(rep(0,b))
i = 1
for (i in 1:b){ 
  test_vec1[i] = test1[[i]][11]
  i = i+1
}
sd(test_vec1)
# Long-Term Probability of going to Jail = 0.01630243

# 5. Graphically displaying standard errors for long term probabilities of 3,4,5, and 6-sided dice
# take die_3_prob, die_4_prob, die_5_prob, die_6_prob to find the standard deviation of the different sided dice
# probabilities for each space
die_prob = data.frame(die_3_prob$probability, die_4_prob$probability, die_5_prob$probability, die_6_prob$probability)
stand_error = data.frame(c(0:39),apply(die_prob, 1, sd))
colnames(stand_error) = c("space", "standard_error")
stand_error$space = factor(stand_error$space)
# graph
ggplot(data = stand_error, aes(x = space, y = standard_error)) + geom_bar(stat = "identity", position = position_dodge(), colour = "black") + ggtitle("Standard Errors (Standard Deviations) of Long-Term Probabilities of Difference Dice" ) + xlab("Spaces Player Could Land On (0 = GO space)") + ylab ("Standard Error")


# Citations
#
## http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/
## http://www.cookbook-r.com/Graphs/Titles_(ggplot2)/
## http://www.cookbook-r.com/Graphs/Axes_(ggplot2)/#axis-labels-and-text-formatting
#
## Help from Nick Ulle
## Collaboration with Marc Toney
