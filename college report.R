# R Code for College Score Card Data Report

collegescore <- readRDS("/Users/quinncabral/Desktop/data/college_scorecard_2013.rds")

#Number of observations and colleges
#3312 observations, 2431 colleges
nrow(collegescore)
sum(collegescore$main_campus)
table(collegescore$main_campus)
#unique colleges
col_names <-collegescore$name
is.factor(col_names)
length(col_names)
col_names<-as.factor(col_names)
col_names[1]
levels(col_names)
col_names1 <- levels(col_names)
length(col_names1)

col_nam_table <- table(col_names)
col_nam_table[which(col_nam_table>3)]

#Number of features and feature type
#51 features = variables = columns
ncol(collegescore)
str(collegescore)
var_class<-sapply(collegescore, class)
table(var_class)
sum(var_class == "factor")
var_class[var_class == "factor"]
names(var_class[var_class=="factor"])
names(var_class[var_class=="character"])
names(var_class[var_class=="integer"])
names(var_class[var_class=="logical"])
names(var_class[var_class=="numeric"])

#Number of NA's, feature w/ most NA's, analysis for patterns
sum(is.na(collegescore))
#missing values: 23197
#avg_sat, 14 NA
which.max(colSums(is.na(collegescore)))
colSums(is.na(collegescore))
#histogram
hist(colSums(is.na(collegescore)), main = "Histogram of Number of Missing Values from College Features", 
     xlab = "Number of Missing Values", ylab = "Number of Features") 

#Public vs. Private colleges, proportions of highest degrees
#public = 716
#nonprofit = 1710
#for profit = 886
#more private than public
table(collegescore$ownership)
proportions = prop.table(table(collegescore$ownership))
proportions
degree_dat<- collegescore[c( "highest_degree", "ownership")]
degree_dat_tab <-table(degree_dat)
degree_dat_tab
highest_degree_prop <- as.matrix(prop.table(table(degree_dat)))
highest_degree_prop
#mosaic plot
mosaicplot(highest_degree_prop,color=TRUE, main = "Proportions of College Ownerships and Highest Degrees Offered", 
           xlab = "Highest Degree", ylab = "Ownership", las = 2)

#Average, median, and deciles of undergraduate population
avg = mean(collegescore$undergrad_pop, na.rm = TRUE)
#3599.502 avg. undergraduate population
med = median(collegescore$undergrad_pop, na.rm = TRUE)
#1295
decile = quantile(collegescore$undergrad_pop, prob = seq(0,1,length = 11), na.rm = TRUE)
decile 
#Boxplots
boxplot(collegescore$undergrad_pop, na.rm = TRUE, main = "Undergraduate Populations with Outliers", 
        ylab = "Undergraduate Student Number")
boxplot(collegescore$undergrad_pop, na.rm = TRUE, outline = FALSE, main = "Undergraduate Populations without Outliers",
        ylab = "Undergraduate Student Number")

#Tuition comparison between 5 most populous states
#http://www.worldatlas.com/articles/us-states-by-population.html to find top 5 populous states
tuition <- data.frame(collegescore$name,collegescore$state, collegescore$tuition)
summary(collegescore$tuition)
ca_tuition <- collegescore[collegescore$state == "CA", "tuition"]
tx_tuition <- collegescore[collegescore$state == "TX", "tuition"]
fl_tuition <- collegescore[collegescore$state == "FL", "tuition"]
ny_tuition <- collegescore[collegescore$state == "NY", "tuition"]
il_tuition <- collegescore[collegescore$state == "IL", "tuition"]
#boxplot
boxplot(ca_tuition, tx_tuition, fl_tuition, ny_tuition, il_tuition, main = "Tuition in Top 5 Most Populous States", 
        names = c("CA", "TX", "FL", "NY", "IL"), ylab = "In-State Tuition ($)")

#Spending per student and student 10-year earnings
public_comp <-data.frame(collegescore[collegescore$ownership == "Public",])

nonprofit_comp <-data.frame(collegescore[collegescore$ownership == "Nonprofit",])
forprofit_comp <-data.frame(collegescore[collegescore$ownership == "For Profit",])
par(mar = c(5,4,4,2) + 4)
#boxplot of relationship between public nonprofit and for profit school tuition
boxplot(public_comp$spend_per_student,public_comp$avg_10yr_salary, nonprofit_comp$spend_per_student, 
        nonprofit_comp$avg_10yr_salary, forprofit_comp$spend_per_student, forprofit_comp$avg_10yr_salary, main = "College Student Spending vs Average 10 Year Salary w/ Outliers",
        las = 2, names = c("Public Spending", "Public Salary", "Nonprofit Spending", "Nonprofit Salary",
                           "For Profit Spending", "For Profit Salary"), ylab = "Amount of Money ($)")
par(mar = c(5,4,4,2) + 4)
boxplot(public_comp$spend_per_student,public_comp$avg_10yr_salary, nonprofit_comp$spend_per_student, 
        nonprofit_comp$avg_10yr_salary, forprofit_comp$spend_per_student, forprofit_comp$avg_10yr_salary, main = "College Student Spending vs Average 10 Year Salary w/out Outliers",
        las = 2, names = c("Public Spending", "Public Salary", "Nonprofit Spending", "Nonprofit Salary",
                           "For Profit Spending", "For Profit Salary"), ylab = "Amount of Money ($)", outline = FALSE)
#Best earnings for cost colleges
# avg. 10 year salary - (net cost for the school + median debt coming out of school)
student_debt <- data.frame(collegescore$name, (collegescore$avg_10yr_salary - (collegescore$med_debt + collegescore$net_cost)))
View(student_debt)

#Most racially diverse college
asian <- collegescore$race_asian * collegescore$undergrad_pop
black <- collegescore$race_black * collegescore$undergrad_pop
white <- collegescore$race_white * collegescore$undergrad_pop
hispanic <- collegescore$race_hispanic * collegescore$undergrad_pop
native <- collegescore$race_native * collegescore$undergrad_pop
pacific <- collegescore$race_pacific * collegescore$undergrad_pop
other <- collegescore$race_other * collegescore$undergrad_pop
race_data <- data.frame(cbind(collegescore$name, asian, black, hispanic, white, native, pacific, other))
#use Simpson's Diversity Index
#http://www.statisticshowto.com/simpsons-diversity-index/
# range from 0-1, 1 for more diverse, 0 for less diverse
# n = # of individuals of each species
# N = total number of individuals of all species
# D = 1 - sum(n(n-1)/N(N-1))^2
race_data1 <- data.frame(cbind( asian, black, hispanic, white, native, pacific, other))
each_race <- rowSums(race_data1*(race_data1 -1), na.rm = TRUE)
total_race <- collegescore$undergrad_pop
total_race_eq <- total_race * (total_race -1)
race_equation <- 1- (each_race/total_race_eq)
race_data <- data.frame(collegescore$name,collegescore$state,collegescore$city,race_equation, asian, black, hispanic, white, native, pacific, other)

#Comparing UC Davis to other national colleges
#in state vs out of state tuition at colleges compared to UC Davis
#histogram
hist(collegescore$tuition_nonresident-collegescore$tuition, main = "Difference Between In-State and Out of State Tuition",
     xlab = "Out of State Tuition - In State Tuition ($)", ylab = "Frequency of Colleges")
zero_tuition_diff <- (collegescore$tuition_nonresident - collegescore$tuition) == 0
more_than_zero <- (collegescore$tuition_nonresident - collegescore$tuition) > 0
sum(zero_tuition_diff, na.rm = TRUE) # how to take off the zeros
sum(more_than_zero, na.rm = TRUE) # how to take off the zeros
sum(zero_tuition_diff, na.rm = TRUE) + sum(more_than_zero, na.rm = TRUE)#total colleges displayed
collegescore[collegescore$name == "University of California-Davis", "tuition_nonresident"]-collegescore[collegescore$name == "University of California-Davis", "tuition"]
# Davis = 22,878
#higher than most, but that is including all the schools with no difference in tuitions, amongst those who have a difference, Davis's difference is not as high

# % of applicants admitted at other colleges compared to UC Davis
#histogram
hist(collegescore$admission, main = "Histogram for Percentage of Applicants Admitted", xlab = "Percentage of applicants admitted to college")
# Davis = 0.4521
collegescore[collegescore$name == "University of California-Davis","admission"]

# Retention Rates colleges compared to UC Davis
#histogram
hist(collegescore$retention, main = "Retention Rates of National Colleges", xlab = "% of Students Who Stay 1+ Years")
collegescore[collegescore$name == "University of California-Davis","retention"]
# Davis = 0.9294


