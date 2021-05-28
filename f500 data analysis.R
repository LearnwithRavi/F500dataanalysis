library(tidyr)
library(dplyr)
library(ggplot2)
f500 <- read.csv(file.choose(),header = T,sep = ",") 
head(f500)
str(f500) 

# ID 
f500$ID <- as.factor(f500$ID)

# seprate() ####
f500 <- separate(f500,HomeTown,c("State","City"),sep = ",")

# lets clean the revenue and expense col

f500$Expenses <- gsub("Dollars","",f500$Expenses)
f500$Expenses <- gsub(",","",f500$Expenses)
f500$Expenses <- as.numeric(f500$Expenses)

# lets clean the revenue
f500$Revenue <- gsub("\\$","",f500$Revenue)
f500$Revenue <- gsub(",","",f500$Revenue)
f500$Revenue <- as.numeric(f500$Revenue)

# lets clean growth col
f500$Growth <- gsub("\\%","",f500$Growth)
f500$Growth <- as.numeric(f500$Growth)
f500$Growth <- f500$Growth/100


# which()
which(!complete.cases(f500))

# lets convert blanks cells into NA in state col
f500[f500$State=="",]
f500$State[f500$State==""] <- NA


# apply fun
apply(f500, 2, function(x) sum(is.na(x)))

sumfun <- function(x){
  return(sum(is.na(x)))
}
apply(f500, 2, sumfun)

# Missing Values ####
# lets Deal with the missing values

apply(f500, 2, function(x) sum(is.na(x)))

# THE output of the apply fun usally change i mean it changes to other than
# data frame like matrix 
# see the rownames of the navaluesdata - not in order
# to make them in order 
navaluesdata <- f500[!complete.cases(f500),]

rownames(navaluesdata) <- NULL # its not there but this cmd works

# now ahead we have the imputation of the missing values

f500[is.na(f500$State),]
# we can impute NY as a state where city is new york
# so lets do it
# we will try n pull the location of these NA

f500[is.na(f500$State) & f500$City=="New York","State"] # the location of these NA

# lets impute NY there
f500[is.na(f500$State) & f500$City=="New York","State"] <- "NY" 

# lets do the same for san francisco
f500[is.na(f500$State),]
# location
f500[is.na(f500$State) & f500$City=="San Francisco","State"] <- "CA"

sum(is.na(f500$State)) # state done

# list of blanks in employees
f500[is.na(f500$Employees),]
# as it is numeric it wil be prefered to use mean or median for imputation
# and the base can be industry
# summary stats

summary(f500[f500$Industry=="Retail" & f500$Employees,"Employees"])

# lets check the mean and median of employees
mean(f500$Employees[f500$Industry=="Retail"],na.rm = T) # mean basis Retail

retail_emp_median <- median(f500$Employees[f500$Industry=="Retail"],na.rm = T) 
# by the output we can see we have to use or we should use median
# for the imputation for no. of employees
# mean value is too high
f500[f500$Industry=="Retail" & is.na(f500$Employees),"Employees"] <- retail_emp_median
f500[3,]
# financial services
summary(f500[f500$Industry=="Financial Services" & f500$Employees,"Employees"])

# so find median
finServ_emp_median <- median(f500$Employees[f500$Industry=="Financial Services"],na.rm = T)

f500[f500$Industry=="Financial Services" & is.na(f500$Employees),"Employees"] <- finServ_emp_median
f500[332,]


# done
# lets try with subset
subset(f500,subset = is.na(f500$Employees)&f500$Industry=="Financial Services")
# this way

f500[!complete.cases(f500),]

# growth NA value

summary(f500[f500$Industry=="Construction" & f500$Growth,"Growth"])

growth_cons_median <- median(f500$Growth[f500$Industry=="Construction"],na.rm = T)

f500[f500$Industry=="Construction" & is.na(f500$Growth),"Growth"] <- growth_cons_median
f500[8,]

# Revenue
f500[is.na(f500$Revenue),]

summary(f500[f500$Industry=="Construction" & f500$Revenue,"Revenue"])

rev_cons_median <- median(f500$Revenue[f500$Industry=="Construction"],na.rm = T)

f500[f500$Industry=="Construction" & is.na(f500$Revenue),"Revenue"] <- rev_cons_median

f500[c(8,44),]

# Expenses
f500[is.na(f500$Expenses),]

summary(f500[f500$Industry=="Construction" & f500$Expenses,"Expenses"])

expn_cons_median <- median(f500$Expenses[f500$Industry=="Construction"],na.rm = T)

f500[f500$Industry=="Construction" & is.na(f500$Expenses),"Expenses"] <- expn_cons_median

# Derived values : Factual Analysis
# Revenue - Expenses = Profit
# Revenue - Profit = Expenses

f500[is.na(f500$Profit),"Profit"]

f500[f500$Industry=="Construction" & is.na(f500$Profit),"Revenue"]-
  f500[f500$Industry=="Construction" & is.na(f500$Profit),"Expenses"]

f500[is.na(f500$Profit),"Profit"] <- f500[f500$Industry=="Construction" & is.na(f500$Profit),"Revenue"]-
  f500[f500$Industry=="Construction" & is.na(f500$Profit),"Expenses"]

# for expenses
f500[is.na(f500$Expenses),"Expenses"]

f500[f500$Industry=="IT Services" & is.na(f500$Expenses),"Revenue"]-
  f500[f500$Industry=="IT Services" & is.na(f500$Expenses),"Profit"]

f500[is.na(f500$Expenses),"Expenses"] <- f500[f500$Industry=="IT Services" & is.na(f500$Expenses),"Revenue"]-
  f500[f500$Industry=="IT Services" & is.na(f500$Expenses),"Profit"]

# Visualisation ####
# scatterplot classified by industry showing revenue, expenses and profit
f500 %>% 
  ggplot(aes(Revenue,Expenses,color=Industry))+
  geom_point(aes(fill=Profit))

# boxplot

f500 %>% 
  ggplot(aes(Industry,Growth,color=Industry))+
  geom_boxplot(size=1,outlier.colour = NA)+geom_jitter()




