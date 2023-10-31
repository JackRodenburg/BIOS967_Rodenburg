#data import problems

errdat <- read.csv("data/SampleData_w_errors.csv")
errdat

#look at structure of data
str(errdat) #str() provides overview

#we found out that "size" is being interpreted as a character, but we want it to be a number

errdat$size #you can see they're in quotations, are therefore characters (bad)
#mean(errdat$size) #this gives a error

#try not to change anything about the raw data, even if there are errors.

#PROBLEM1: weight is not a number

errdat$weight #this is caused by #N/A

#solution: tell R that this is a missing value when importing the data

errdat <- read.csv("data/SampleData_w_errors.csv", na.strings="#N/A!")
#this tells R that "#N/A!" should be treated as NA (woah!!!!!)

errdat
errdat$weight

#PROBLEM2: size is not a number (and it should be)

#to deal with the same problem in size, also designate "**" as NA
errdat <- read.csv("data/SampleData_w_errors.csv", na.strings=c("#N/A!","**")) #solves both problems1 and 2 at once!
errdat
mean(errdat$size, na.rm=T) #removed NA from the mean calculation

#PROBLEM3: mystery column "X" with 1 and rest NA
#to remove column "X":

names(errdat) #returns all the column names in your data set

names(errdat)=="X" #returns False for every caloumn but TRUE for X column

which(names(errdat)=="X") #returns WHICH column is titled X

errdat[,which(names(errdat)=="X")] #returns me the values for the clolumn called X in dataframe errdat
#since we want ALL the rows, we have [,which...]
#if we wanted ALL the columns, [which...,]

dat <- errdat[,which(names(errdat)!="X")] #gives me all of the columns EXCEPT the one called X
dat

#let's remove any rows that are missing an individual ID (Indiv_ID)
dat$Indiv_ID #returns column of Indiv_ID

dat2 <- dat[which(dat$Indiv_ID!=""),] #gets rid of rows without Indiv_ID
dat2

#plotting basics:
plot(dat2$size, dat2$weight) #plots one continous variable against the other
plot(dat2$weight~dat2$size) #same plot in formula notation, ie y=x
plot(weight~size, data=dat2) #alternative way: use variable names in the formula, then data= to designate the dataframe they come from

boxplot(weight~sex, data=dat2) #creates boxplot!
