knitr::opts_chunk$set(echo = TRUE)
library(tidyverse) #useful functions to make plots
library(dplyr)
library(popdemo) #matrix modeling library

#SURVIVAL RATES for each stage
sj= 0.0004 #egg to age1 survival
sa= 0.743 #young juvinile survival
se= 0.548 #egg survival
sc= 0.548 #chick survival

#BIRTH RATE VARIABLES
B= 1.27 #breeding frequency
E= 1.86 #eggs per clutch
P= 1 #breeding probability
f= 0.5 #proportion of females in population

#FERTILITY
b5= P*f*E*B*se*sc*sa #fecundity

#SIMULATION TIME
SimTime= 40 #how many steps, since our interval is 6 months, SimTime of 40 is 20 years

A= matrix(0,5,5) #creates an 5x5 matrix A (corresponding to 5 stages)

#SURVIVAL RATES INTO MATRIX A
A[2,1]= sj #defines 2nd row, 1st column value of A as sj
A[3,2]= sa
A[4,3]= sa
A[5,4]= sa
A[5,5]= sa

#BIRTH RATES INTO MATRIX A
A[1,5]= b5

Res= eigs(A)
#dominant eigenvalue is asymptotic growth rate, and eigenvector is stable state ratio
L= Res$lambda #defines L as lambda, the is dominant eigenvalue, part of the Res function
SSD= Res$ss #defines solid state distribution vector. Under the assumption Snow Leopards are in a solid state distribution, this will be our initial population distribution.

#CURRENT POPULATION
ntot= 17024 #total population of African Penguins (in the study)
nfem= ntot/2
Ptotfem = nfem/SSD[5] #total female population of all the stages combinedSS

n1= SSD[1]*Ptotfem
n2= SSD[2]*Ptotfem
n3= SSD[3]*Ptotfem
n4= SSD[4]*Ptotfem
n5= nfem


africanpenguinvect =c(n1,n2,n3,n4,n5) #creates an initial population vector for each stage n


#Iterates the population matrix based on our initial population
africanpenguin= project(A, africanpenguinvect, time=SimTime)
#(matrix, starting pop vector, how many time steps to simulate)
#africanpenguin[1:10] would display first 10 iterations of africanpenguin

growth_end = africanpenguin[SimTime]/africanpenguin[SimTime-1]
growth_beginning = africanpenguin[2]/africanpenguin[1]
#we have assumed asymptotic growth rate, therefore these values will be the same (0.96)

#CREATING A TABLE FROM OUR DATA
africanpenguin.pop= data.frame(Time=1:SimTime, N=africanpenguin[1:SimTime])
#first variable is time, second variable is africanpenguin population

ggplot(data=africanpenguin.pop, mapping=aes(x=Time, y=N)) + geom_point() + geom_line() + labs(x= "Time (years)", y= "Number of Individuals")

ggplot(data=africanpenguin.pop, mapping=aes(x=Time, y=log(N))) + geom_point() + geom_line() + labs(x= "Time (years)", y= "Log(Number of Individuals)")
#data=africanpenguin.pop \is telling it what data to plot
#mapping=aes tells it what to map to which variable this case x is Time and y is N
#geom_point() represents data as points
#geom_line() connects the points with lines
#labs(x="Time", y="Log(Number of Individuals)") creates labels for x and y axis

#SAVING PLOT
#ggsave(file="African_Penguin_Population_Projection_3_11.png") saves the file