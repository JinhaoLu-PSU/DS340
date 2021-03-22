#authored by Yang Yu; all rights reserved;
#yuyangjason@gmail.com,yzy43@psu.edu
#data:https://data.cdc.gov/NCHS/Indicators-of-Anxiety-or-Depression-Based-on-Repor/8pt5-q6wp
#import package
setwd("~/ds340w")
library("data.table")
library("ggplot2")
library("stringr")
library(lubridate)

#load data
data <- fread("data.csv")
#set Time zone & format
Sys.setlocale("LC_TIME", "English")

#rename and change to DATETIME format
setnames(data,"Time Period Start Date","Time")
data[,Time:=str_sub(Time,1,11) ]
data[,Time:=mdy(Time)]

# Time series plot of National Estimate 
national<-data[Group=="National Estimate"]
p <- ggplot(na.omit(national), aes(Time, Value))
p + geom_point(aes(colour = factor(Indicator)), size = 4)+
geom_line(aes(colour = factor(Indicator)))+
theme(axis.text.x = element_text(hjust = 1, vjust = .5))+
scale_x_date(date_labels = "%Y %b")+ ylab("Percentage")+ 
ggtitle("Symptoms of Anxiety Disorder or Depressive Disorder National Estimate")


#SADDD for Symptoms of Anxiety Disorder or Depressive Disorder
SADDD<-data[Indicator=="Symptoms of Anxiety Disorder or Depressive Disorder"]

#Time series plot of SADDD by AGE
SADDD_age<-SADDD[Group=="By Age"]
p <- ggplot(na.omit(SADDD_age),aes(Time, Value))
p + geom_point(aes(colour = factor(Subgroup)), size = 4)+
geom_line(aes(colour = factor(Subgroup)))+
theme(axis.text.x = element_text(hjust = 1, vjust = .5))+
scale_x_date(date_labels = "%Y %b")+ylab("Percentage")+ 
ggtitle("Symptoms of Anxiety Disorder or Depressive Disorder by Age")

#Time series plot of SADDD by Gender
SADDD_gender<-SADDD[Group=="By Gender"]
p <- ggplot(na.omit(SADDD_gender),aes(Time, Value))
p + geom_point(aes(colour = factor(Subgroup)), size = 4)+
geom_line(aes(colour = factor(Subgroup)))+
theme(axis.text.x = element_text(hjust = 1, vjust = .5))+
scale_x_date(date_labels = "%Y %b")+ylab("Percentage")+ 
ggtitle("Symptoms of Anxiety Disorder or Depressive Disorder by Gender")

#Time series plot of SADDD by Race
SADDD_race<-SADDD[Group=="By Race/Hispanic ethnicity"]
p <- ggplot(na.omit(SADDD_race),aes(Time, Value))
p + geom_point(aes(colour = factor(Subgroup)), size = 4)+
geom_line(aes(colour = factor(Subgroup)))+
theme(axis.text.x = element_text(hjust = 1, vjust = .5))+
scale_x_date(date_labels = "%Y %b")+ylab("Percentage")+ 
ggtitle("Symptoms of Anxiety Disorder or Depressive Disorder by Race")

#Time series plot of SADDD by Education
SADDD_edu<-SADDD[Group=="By Education"]
p <- ggplot(na.omit(SADDD_edu),aes(Time, Value))
p + geom_point(aes(colour = factor(Subgroup)), size = 4)+
geom_line(aes(colour = factor(Subgroup)))+
theme(axis.text.x = element_text(hjust = 1, vjust = .5))+
scale_x_date(date_labels = "%Y %b")+ylab("Percentage")+ 
ggtitle("Symptoms of Anxiety Disorder or Depressive Disorder by Education")

#by state? 50 states can't put in one graph.
#Time series plot of SADDD in Pennsylvania
SADDD_state<-SADDD[Group=="By State"]
SADDD_PA<-SADDD_state[State=="Pennsylvania"]
p <- ggplot(na.omit(SADDD_PA),aes(Time, Value))
p + geom_point(aes(colour = factor(Subgroup)), size = 4)+
geom_line(aes(colour = factor(Subgroup)))+
theme(axis.text.x = element_text(hjust = 1, vjust = .5))+
scale_x_date(date_labels = "%Y %b")+ylab("Percentage")+ 
ggtitle("Symptoms of Anxiety Disorder or Depressive Disorder In Pennsylvania")
