#authored by Yang Yu; all rights reserved;
#yuyangjason@gmail.com,yzy43@psu.edu
  
#authored by Jinhao Lu; all rights reserved;
#email: jxl469@psu.edu

#data:https://data.cdc.gov/NCHS/Indicators-of-Anxiety-or-Depression-Based-on-Repor/8pt5-q6wp
#https://www.cdc.gov/nchs/covid19/pulse/mental-health.htm
#import package
setwd("~/ds340w")
library("data.table")
library("ggplot2")
library("stringr")
library(lubridate)

#load data
data <- fread("Mental_Health.csv")#https://data.cdc.gov/NCHS/Indicators-of-Anxiety-or-Depression-Based-on-Repor/8pt5-q6wp
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

SADDD_state<-SADDD[Group=="By State"]
SADDD_CA<-SADDD_state[State=="California"]
p <- ggplot(na.omit(SADDD_CA),aes(Time, Value))
p + geom_point(aes(colour = factor(Subgroup)), size = 4)+
geom_line(aes(colour = factor(Subgroup)))+
theme(axis.text.x = element_text(hjust = 1, vjust = .5))+
scale_x_date(date_labels = "%Y %b")+ylab("Percentage")+ 
ggtitle("Symptoms of Anxiety Disorder or Depressive Disorder In California")

SADDD_NY<-SADDD_state[State=="New York"]
p <- ggplot(na.omit(SADDD_NY),aes(Time, Value))
p + geom_point(aes(colour = factor(Subgroup)), size = 4)+
geom_line(aes(colour = factor(Subgroup)))+
theme(axis.text.x = element_text(hjust = 1, vjust = .5))+
scale_x_date(date_labels = "%Y %b")+ylab("Percentage")+ 
ggtitle("Symptoms of Anxiety Disorder or Depressive Disorder In New York")

SADDD_FL<-SADDD_state[State=="Florida"]
p <- ggplot(na.omit(SADDD_FL),aes(Time, Value))
p + geom_point(aes(colour = factor(Subgroup)), size = 4)+
geom_line(aes(colour = factor(Subgroup)))+
theme(axis.text.x = element_text(hjust = 1, vjust = .5))+
scale_x_date(date_labels = "%Y %b")+ylab("Percentage")+ 
ggtitle("Symptoms of Anxiety Disorder or Depressive Disorder In Florida")

#useless-
#SADDD_state<-SADDD[Group=="By State"]
#setkey(SADDD_state,State)
#SADDD_CANY<-SADDD_state[c("California","Florida","New York")]
#p <- ggplot(na.omit(SADDD_CANY),aes(Time, Value))
#p + geom_point(aes(colour = factor(Subgroup)), size = 4)+
#geom_line(aes(colour = factor(Subgroup)))+
#theme(axis.text.x = element_text(hjust = 1, vjust = .5))+
#scale_x_date(date_labels = "%Y %b")+ylab("Percentage")+ 
#ggtitle("Symptoms of Anxiety Disorder or Depressive Disorder In CA,NY,FL")


################----------USE Num_response dataset----------#######################

sample_data<- fread("Num_Response.csv")#https://www.cdc.gov/nchs/covid19/pulse/mental-health.htm

#sample_data processing
sample_data$weighted_response_rate <- as.numeric(sub("%","",sample_data$weighted_response_rate))/100 #ratio column val into digit
sample_data$sample_size <- (sub(",","",sample_data$sample_size))#sub "," in val
sample_data$number_response <- sample_data$weighted_response_rate * as.numeric(sample_data$sample_size)
sample_data$year <- 2020 #add year variable to the table
sample_data$year[22:25] <- 2021

#graph of sample number
test1 <- factor(sample_data$week,levels = sample_data$week)#rank the week as the actual time
graph_numberResponse <- data.frame(test1,sample_data$number_response, sample_data$year)

#change the name of columns
setnames(graph_numberResponse,"test1","week")
setnames(graph_numberResponse,"sample_data.number_response","number_response")
setnames(graph_numberResponse,"sample_data.year","year")

#create the barplot 
p <- ggplot(data = graph_numberResponse, mapping = aes(x = number_response, y = week))
p + geom_col(mapping = aes(fill = year))+ geom_text(mapping = aes(label = number_response),colour = 'red', vjust = 0.1, hjust = - 0.1)
