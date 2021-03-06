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

#============================== covid 19 confirmed data ==============================
#setwd("~/Desktop/DS 340W/paper data")
#library("data.table")
#library("ggplot2")
#load data

confirmed_19 <- fread("time_series_covid_19_confirmed_US.csv")#https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset?select=covid_19_data.csv

#state column
state <- as.data.frame(confirmed_19$Province_State)

#confirmed number column
confirmed_data <- confirmed_19[,12:414]

#setnames(num_confirmed,"confirmed_19$Province_State","province")
test <- state
test$Jan_20 <- confirmed_data[,10]
test$Feb_20 <- confirmed_data[,39]
test$Mar_20 <- confirmed_data[,70]
test$Apr_20 <- confirmed_data[,100]
test$May_20 <- confirmed_data[,131]
test$Jun_20 <- confirmed_data[,161]
test$Jul_20 <- confirmed_data[,192]
test$Aug_20 <- confirmed_data[,223]
test$Sep_20 <- confirmed_data[,253]
test$Oct_20 <- confirmed_data[,284]
test$Nov_20 <- confirmed_data[,314]
test$Dec_20 <- confirmed_data[,345]
test$Jan_21 <- confirmed_data[,376]
test$Feb_21 <- confirmed_data[,403]

c <- test[,2:15]
#sum up the total number of each month
num_confirmed <- as.data.frame(t(apply(c,2,sum)))

#calculate the difference by months
diff <- num_confirmed[,1:(ncol(num_confirmed)-1)]
diff <- cbind(0,diff)
confirmed_num_diff <- num_confirmed - diff

#calculate the difference by days
day_diff <- confirmed_data[,1:(ncol(confirmed_data)-1)]
day_diff <- cbind(0,day_diff)
day_num_diff <- confirmed_data - day_diff

#sum up the total number of each day
confirmend_day_num_diff <- as.data.frame(t(apply(day_num_diff,2,sum)))
#============================== covid 19 deaths data ==============================
#load data
deaths_19 <- fread("time_series_covid_19_deaths_US.csv")#https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset?select=covid_19_data.csv

#state column
state <- as.data.frame(deaths_19$Province_State)

#confirmed number column
deaths_data <- deaths_19[,13:415]

test <- state
test$Jan_20 <- deaths_data[,10]
test$Feb_20 <- deaths_data[,39]
test$Mar_20 <- deaths_data[,70]
test$Apr_20 <- deaths_data[,100]
test$May_20 <- deaths_data[,131]
test$Jun_20 <- deaths_data[,161]
test$Jul_20 <- deaths_data[,192]
test$Aug_20 <- deaths_data[,223]
test$Sep_20 <- deaths_data[,253]
test$Oct_20 <- deaths_data[,284]
test$Nov_20 <- deaths_data[,314]
test$Dec_20 <- deaths_data[,345]
test$Jan_21 <- deaths_data[,376]
test$Feb_21 <- deaths_data[,403]

c <- test[,2:15]
#sum up the total number of each month
num_deaths <- as.data.frame(t(apply(c,2,sum)))

#calculate the difference between by months
diff <- num_deaths[,1:(ncol(num_deaths)-1)]
diff <- cbind(0,diff)
death_num_diff <- num_deaths - diff

#calculate the difference by days
day_diff <- deaths_data[,1:(ncol(deaths_data)-1)]
day_diff <- cbind(0,day_diff)
day_num_diff <- deaths_data - day_diff

#sum up the total number of each day
deaths_day_num_diff <- as.data.frame(t(apply(day_num_diff,2,sum)))

#combine the columns
state <- t(list('confirmed_num','deaths_num'))
state <- as.data.frame(state)
confirmed_num_diff <- cbind(state$V1, confirmed_num_diff)
death_num_diff <- cbind(state$V2, death_num_diff)

#change column name
setnames(confirmed_num_diff,'"confirmed_num"',"National_Estimate")
setnames(death_num_diff,'"deaths_num"',"National_Estimate")

#National_Estimate <- rbind(confirmed_num_diff, death_num_diff)
#melt table in to tidy table form
confirmed_num_diff <- as.data.table(confirmed_num_diff)
confirmed_num_diff<-melt(confirmed_num_diff,id.vars= c("National_Estimate"),variable.name="Month",value.name="number")

#melt table in to tidy table form
death_num_diff <- as.data.table(death_num_diff)
death_num_diff<-melt(death_num_diff,id.vars= c("National_Estimate"),variable.name="Month",value.name="number")

#plot the line graph
ggplot(data = confirmed_num_diff, mapping = aes(x = confirmed_num_diff$Month, y = confirmed_num_diff$number, colour , group = 1)) + geom_line()

ggplot(data = death_num_diff, mapping = aes(x = death_num_diff$Month, y = death_num_diff$number, colour , group = 1)) + geom_line()


#=================================infection rate by states===================================
#setwd("~/Desktop/DS 340W/paper data")
#library("data.table")
#library("ggplot2")
#load data
#confirmed_19 <- fread("covid_19_confirmed.csv")#https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset?select=covid_19_data.csv
#deaths_19 <- fread("covid_19_deaths.csv")#https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset?select=covid_19_data.csv

#infection rate of New York
NewYork <- confirmed_19[Province_State == 'New York']
NewYork_confirmed <- as.data.frame(NewYork$Province_State)
NewYork <- NewYork[,12:414]

#NewYork_confirmed number by month 
NewYork_confirmed$Jan_20 <- NewYork[,10]
NewYork_confirmed$Feb_20 <- NewYork[,39]
NewYork_confirmed$Mar_20 <- NewYork[,70]
NewYork_confirmed$Apr_20 <- NewYork[,100]
NewYork_confirmed$May_20 <- NewYork[,131]
NewYork_confirmed$Jun_20 <- NewYork[,161]
NewYork_confirmed$Jul_20 <- NewYork[,192]
NewYork_confirmed$Aug_20 <- NewYork[,223]
NewYork_confirmed$Sep_20 <- NewYork[,253]
NewYork_confirmed$Oct_20 <- NewYork[,284]
NewYork_confirmed$Nov_20 <- NewYork[,314]
NewYork_confirmed$Dec_20 <- NewYork[,345]
NewYork_confirmed$Jan_21 <- NewYork[,376]
NewYork_confirmed$Feb_21 <- NewYork[,403]

c <- NewYork_confirmed[,2:15]
NewYork_confirmed <- as.data.frame(t(apply(c,2,sum)))
#Population estimates of New york 2019 = 19453561 https://www.census.gov/quickfacts/NY
pop <- 19453561
#infection rate
NewYork_infection_rate <- NewYork_confirmed/pop


#California
California <- confirmed_19[Province_State == 'California']
California_confirmed <- as.data.frame(California$Province_State)
California <- California[,12:414]

#California_confirmed number by month 
California_confirmed$Jan_20 <- California[,10]
California_confirmed$Feb_20 <- California[,39]
California_confirmed$Mar_20 <- California[,70]
California_confirmed$Apr_20 <- California[,100]
California_confirmed$May_20 <- California[,131]
California_confirmed$Jun_20 <- California[,161]
California_confirmed$Jul_20 <- California[,192]
California_confirmed$Aug_20 <- California[,223]
California_confirmed$Sep_20 <- California[,253]
California_confirmed$Oct_20 <- California[,284]
California_confirmed$Nov_20 <- California[,314]
California_confirmed$Dec_20 <- California[,345]
California_confirmed$Jan_21 <- California[,376]
California_confirmed$Feb_21 <- California[,403]

c <- California_confirmed[,2:15]
California_confirmed <- as.data.frame(t(apply(c,2,sum)))
#Population estimates of California = 39512223 https://www.census.gov/quickfacts/fact/table/CA/PST045219
pop <- 39512223
#infection rate
California_infection_rate <- California_confirmed/pop

#Florida
Florida <- confirmed_19[Province_State == 'Florida']
Florida_confirmed <- as.data.frame(Florida$Province_State)
Florida <- Florida[,12:414]

#Florida_confirmed number by month 
Florida_confirmed$Jan_20 <- Florida[,10]
Florida_confirmed$Feb_20 <- Florida[,39]
Florida_confirmed$Mar_20 <- Florida[,70]
Florida_confirmed$Apr_20 <- Florida[,100]
Florida_confirmed$May_20 <- Florida[,131]
Florida_confirmed$Jun_20 <- Florida[,161]
Florida_confirmed$Jul_20 <- Florida[,192]
Florida_confirmed$Aug_20 <- Florida[,223]
Florida_confirmed$Sep_20 <- Florida[,253]
Florida_confirmed$Oct_20 <- Florida[,284]
Florida_confirmed$Nov_20 <- Florida[,314]
Florida_confirmed$Dec_20 <- Florida[,345]
Florida_confirmed$Jan_21 <- Florida[,376]
Florida_confirmed$Feb_21 <- Florida[,403]

c <- Florida_confirmed[,2:15]
Florida_confirmed <- as.data.frame(t(apply(c,2,sum)))
#Population estimates of Florida = 21477737  https://www.census.gov/quickfacts/FL
pop <- 21477737
#infection rate
Florida_infection_rate <- Florida_confirmed/pop

#=================================deaths rate by states===================================
#deaths rate of New York
NewYork <- deaths_19[Province_State == 'New York']
NewYork_deaths <- as.data.frame(NewYork$Province_State)
pop <- sum(NewYork$Population)# 19453561
NewYork <- NewYork[,13:415]

#NewYork_deaths number by month 
NewYork_deaths$Jan_20 <- NewYork[,10]
NewYork_deaths$Feb_20 <- NewYork[,39]
NewYork_deaths$Mar_20 <- NewYork[,70]
NewYork_deaths$Apr_20 <- NewYork[,100]
NewYork_deaths$May_20 <- NewYork[,131]
NewYork_deaths$Jun_20 <- NewYork[,161]
NewYork_deaths$Jul_20 <- NewYork[,192]
NewYork_deaths$Aug_20 <- NewYork[,223]
NewYork_deaths$Sep_20 <- NewYork[,253]
NewYork_deaths$Oct_20 <- NewYork[,284]
NewYork_deaths$Nov_20 <- NewYork[,314]
NewYork_deaths$Dec_20 <- NewYork[,345]
NewYork_deaths$Jan_21 <- NewYork[,376]
NewYork_deaths$Feb_21 <- NewYork[,403]

c <- NewYork_deaths[,2:15]
NewYork_deaths <- as.data.frame(t(apply(c,2,sum)))
#Population estimates of New york 2019 = 19453561 https://www.census.gov/quickfacts/NY
#infection rate
NewYork_deaths_rate <- NewYork_deaths/pop

#California
California <- deaths_19[Province_State == 'California']
California_deaths <- as.data.frame(California$Province_State)
pop <- sum(California$Population) #39512223
California <- California[,13:415]

#California_deaths number by month 
California_deaths$Jan_20 <- California[,10]
California_deaths$Feb_20 <- California[,39]
California_deaths$Mar_20 <- California[,70]
California_deaths$Apr_20 <- California[,100]
California_deaths$May_20 <- California[,131]
California_deaths$Jun_20 <- California[,161]
California_deaths$Jul_20 <- California[,192]
California_deaths$Aug_20 <- California[,223]
California_deaths$Sep_20 <- California[,253]
California_deaths$Oct_20 <- California[,284]
California_deaths$Nov_20 <- California[,314]
California_deaths$Dec_20 <- California[,345]
California_deaths$Jan_21 <- California[,376]
California_deaths$Feb_21 <- California[,403]

c <- California_deaths[,2:15]
California_deaths <- as.data.frame(t(apply(c,2,sum)))
#Population estimates of California = 39512223 https://www.census.gov/quickfacts/fact/table/CA/PST045219
#infection rate
California_deaths_rate <- California_deaths/pop

#Florida
Florida <- deaths_19[Province_State == 'Florida']
Florida_deaths <- as.data.frame(Florida$Province_State)
pop <- sum(Florida$Population) #21477737
Florida <- Florida[,13:415]

#Florida_deaths number by month 
Florida_deaths$Jan_20 <- Florida[,10]
Florida_deaths$Feb_20 <- Florida[,39]
Florida_deaths$Mar_20 <- Florida[,70]
Florida_deaths$Apr_20 <- Florida[,100]
Florida_deaths$May_20 <- Florida[,131]
Florida_deaths$Jun_20 <- Florida[,161]
Florida_deaths$Jul_20 <- Florida[,192]
Florida_deaths$Aug_20 <- Florida[,223]
Florida_deaths$Sep_20 <- Florida[,253]
Florida_deaths$Oct_20 <- Florida[,284]
Florida_deaths$Nov_20 <- Florida[,314]
Florida_deaths$Dec_20 <- Florida[,345]
Florida_deaths$Jan_21 <- Florida[,376]
Florida_deaths$Feb_21 <- Florida[,403]

c <- Florida_deaths[,2:15]
Florida_deaths <- as.data.frame(t(apply(c,2,sum)))
#Population estimates of Florida = 21477737  https://www.census.gov/quickfacts/FL
#infection rate
Florida_deaths_rate <- Florida_deaths/pop

state <- t(list('New York','California','Florida'))
state <- as.data.frame(state)

#combine column 
NewYork_infection_rate <- cbind(state$V1, NewYork_infection_rate)
California_infection_rate <- cbind(state$V2, California_infection_rate)
Florida_infection_rate <- cbind(state$V3, Florida_infection_rate)

NewYork_deaths_rate <- cbind(state$V1, NewYork_deaths_rate)
California_deaths_rate <- cbind(state$V2, California_deaths_rate)
Florida_deaths_rate <- cbind(state$V3, Florida_deaths_rate)

#change column name
setnames(NewYork_infection_rate,'"New York"',"state")
setnames(California_infection_rate,'"California"',"state")
setnames(Florida_infection_rate,'"Florida"',"state")

setnames(NewYork_deaths_rate,'"New York"',"state")
setnames(California_deaths_rate,'"California"',"state")
setnames(Florida_deaths_rate,'"Florida"',"state")

#combine the row
infection_rate <- rbind(NewYork_infection_rate,California_infection_rate,Florida_infection_rate)
deaths_rate <- rbind(NewYork_deaths_rate, California_deaths_rate, Florida_deaths_rate)

infection_rate <- as.data.table(infection_rate)
deaths_rate <- as.data.table(deaths_rate)

#melt table in to tidy table form
infection_rate<-melt(infection_rate,id.vars= "state",variable.name="Month",value.name="rate")
deaths_rate<-melt(deaths_rate,id.vars= "state",variable.name="Month",value.name="rate")

#plot the line graph
ggplot(data = infection_rate, mapping = aes(x = infection_rate$Month, y = infection_rate$rate, colour = infection_rate$state, group = infection_rate$state)) + geom_line()

ggplot(data = deaths_rate, mapping = aes(x = deaths_rate$Month, y = deaths_rate$rate, colour = deaths_rate$state, group = deaths_rate$state)) + geom_line()
