#authored by Jinhao Lu; all rights reserved;
#email: jxl469@psu.edu
setwd("~/DS340W")

#load library
library("data.table")
library("ggplot2")

data <- fread("Mental_Health.csv")#https://data.cdc.gov/NCHS/Indicators-of-Anxiety-or-Depression-Based-on-Repor/8pt5-q6wp
sample_data<- fread("Num_Response.csv")#https://www.cdc.gov/nchs/covid19/pulse/mental-health.htm
#https://www.cdc.gov/mmwr/volumes/68/wr/mm685152a1.htm#References

#sample_data processing
sample_data$weighted_response_rate <- as.numeric(sub("%","",sample_data$weighted_response_rate))/100
sample_data$sample_size <- (sub(",","",sample_data$sample_size))
sample_data$number_response <- sample_data$weighted_response_rate * as.numeric(sample_data$sample_size)
sample_data$year <- 2020 
sample_data$year[22:25] <- 2021

#graph of sample number
test1 <- factor(sample_data$week,levels = sample_data$week)
graph_numberResponse <- data.frame(test1,sample_data$number_response, sample_data$year)# <- sample_data[,.(week,number_response)]

setnames(graph_numberResponse,"test1","week")
setnames(graph_numberResponse,"sample_data.number_response","number_response")
setnames(graph_numberResponse,"sample_data.year","year")

p <- ggplot(data = graph_numberResponse, mapping = aes(x = number_response, y = week))
p + geom_col(mapping = aes(fill = year))+ geom_text(mapping = aes(label = number_response),colour = 'red', vjust = 0.1, hjust = - 0.1)

#National Estimate
national<- data[Group == 'National Estimate']

#============================== covid 19 confirmed data ==============================
#load data
confirmed_19 <- fread("covid_19_confirmed.csv")

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

a <- test[,2:15]
num_confirmed <- as.data.frame(t(apply(a,2,sum)))

#calculate the difference by months
diff_1<-num_confirmed[,1:(ncol(num_confirmed)-1)]
diff_1<-cbind(0,diff_1)
num_diff<- num_confirmed - diff_1

#calculate the difference by days
day_diff_1<-confirmed_data[,1:(ncol(confirmed_data)-1)]
day_diff_1<-cbind(0,day_diff_1)
day_num_diff<- confirmed_data - day_diff_1

day_num_diff <- as.data.frame(t(apply(day_num_diff,2,sum)))
#============================== covid 19 deaths data ==============================
#load data
deaths_19 <- fread("covid_19_deaths.csv")

#state column
state_2 <- as.data.frame(deaths_19$Province_State)

#confirmed number column
deaths_data <- deaths_19[,13:415]

test_2 <- state_2
test_2$Jan_20 <- deaths_data[,10]
test_2$Feb_20 <- deaths_data[,39]
test_2$Mar_20 <- deaths_data[,70]
test_2$Apr_20 <- deaths_data[,100]
test_2$May_20 <- deaths_data[,131]
test_2$Jun_20 <- deaths_data[,161]
test_2$Jul_20 <- deaths_data[,192]
test_2$Aug_20 <- deaths_data[,223]
test_2$Sep_20 <- deaths_data[,253]
test_2$Oct_20 <- deaths_data[,284]
test_2$Nov_20 <- deaths_data[,314]
test_2$Dec_20 <- deaths_data[,345]
test_2$Jan_21 <- deaths_data[,376]
test_2$Feb_21 <- deaths_data[,403]

b <- test_2[,2:15]
num_deaths <- as.data.frame(t(apply(b,2,sum)))

#calculate the difference between by months
diff_2<-num_deaths[,1:(ncol(num_deaths)-1)]
diff_2<-cbind(0,diff_2)
death_num_diff<- num_deaths - diff_2

#calculate the difference by days
day_diff_2<-deaths_data[,1:(ncol(deaths_data)-1)]
day_diff_2<-cbind(0,day_diff_2)
day_num_diff_2<- deaths_data - day_diff_2

day_num_diff_2 <- as.data.frame(t(apply(day_num_diff_2,2,sum)))

