setwd("~/Desktop/DS 340W/paper data")

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



#============================== covid 19 confirmed data ==============================
#load data
confirmed_19 <- fread("covid_19_confirmed.csv")#https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset?select=covid_19_data.csv

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
num_confirmed <- as.data.frame(t(apply(c,2,sum)))

#calculate the difference by months
diff <- num_confirmed[,1:(ncol(num_confirmed)-1)]
diff <- cbind(0,diff)
confirmed_num_diff <- num_confirmed - diff

#calculate the difference by days
day_diff <- confirmed_data[,1:(ncol(confirmed_data)-1)]
day_diff <- cbind(0,day_diff)
day_num_diff <- confirmed_data - day_diff

confirmend_day_num_diff <- as.data.frame(t(apply(day_num_diff,2,sum)))
#============================== covid 19 deaths data ==============================
#load data
deaths_19 <- fread("covid_19_deaths.csv")#https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset?select=covid_19_data.csv

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
num_deaths <- as.data.frame(t(apply(c,2,sum)))

#calculate the difference between by months
diff <- num_deaths[,1:(ncol(num_deaths)-1)]
diff <- cbind(0,diff)
death_num_diff <- num_deaths - diff

#calculate the difference by days
day_diff <- deaths_data[,1:(ncol(deaths_data)-1)]
day_diff <- cbind(0,day_diff)
day_num_diff <- deaths_data - day_diff

deaths_day_num_diff <- as.data.frame(t(apply(day_num_diff,2,sum)))

#=================================infection rate by states===================================
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
NewYork_infection_rate <- cbind(state$V1, NewYork_infection_rate)
California_infection_rate <- cbind(state$V2, California_infection_rate)
Florida_infection_rate <- cbind(state$V3, Florida_infection_rate)

NewYork_deaths_rate <- cbind(state$V1, NewYork_deaths_rate)
California_deaths_rate <- cbind(state$V2, California_deaths_rate)
Florida_deaths_rate <- cbind(state$V3, Florida_deaths_rate)

setnames(NewYork_infection_rate,'"New York"',"state")
setnames(California_infection_rate,'"California"',"state")
setnames(Florida_infection_rate,'"Florida"',"state")

setnames(NewYork_deaths_rate,'"New York"',"state")
setnames(California_deaths_rate,'"California"',"state")
setnames(Florida_deaths_rate,'"Florida"',"state")

infection_rate <- rbind(NewYork_infection_rate,California_infection_rate,Florida_infection_rate)

deaths_rate <- rbind(NewYork_deaths_rate, California_deaths_rate, Florida_deaths_rate)


