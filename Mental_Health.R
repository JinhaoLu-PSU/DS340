setwd("~/Desktop/DS 340W/paper data")

library("data.table")

data <- fread("Mental_Health.csv")
sample_data<- fread("Num_Response.csv")

#sample_data processing
sample_data$weighted_response_rate <- as.numeric(sub("%","",sample_data$weighted_response_rate))/100
sample_data$sample_size <- (sub(",","",sample_data$sample_size))
sample_data$number_response <- sample_data$weighted_response_rate * as.numeric(sample_data$sample_size)



#National Estimate
national<- data[Group == 'National Estimate']

#byAge

#byGneder

#byRace/Hispanic ethnicity

