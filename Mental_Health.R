setwd("~/Desktop/DS 340W/paper data")

library("data.table")
library("ggplot2")

data <- fread("Mental_Health.csv")#https://data.cdc.gov/NCHS/Indicators-of-Anxiety-or-Depression-Based-on-Repor/8pt5-q6wp
sample_data<- fread("Num_Response.csv")#https://www.cdc.gov/nchs/covid19/pulse/mental-health.htm

#sample_data processing
sample_data$weighted_response_rate <- as.numeric(sub("%","",sample_data$weighted_response_rate))/100
sample_data$sample_size <- (sub(",","",sample_data$sample_size))
sample_data$number_response <- sample_data$weighted_response_rate * as.numeric(sample_data$sample_size)

#graph of sample number
test1 <- factor(sample_data$week,levels = sample_data$week)
graph_numberResponse <- data.frame(test1,sample_data$number_response)# <- sample_data[,.(week,number_response)]

setnames(graph_numberResponse,"test1","week")
setnames(graph_numberResponse,"sample_data.number_response","number_response")

p <- ggplot(data = graph_numberResponse, mapping = aes(x = number_response, y = week))
p + geom_col()

#National Estimate
national<- data[Group == 'National Estimate']

#byAge

#byGneder

#byRace/Hispanic ethnicity

