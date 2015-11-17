
# STAT 8330 Project 1 Testing code
# Peng Shao, Wenyang Wang, Shuhua Zhang

setwd("~/Documents/git/D3_proj1")
rm(list = ls())
news_test <- read.table("./News_test_features.txt", header = FALSE)
hill_test <- read.table("./Hill-Valley_test_features.txt", header = FALSE)
news_names <- read.table("./names.txt", header = FALSE, skip = 44, nrows = 59, sep = ":", col.names = c("Name", "Description"), stringsAsFactors = FALSE, strip.white = TRUE)
load("./rf1r.RData")
load("./rf1c.RData")
source("./hill.R")
library(randomForest)
library(dplyr)
library(data.table)

# Data Preprocessing
types <- c(sapply(strsplit(news_names[12:17, 1], "_"), "[", 4), "others")
weekdays <- sapply(strsplit(news_names[30:36, 1], "_"), "[", 3)
drops <- c(12:17, 30:36)
library(dplyr)
news_test <- news_test %>%
        mutate(type = factor(cbind(V12, V13, V14, V15, V16, V17, 
                                   !(V12|V13|V14|V15|V16|V17)) %*% 
                                     matrix(1:length(types)), 
                             levels = 1:length(types), 
                             labels = types), 
               weekday = factor(cbind(V30, V31, V32, V33, V34, V35, V36) %*% 
                                        matrix(1:length(weekdays)), 
                                levels = 1:length(weekdays), 
                                labels = weekdays), 
               V37 = factor(V37, levels = c(1, 0), labels = c(TRUE, FALSE))) %>%
        select(-drops)
names(news_test)[1:45] <- sapply(strsplit(news_names[, 1], ". "), "[", 2)[-c(drops, 59)]

input_features <- model.matrix( ~. , data = news_test)

# Problem 1
prediciton1reg <- predict(rf1r, newdata = input_features)
prediciton1reg <- as.data.frame(prediciton1reg)
prediction1class <- as.numeric(predict(rf1c, newdata = input_features))
prediction1class <- as.data.frame(prediction1class)

# Problem 2
prediction2 <- hill_valley(hill_test)
prediction2 <- as.data.frame(prediction2)


# Output
write.table(prediciton1reg, file = "News_logresp_SWZ.txt", 
            col.names = FALSE, row.names = FALSE)
write.table(prediction1class, file = "News_class_SWZ.txt", 
            col.names = FALSE, row.names = FALSE)
write.table(prediction2, file = "Hill_class_SWZ.txt", 
            col.names = FALSE, row.names = FALSE)
