news_raw <- read.table("./News_train.txt", header = FALSE)
hill <- read.table("./Hill-Valley_train.txt", header = FALSE)
news_names <- read.table("./names.txt", header = FALSE, skip = 44, nrows = 59, sep = ":", col.names = c("Name", "Description"), stringsAsFactors = FALSE, strip.white = TRUE)


types <- c(sapply(strsplit(news_names[12:17, 1], "_"), "[", 4), "others")
weekdays <- sapply(strsplit(news_names[30:36, 1], "_"), "[", 3)
drops <- c(12:17, 30:36, 59)
library(dplyr)
news_reg <- news_raw %>%
        mutate(log_res = log(V59), 
               type = factor(cbind(V12, V13, V14, V15, V16, V17, 
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

news_cls <- news_raw %>%
        mutate(class_res = factor(cbind(V59<=1100, V59>1100&V59<=2100, V59>2100) %*%
                                          c(1, 2, 3), 
                                  levels = 1:3, 
                                  labels = c("low", "med", "high")), 
               type = factor(cbind(V12, V13, V14, V15, V16, V17, 
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











