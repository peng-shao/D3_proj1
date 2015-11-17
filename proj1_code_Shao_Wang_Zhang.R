
# R code for project 1
# Peng Shao, Wenyang Wang, Shuhua Zhang
# Problem 1
# Reading data
news_raw <- read.table("./News_train.txt", header = FALSE)
hill <- read.table("./Hill-Valley_train.txt", header = FALSE)
news_names <- read.table("./names.txt", header = FALSE, skip = 44, nrows = 59, sep = ":", col.names = c("Name", "Description"))
# Data preprocessing
library(dplyr)
types <- c(sapply(strsplit(news_names[12:17, 1], "_"), "[", 4), "others")
weekdays <- sapply(strsplit(news_names[30:36, 1], "_"), "[", 3)
drops <- c(12:17, 30:36, 59)

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
names(news_reg)[1:45] <- sapply(strsplit(news_names[, 1], ". "), "[", 2)[-drops]

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
names(news_cls)[1:45] <- sapply(strsplit(news_names[, 1], ". "), "[", 2)[-drops]

# Missing values processing
news_reg1 <- news_reg
news_cls1 <- news_cls

news_reg2 <- news_reg
news_reg2 <- news_reg2[!(news_reg2$n_tokens_content == 0), ]
news_cls2 <- news_cls
news_cls2 <- news_cls2[!(news_cls2$n_tokens_content == 0), ]

news_reg3 <- news_reg
news_cls3 <- news_cls
news_reg3_zero <- news_reg3[(news_reg3$n_tokens_content == 0), ]
news_reg3_nonzero <- news_reg3[!(news_reg3$n_tokens_content == 0), ]
news_cls3_zero <- news_cls3[(news_cls3$n_tokens_content == 0), ]
news_cls3_nonzero <- news_cls3[!(news_cls3$n_tokens_content == 0), ]

news_reg4 <- news_reg
news_cls4 <- news_cls
ind <- (news_reg4$n_tokens_content == 0)
news_reg4[ind, 2] <- round(mean(news_reg4[!ind, 2]))
news_reg4[ind, 3] <- mean(news_reg4[!ind, 3])
news_reg4[ind, 4] <- mean(news_reg4[!ind, 4])
news_reg4[ind, 5] <- mean(news_reg4[!ind, 5])
news_cls4[ind, 2] <- round(mean(news_cls4[!ind, 2]))
news_cls4[ind, 3] <- mean(news_cls4[!ind, 3])
news_cls4[ind, 4] <- mean(news_cls4[!ind, 4])
news_cls4[ind, 5] <- mean(news_cls4[!ind, 5])


# Model Fitting
# 1
input1r <- model.matrix(log_res ~., data = news_reg1)
output1r <- news_reg1$log_res
input1c <- model.matrix(class_res ~., data = news_cls1)
output1c <- news_cls1$class_res
ntree <- 250
# For regression
mse <- 10
rf1r <- randomForest(x = input1r[1:100, ], y = output1r[1:100], mtry = 1, ntree = 1, do.trace = TRUE)
for (i in c(8:13)){
        print(i)
        rf_reg <- randomForest(x = input1r, y = output1r, mtry = i, ntree = ntree, do.trace = TRUE)
        mse_new <- rf_reg$mse[ntree]
        if (mse_new < mse){
                mse <- mse_new
                rf1r <- rf_reg
        }
}
save(rf1r, file = "./rf1r.RData")
# For classification
err <- 1
rf1c <- randomForest(x = input1c[1:1000, ], y = output1c[1:1000], mtry = 1, ntree = 1, do.trace = TRUE)
for (i in c(8:13)){
        print(i)
        rf_cls <- randomForest(x = input1c, y = output1c, mtry = i, ntree = ntree, do.trace = TRUE)
        err_new <- rf_cls$err.rate[1]
        if (err_new < err){
                err <- err_new
                rf1c <- rf_cls
        }
}
save(rf1c, file = "./rf1c.RData")


# 2
input2r <- model.matrix(log_res ~., data = news_reg2)
output2r <- news_reg2$log_res
input2c <- model.matrix(class_res ~., data = news_cls2)
output2c <- news_cls2$class_res
ntree <- 250
# For regression
mse <- 10
rf2r <- randomForest(x = input2r[1:100, ], y = output2r[1:100], mtry = 1, ntree = 1, do.trace = TRUE)
for (i in c(8:13)){
        print(i)
        rf_reg <- randomForest(x = input2r, y = output2r, mtry = i, ntree = ntree, do.trace = TRUE)
        mse_new <- rf_reg$mse[ntree]
        if (mse_new < mse){
                mse <- mse_new
                rf2r <- rf_reg
        }
}
save(rf2r, file = "./rf2r.RData")
# For classification
err <- 1
rf2c <- randomForest(x = input2c[1:1000, ], y = output2c[1:1000], mtry = 1, ntree = 1, do.trace = TRUE)
for (i in c(8:13)){
        print(i)
        rf_cls <- randomForest(x = input2c, y = output2c, mtry = i, ntree = ntree, do.trace = TRUE)
        err_new <- rf_cls$err.rate[1]
        if (err_new < err){
                err <- err_new
                rf2c <- rf_cls
        }
}
save(rf2c, file = "./rf2c.RData")


# 3
input31 <- model.matrix(log_res ~., data = news_reg3_nonzero)
output31 <- news_reg3_nonzero$log_res
input32 <- model.matrix(log_res ~., data = news_reg3_zero)
output32 <- news_reg3_zero$log_res
input41 <- model.matrix(class_res ~., data = news_cls3_nonzero)
output41 <- news_cls3_nonzero$class_res
input42 <- model.matrix(class_res ~., data = news_cls3_zero)
output42 <- news_cls3_zero$class_res
ntree <- 250
# For regression
mse <- 10
rf31 <- randomForest(x = input31[1:100, ], y = output31[1:100], mtry = 1, ntree = 1, do.trace = TRUE)
for (i in c(8:13)){
        print(i)
        rf_reg <- randomForest(x = input31, y = output31, mtry = i, ntree = ntree, do.trace = TRUE)
        mse_new <- rf_reg$mse[ntree]
        if (mse_new < mse){
                mse <- mse_new
                rf31 <- rf_reg
        }
}
save(rf31, file = "./rf31.RData")
mse <- 10
rf32 <- randomForest(x = input32[1:100, ], y = output32[1:100], mtry = 1, ntree = 1, do.trace = TRUE)
for (i in c(8:13)){
        print(i)
        rf_reg <- randomForest(x = input32, y = output32, mtry = i, ntree = ntree, do.trace = TRUE)
        mse_new <- rf_reg$mse[ntree]
        if (mse_new < mse){
                mse <- mse_new
                rf31 <- rf_reg
        }
}
save(rf32, file = "./rf32.RData")
# for classification
err <- 1
rf41 <- randomForest(x = input41[1:1000, ], y = output41[1:1000], mtry = 1, ntree = 1, do.trace = TRUE)
for (i in c(8:13)){
        print(i)
        rf_cls <- randomForest(x = input41, y = output41, mtry = i, ntree = ntree, do.trace = TRUE)
        err_new <- rf_cls$err.rate[1]
        if (err_new < err){
                err <- err_new
                rf41 <- rf_cls
        }
}
save(rf41, file = "./rf41.RData")
err <- 1
rf42 <- randomForest(x = input42[1:1000, ], y = output42[1:1000], mtry = 1, ntree = 1, do.trace = TRUE)
for (i in c(8:13)){
        print(i)
        rf_cls <- randomForest(x = input42, y = output42, mtry = i, ntree = ntree, do.trace = TRUE)
        err_new <- rf_cls$err.rate[1]
        if (err_new < err){
                err <- err_new
                rf42 <- rf_cls
        }
}
save(rf42, file = "./rf42.RData")



# 4
input4r <- model.matrix(log_res ~., data = news_reg4)
output4r <- news_reg4$log_res
input4c <- model.matrix(class_res ~., data = news_cls4)
output4c <- news_cls4$class_res
ntree <- 250
mse <- 10
rf4r <- randomForest(x = input4r[1:100, ], y = output4r[1:100], mtry = 1, ntree = 1, do.trace = TRUE)
for (i in c(8:13)){
        print(i)
        rf_reg <- randomForest(x = input4r, y = output4r, mtry = i, ntree = ntree, do.trace = TRUE)
        mse_new <- rf_reg$mse[ntree]
        if (mse_new < mse){
                mse <- mse_new
                rf4r <- rf_reg
        }
}
save(rf4r, file = "./rf4r.RData")
# for classification
err <- 1
rf4c <- randomForest(x = input4c[1:1000, ], y = output4c[1:1000], mtry = 1, ntree = 1, do.trace = TRUE)
for (i in c(8:13)){
        print(i)
        rf_cls <- randomForest(x = input4c, y = output4c, mtry = i, ntree = ntree, do.trace = TRUE)
        err_new <- rf_cls$err.rate[1]
        if (err_new < err){
                err <- err_new
                rf4c <- rf_cls
        }
}
save(rf4c, file = "./rf4c.RData")

# Problem 2
hill_valley <- function(hill){
        norm_hill <- t(apply(as.matrix(hill)[, -101], 1, scale))
        return(ifelse(sign(norm_hill[cbind(1:700, apply(abs(norm_hill), 
                                                        1, 
                                                        which.max))])>0, 
                      1, 
                      0))
}