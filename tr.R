
library(doMC)
library(caret)
doMC::registerDoMC(cores=4)

ctrl <- trainControl(method = "cv",
                     number = 5)
news_reg3_zero$n_tokens_content <- NULL
news_reg3_zero$n_unique_tokens <- NULL
news_reg3_zero$n_non_stop_words <- NULL
news_reg3_zero$n_non_stop_unique_tokens <- NULL

news_cls3_zero$n_tokens_content <- NULL
news_cls3_zero$n_unique_tokens <- NULL
news_cls3_zero$n_non_stop_words <- NULL
news_cls3_zero$n_non_stop_unique_tokens <- NULL

input31 <- model.matrix(log_res ~ ., data = news_reg3_nonzero)
output31 <- news_reg3_nonzero$log_res
input32 <- model.matrix(log_res ~ ., data = news_reg3_zero)
output32 <- news_reg3_zero$log_res

input41 <- model.matrix(class_res ~ ., data = news_cls3_nonzero)
output41 <- news_cls3_nonzero$class_res
input42 <- model.matrix(class_res ~ ., data = news_cls3_zero)
output42 <- news_cls3_zero$class_res

rf.grid <- expand.grid(.mtry= 1:2)
rf31 <- train(x = input31, 
              y = output31,
              method = "rf", 
              trControl = ctrl,
              tuneGrid = rf.grid, 
              do )
rf32 <- train(x = input32, 
              y = output32,
              method = "rf", 
              trControl = ctrl,
              tuneGrid = rf.grid)
rf41 <- train(x = input41, 
              y = output41,
              method = "rf", 
              trControl = ctrl,
              tuneGrid = rf.grid)
rf42 <- train(x = input42, 
              y = output42,
              method = "rf", 
              trControl = ctrl,
              tuneGrid = rf.grid)

glmnet.grid <- expand.grid(.lambda= 1:10,
                           .alpha = seq(0, 1, 0.5))
glmnet31 <- train(x = input31,
                  y = output31,
                  method ='glmnet',
                  tuneGrid = glmnet.grid, 
                  trControl = ctrl)
glmnet32 <- train(x = input32,
                  y = output32,
                  method ='glmnet',
                  tuneGrid = glmnet.grid, 
                  trControl = ctrl)



