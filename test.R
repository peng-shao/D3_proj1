library(dplyr)
news <- read.table("./News_train.txt", header = FALSE)
hill <- read.table("./Hill-Valley_train.txt", header = FALSE)


trial <- sample(1:nrow(news), 1000)
trial <- news[trial,]
h_v <- function(input){
        x <- t(apply(matrix(as.numeric(as.matrix(input[, -101])), ncol = 100), 
                     1, scale))
        output <- apply(x, 1, function(z)ifelse(sign(z[which.max(abs(z))]>0), 1, 0))
}