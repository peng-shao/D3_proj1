# test set
setwd("~/Documents/workspace/r")

original_data <- read.csv("./OnlineNewsPopularity.csv", header = TRUE)
original_data$url <- NULL
original_data$timedelta <- NULL
# original_data$shares <- NULL
key.ind <- c(1, 2, 6:9, 11:17, 21, 27, 28, 30:37, 59)
datawikle <- news_raw
# key
# 1, 2, 8, 23, 24(not useful now)

library(data.table)
names <- sapply(strsplit(news_names[, 1], ". "), "[", 2)
names(datawikle) <- names
keynames <- names[key.ind]
dt1 <- data.table(datawikle, key = keynames)
dt2 <- data.table(original_data, key = keynames)
test <- dt2[!dt1]
write.table(test, file = "./proj1_test.RData")

# > datawikle[1, 23]
# [1] 358580
# > datawikle[1, c(1, 2, 8, 23, 24)]
# n_tokens_title n_tokens_content num_imgs kw_avg_max kw_min_avg
# 1              8              298        1     358580   2078.957

# nrow(distinct(news_raw[, -c(3:5, 10, 20, 23, 24, 25, 26, 38:58)]))
# new kwy
# 1, 2, 6:9, 11:17, 21, 27, 28, 30:37, 59


# test key
# key.ind[-length(key.ind)]
names(news_test) <- names[-length(names)]
k.ind.new <- c(1, 2, 6:9, 11:17, 21, 27, 28, 30:37)
keynames.new <- names[k.ind.new]
dt3 <- data.table(news_test, key = keynames.new)
test2 <- test[!dt3]
test3 <- test[!test2]
nrow(test3)
save(test3, file = "./test_with_response.RData")
