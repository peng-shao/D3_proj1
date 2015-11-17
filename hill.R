
hill_valley <- function(hill){
        norm_hill <- t(apply(as.matrix(hill)[, -101], 1, scale))
        return(ifelse(sign(norm_hill[cbind(1:700, apply(abs(norm_hill), 
                                                        1, 
                                                        which.max))])>0, 
                      1, 
                      0))
}
pred_hill <- hill_valley(hill)
library(caret)
confusionMatrix(hill$V101, pred_hill)
