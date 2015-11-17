
hill_valley <- function(hill){
        norm_hill <- t(apply(as.matrix(hill)[, 1:100], 1, scale))
        return(ifelse(sign(norm_hill[cbind(1:nrow(hill), apply(abs(norm_hill), 
                                                        1, 
                                                        which.max))])>0, 
                      1, 
                      0))
}
