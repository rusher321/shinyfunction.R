###----- text --------
## used to join the vector 
pasteP <- function(x, sep){
  # x is vector 
  # sep is a  character string to separate the terms
  # join the vector
  len <- length(x)
  y <- c()
  for(i in 1:len){
    y <- paste(y, x[i], sep = sep)
  }
  return(y)
}
###----- stat ---------
## to filter the outlier of vector 
filterE <- function(x, percent = 0.05){
    # percent make sure you will remove the data
    # percent < 50%
    num <- length(x)
    y <- sort(x)
    y2 <- y[round(num*percent):round(num*(1-percet))]
    return(y2)
  }

## get intra distance 
getD <- function(pro1, pro2, methods){
    # pro1 pro2 is matrix 
    # methods is distance 
    dis <- c()
    for(i in 1:ncol(pro1)){
      x1 <- as.numeric(pro1[,i])
      x2 <- as.numeric(pro2[,i])
      if(methods == "Hell"){
        library(distrEx)
        x1 <- filterE(x1)
        x2 <- filterE(x2)
        tmp <- HellingerDist(e1 = Norm(mean = mean(x1), sd=sd(x1)), e2 = Norm(mean = mean(x2), sd=sd(x2)))
        dis <- c(dis, tmp)
      }else if(methods == "jsd"){
        library(philentropy)
        tmp <- JSD(rbind(x1, x2))
        dis <- c(dis, tmp)
      }else{
        library(vegan)
        tmp <- vegdist(rbind(x1, x2), method = methods)
        dis <- c(dis, tmp)
      }
    }
