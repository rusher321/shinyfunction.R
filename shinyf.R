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

## used to rev the string
revString <- function(text){
  paste(rev(unlist(strsplit(text,NULL))),collapse="")
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

## get the multigroup permanova
multiadonis <- function(dat, config, perm=1000, method="bray"){
  # reorder the sample id of dat and config 
  # dat : row is sample 
  # perm : permutation number ,default 1000
  # method : distance method
  
  id <- intersect(rownames(dat), rownames(config))
  dat <- dat[id, ]
  config <- config[id,]
  combng <- combn(unique(config), 2)
  res <- matrix(NA, nrow=ncol(combng), ncol = 6)
  for(i in 1:ncol(combng)){
    group <- c(config[which(config==combng[1,i])],
               config[which(config==combng[2,i])])
    pro <- dat[c(which(config==combng[1,i]), 
                 which(config==combng[2,i])), ]
    # here use the adonise function 
    library(vegan)
    res[i, 1:6] <- as.numeric(adonis(pro~group, permutations = perm, method = method)$aov[1,])
  }
  rownames(res) <- apply(combng, 2, function(x){paste0(x[1], " vs ", x[2])})
  colnames(res) <- c("Df", "SumsOfSqs", "MeanSqs", "F.Model", "R2", "Pr(>F)")
  return(res)
  
}
### --- plot -----

