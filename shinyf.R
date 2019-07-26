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

## rever the string in  vector
revStringVector <- function(vetor){
  res <- sapply(vector, function(x){paste(rev(unlist(strsplit(x,NULL))),collapse="")})
  return(as.vector(res))
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

## to compute the JSD distance 

dist.JSD <- function(inMatrix, pseudocount=0.0000000001, ...) {
	# to compute the JSD distance 
	inMatrix <- t(inMatrix)
	KLD <- function(x,y) sum(x *log(x/y))
	JSD<- function(x,y) sqrt(0.5 * KLD(x, (x+y)/2) + 0.5 * KLD(y, (x+y)/2))
	matrixColSize <- length(colnames(inMatrix))
	matrixRowSize <- length(rownames(inMatrix))
	colnames <- colnames(inMatrix)
	resultsMatrix <- matrix(0, matrixColSize, matrixColSize)
        
  inMatrix = apply(inMatrix,1:2,function(x) ifelse (x==0,pseudocount,x))

	for(i in 1:matrixColSize) {
		for(j in 1:matrixColSize) { 
			resultsMatrix[i,j]=JSD(as.vector(inMatrix[,i]),
			as.vector(inMatrix[,j]))
		}
	}
	colnames -> colnames(resultsMatrix) -> rownames(resultsMatrix)
	as.dist(resultsMatrix)->resultsMatrix
	attr(resultsMatrix, "method") <- "dist"
	return(resultsMatrix)

}



## to select the best cluster number based on the CH-index
kBest <- function(data, dist , method = "kmeans"){
  # data is a profile /col is sample , row is feature
  # dist is distance from vegdist or dist  
  # method is cluster method 
  nclusters=NULL
  sil = NULL
  out <- list()
  res <- matrix(NA, 19, ncol(data))
	for (k in 2:20) { 
	    #print(k)
		  switch (method,
		          kmeans = { data.cluster_temp <-kmeans(dist, k)$cluster},
		          pam = { data.cluster_temp <-pam(dist, k)$clustering},
		          fanny = {  data.cluster_temp <- fanny(dist, k)$clustering}
		                               )
		  res[k-1,] <- data.cluster_temp
			nclusters[k-1] <- index.G1(t(data) , data.cluster_temp,  d = dist,
			centrotypes = "medoids")
			sil[k-1] <- mean(silhouette(data.cluster_temp, dist = dist)[,3])
	}
  
  best <- which.max(nclusters)+1
  kCluster <- c(2:20)
  CH_index <- nclusters
  Silhouette <- sil
  cluster <- data.frame(kCluster,  CH_index, Silhouette)
  cluster <- melt(cluster, id = "kCluster")
  colnames(cluster) <- c("kCluster", "Index", "value")
  # final Theme 
  finalTheme <- theme_set(theme_bw()) +
  		theme(panel.grid.major = element_blank(),
        	panel.grid.minor = element_blank())
  
  figure <- ggplot(cluster, aes(x=value, y=kCluster))+
  geom_segment(aes(yend=kCluster),xend=0,colour="grey")+
  geom_point(size=3,aes(colour=Index))+
  scale_colour_brewer(palette="Set1",limits=c("CH_index","Silhouette"))+
  theme_bw()+finalTheme+xlab("")+ylab("Number of cluster")+facet_grid(.~Index, scales = "free")
  
  out <- list(res[best-1,], best, figure)
}


### --- plot -----
pcoaFig <- function(data.dist, cluster){
  # plot the pcoa
  # data.dist is distance data format from vegdist or dist 

  obs.pcoa=dudi.pco(data.dist, scannf=F, nf=3)
  var1 <- round((obs.pcoa$eig[1]/sum(obs.pcoa$eig))*100,2)
  var2 <- round((obs.pcoa$eig[2]/sum(obs.pcoa$eig))*100,2)
  minX <- min(obs.pcoa$li[,1])
  maxX <- max(obs.pcoa$li[,1])
  minY <- min(obs.pcoa$li[,2])
  maxY <- max(obs.pcoa$li[,2])

  plot(0,0, main = "Pcoa", type = "n",
    xlab=paste("Pco1 (",var1,"%)"),ylab=paste("Pco2 (",var2,"%)"), 
    xlim=c(minX-10^floor(log10(abs(minX))),maxX+10^floor(log10(abs(maxX)))), 
    ylim=c(minY-10^floor(log10(abs(minY))),maxY+10^floor(log10(abs(maxY)))),
    frame=TRUE, cex=1.5, add=T)
 s.class(obs.pcoa$li, fac=as.factor(cluster), cell =2 ,
        csta = 0 , col = color, grid=F, add.plot = T)

  }
