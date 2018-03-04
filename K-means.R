# k-means algorithm
library(tidyverse)
library(ggplot2)
# random points

x1 <- rnorm(50, -1, 2)
x1 <- c(x1,rnorm(50, -3, 2))
x1 <- c(x1,rnorm(50, 0, 2))
y1 <- rnorm(50, 0, 2)
y1 <- c(y1,rnorm(50, 4, 2))
y1 <- c(y1,rnorm(50, -2, 5))
xy <- data.frame('x'=x1, 'y'=y1, 'centroid'=0)
ggplot(xy, aes(x=x, y=y))+geom_point()
xc <- rnorm(3)*max(x1)
yc <- rnorm(3)*max(y1)
xyc <- data.frame(xc,yc);xyc
ggplot(xy, aes(x=x, y=y))+geom_point()+geom_point(data=xyc, aes(x=xc,y=yc), color="red")

# make distance function
dist <- function(a,b){
  d <- sqrt((a[,1]-b[,1])^2 + (a[,2]-b[,2])^2)
  return (d)
}

# make progress by mean

for (k in 1:10){
  for (i in 1:nrow(xy)){
    distanc <- Inf
    closest <- 2
    for (j in 1:nrow(xyc)){
      fordist <- dist(xy[i,],xyc[j,])
      if (fordist < distanc){
        distanc <- fordist
        closest <- j
      }
    }
    xy[i,'centroid'] <- closest
  }
  cencol <- ifelse(xy[,'centroid']==1, "black", ifelse(xy[,'centroid']==2, "pink", "forestgreen"))
  mean_step <- xy %>% group_by(centroid) %>% summarise_all(mean)
  xyc <- as.data.frame(mean_step[-1])
  colnames(xyc) <- c('xc','yc')
  print(xyc)
  print(ggplot(xy, aes(x=x, y=y, color=cencol))+geom_point()+
          geom_point(data=xyc, aes(x=xc,y=yc), color="black", size = 5, shape = 3))
  Sys.sleep(3)
}
