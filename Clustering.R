library(tidyverse)
library(data.table)
library(broom)

train <-  fread("clustering_train.csv")
test <-  fread("clustering_test.csv")  

# parameterization
minClusters        <- 1
maxClusters        <- 10
seed               <- 661641

#Random Number Generator
set.seed(seed)

#kmeans()
kmTWSS      <- rep(-1,maxClusters)

for (k in minClusters:maxClusters){ 
  set.seed(seed)
  kmTWSS[k]  <- kmeans(select(train,v1:v5), 
                       k, nstart=5)$tot.withinss
}

#[[kmTWSS]]
kmTWSS

#Compute Euclidean Distance for Hclust
Eucli_dis <- dist(train, method = "euclidean")

#hclust()
hierarchy_mod <- hclust(Eucli_dis,method = "complete")
memb <- cutree(hierarchy_mod,minClusters:maxClusters)
memb_DT <- data.table(memb)
colnames(memb_DT) <- c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10")

x.SS <- aggregate(train, by=list(memb[, 1]), function(train) sum(scale(train,scale=FALSE)^2))
TSS_g1 <- rowSums(x.SS[, -1])

TSS <- function(x, g) {
  sum(aggregate(x, by=list(g), function(x) sum(scale(x, 
                                                     scale=FALSE)^2))[, -1])
}

hcTWSS <- apply(memb,2, function(g) TSS(train,g))

#[[hcTWSS]]
hcTWSS