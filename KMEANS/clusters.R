# This file loads the pca data and runs clusGap to see what the optimum
# number of clusters is for kmeans

library(cluster)

DATAFILE <- "Desktop/MRes/COMP777/Project/forestdata/KMEANS/Data/pca-1000.csv"
pca.data <- as.data.frame(read.csv(DATAFILE))

NUM_CLUSTERS <- 20
gap <- clusGap(pca.data, kmeans, NUM_CLUSTERS, B=100)
write.csv(gap, file="gap.csv", row.names=FALSE)
gap
