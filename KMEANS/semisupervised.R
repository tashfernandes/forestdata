# Semi-supervised learner.
# Idea: Use combined data in kmeans cluster.
# For each cluster, use majority label to label everything in cluster
# Use all this new labelled data in the training algorithm (eg randomForest)
# We can use the error rate on the labelled data to identify a good value of k.

library(plyr)
library(ggplot2)

# Create scaled data set
forest.tx.combined <- forest.tidy.train
forest.tx.combined$Cover_Type <- NULL

forest.tx.combined$Elevation <- scale(forest.tx.combined$Elevation)
forest.tx.combined$Slope <- scale(forest.tx.combined$Slope)
forest.tx.combined$Vertical_Distance_To_Hydrology <- scale(forest.tx.combined$Vertical_Distance_To_Hydrology)
forest.tx.combined$Aspect <- scale(forest.tx.combined$Aspect)
forest.tx.combined$Horizontal_Distance_To_Hydrology <- scale(forest.tx.combined$Horizontal_Distance_To_Hydrology)
forest.tx.combined$Horizontal_Distance_To_Fire_Points <- scale(forest.tx.combined$Horizontal_Distance_To_Fire_Points)
forest.tx.combined$Hillshade_Noon <- scale(forest.tx.combined$Hillshade_Noon)
forest.tx.combined$EVDH <- scale(forest.tx.combined$EVDH)
forest.tx.combined$Hydro_Fire_1 <- scale(forest.tx.combined$Hydro_Fire_1)
forest.tx.combined$Hydro_Road_1 <- scale(forest.tx.combined$Hydro_Road_1)
forest.tx.combined$Fire_Road_1 <- scale(forest.tx.combined$Fire_Road_1)
forest.tx.combined$Horizontal_Distance_To_Roadways <- scale(forest.tx.combined$Horizontal_Distance_To_Roadways)
forest.tx.combined$Hillshade_9am <- scale(forest.tx.combined$Hillshade_9am)
forest.tx.combined$Hillshade_3pm <- scale(forest.tx.combined$Hillshade_3pm)
forest.tx.combined$EHDH <- scale(forest.tx.combined$EHDH)
forest.tx.combined$Hydrology_Distance <- scale(forest.tx.combined$Hydrology_Distance)
forest.tx.combined$Hydro_Fire_2 <- scale(forest.tx.combined$Hydro_Fire_2)
forest.tx.combined$Hydro_Road_2 <- scale(forest.tx.combined$Hydro_Road_2)
forest.tx.combined$Fire_Road_2 <- scale(forest.tx.combined$Fire_Road_2)

# Remove categorical predictors
forest.tx.combined$Highwater = NULL
forest.tx.combined$Soil_Type = NULL
forest.tx.combined$Wilderness_Area = NULL

# Kmeans model
kmeans.model <- kmeans(forest.tx.combined, 3, nstart=50)
kmeans.forest <- data.frame(cluster=kmeans.model$cluster[0:15120], cover_type=forest.tx.train$Cover_Type)
count(kmeans.forest, c("cluster", "cover_type"))

kmeans.models <- list()
for (i in 4:8) {
  kmeans.models[[i]] <- kmeans(forest.tx.combined, i, nstart=50)
}

kmeans.models[[15]] <- kmeans(forest.tx.combined, 15, nstart=50)

kmeans.forests <- list()
kmeans.counts <- list()

for (i in 4:8) {
  kmeans.forests[[i]] <- data.frame(cluster=kmeans.models[[i]]$cluster[0:15120], cover_type=forest.tx.train$Cover_Type)
  kmeans.counts[[i]] <- count(kmeans.forests[[i]], c("cluster", "cover_type"))
}
kmeans.forests[[3]] = kmeans.forest
kmeans.counts[[3]] <- count(kmeans.forest, c("cluster", "cover_type"))
# Sample plots
qplot(cluster, cover_type, data=kmeans.forests[[4]], geom="jitter", colour=cover_type)
qplot(cluster, data=kmeans.forests[[5]], geom="histogram", fill=cover_type, binwidth=1)

# No one cover type is well predicted in a single cluster.
# Recall we had to ignore our discrete variables

# Let's try PCA to reduce the set of predictors
forest.pca <- prcomp(forest.tx.combined, scale=FALSE)
forest.pca.var <- forest.pca$sdev^2                   
forest.pca.pve <- forest.pca.var / sum(forest.pca.var)  # % variance explained

plot(forest.pca.pve, xlab="Principal Component", ylab="Proportion of Variance Explained ")
plot(cumsum(forest.pca.pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained ", 
     ylim=c(0,1) ,type='b')

# The first 10 components explain *all* the variance!
best.pca <- forest.pca$x[,c(1:10)]
best.pca <- as.data.frame(best.pca)
# We are using 90% for training and reserve 10% for test.
write.csv(best.pca, file="Desktop/MRes/COMP777/Project/ForestCover/KMEANS/Data/pca.csv",
          row.names=FALSE)

# Also get the min and max values within each column.
apply(best.pca, 2, min)
apply(best.pca, 2, max)

# Now try a kmeans on this.
kmeans.pca <- kmeans(best.pca, 7, nstart=10)
kmeans.pca.forest <- data.frame(cluster=kmeans.pca$cluster, cover_type=forest.tidy.train$Cover_Type)
count(kmeans.pca.forest, c("cluster", "cover_type"))
qplot(cluster, cover_type, data=kmeans.pca.forest, geom="jitter", colour=cover_type)
qplot(cluster, data=kmeans.pca.forest, geom="histogram", binwidth=1, fill=cover_type)
