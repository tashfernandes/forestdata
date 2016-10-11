# Forest Cover Project
# 1. Load in the data
forest.data.train <- read.csv("Desktop/MRes/COMP777/Project/ForestCover/KMEANS/Data/shuffled.csv")

# Create a tidy data set with the factors in place for analysis
forest.tidy.train <- forest.data.train[,c('Elevation','Aspect',
                              'Slope','Horizontal_Distance_To_Hydrology',
                              'Vertical_Distance_To_Hydrology','Horizontal_Distance_To_Roadways',
                              'Horizontal_Distance_To_Fire_Points',
                              'Hillshade_9am','Hillshade_Noon','Hillshade_3pm','Cover_Type')]

attach(forest.data.train)
# Insert Wilderness_Area column with combined values for plotting
Wilderness_Area <- rep(1, nrow(forest.data.train))
Wilderness_Area[Wilderness_Area4==1] =4
Wilderness_Area[Wilderness_Area3==1] =3
Wilderness_Area[Wilderness_Area2==1] =2
forest.tidy.train$Wilderness_Area <- factor(Wilderness_Area)

# Combine soil type data
Soil_Type <- rep(1, nrow(forest.data.train))
Soil_Type[Soil_Type2==1] =2
Soil_Type[Soil_Type3==1] =3
Soil_Type[Soil_Type4==1] =4
Soil_Type[Soil_Type5==1] =5
Soil_Type[Soil_Type6==1] =6
Soil_Type[Soil_Type7==1] =7
Soil_Type[Soil_Type8==1] =8
Soil_Type[Soil_Type9==1] =9
Soil_Type[Soil_Type10==1] =10
Soil_Type[Soil_Type11==1] =11
Soil_Type[Soil_Type12==1] =12
Soil_Type[Soil_Type13==1] =13
Soil_Type[Soil_Type14==1] =14
Soil_Type[Soil_Type15==1] =15
Soil_Type[Soil_Type16==1] =16
Soil_Type[Soil_Type17==1] =17
Soil_Type[Soil_Type18==1] =18
Soil_Type[Soil_Type19==1] =19
Soil_Type[Soil_Type20==1] =20
Soil_Type[Soil_Type21==1] =21
Soil_Type[Soil_Type22==1] =22
Soil_Type[Soil_Type23==1] =23
Soil_Type[Soil_Type24==1] =24
Soil_Type[Soil_Type25==1] =25
Soil_Type[Soil_Type26==1] =26
Soil_Type[Soil_Type27==1] =27
Soil_Type[Soil_Type28==1] =28
Soil_Type[Soil_Type29==1] =29
Soil_Type[Soil_Type30==1] =30
Soil_Type[Soil_Type31==1] =31
Soil_Type[Soil_Type32==1] =32
Soil_Type[Soil_Type33==1] =33
Soil_Type[Soil_Type34==1] =34
Soil_Type[Soil_Type35==1] =35
Soil_Type[Soil_Type36==1] =36
Soil_Type[Soil_Type37==1] =37
Soil_Type[Soil_Type38==1] =38
Soil_Type[Soil_Type39==1] =39
Soil_Type[Soil_Type40==1] =40
forest.tidy.train$Soil_Type <- factor(Soil_Type)
detach(forest.tidy.train)

# Add cover type
forest.tidy.train$Cover_Type <- factor(forest.data.train$Cover_Type)

library(ggplot2)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
# Do some plotting
# We see wilderness area 4 is at significantly lower elevation
qplot(Wilderness_Area, Elevation, data=forest.tidy.train, colour=Cover_Type, geom="jitter")
# Tree cover type seems to be dependent on elevation
qplot(Cover_Type, Elevation, data=forest.tidy.train, colour=Cover_Type, geom="jitter", xlab="Cover type")
# Particular wilderness areas have particular cover types
# One cover type is exclusive to a specific wilderness area
# Some cover types are restricted to particular soil types
qplot(Cover_Type, Soil_Type, data=forest.tidy.train, geom="jitter", colour=Cover_Type, xlab="Cover type", ylab="Soil type")
qplot(Cover_Type, Aspect, data=forest.tidy.train, geom="jitter", colour=Cover_Type)
qplot(Cover_Type, Slope, data=forest.tidy.train, geom="jitter", colour=Cover_Type)
qplot(Cover_Type, Horizontal_Distance_To_Hydrology, data=forest.tidy.train, geom="jitter", colour=Cover_Type)
qplot(Cover_Type, Vertical_Distance_To_Hydrology, data=forest.tidy.train, geom="jitter", colour=Cover_Type)
qplot(Cover_Type, Horizontal_Distance_To_Roadways, data=forest.tidy.train, geom="jitter", colour=Cover_Type)
qplot(Cover_Type, Horizontal_Distance_To_Fire_Points, data=forest.tidy.train, geom="jitter", colour=Cover_Type)
qplot(Cover_Type, Hillshade_9am, data=forest.tidy.train, geom="jitter", colour=Cover_Type)
qplot(Cover_Type, Hillshade_Noon, data=forest.tidy.train, geom="jitter", colour=Cover_Type)
qplot(Cover_Type, Hillshade_3pm, data=forest.tidy.train, geom="jitter", colour=Cover_Type)

# Cover type 4 seems to be restricted but that's because it's only in wilderness area 4
qplot(Wilderness_Area, Horizontal_Distance_To_Hydrology, data=forest.tidy.train, geom="jitter", colour=Cover_Type)
qplot(Wilderness_Area, Vertical_Distance_To_Hydrology, data=forest.tidy.train, geom="jitter", colour=Cover_Type)
qplot(Wilderness_Area, Horizontal_Distance_To_Roadways, data=forest.tidy.train, geom="jitter", colour=Cover_Type)
qplot(Wilderness_Area, Horizontal_Distance_To_Fire_Points, data=forest.tidy.train, geom="jitter", colour=Cover_Type)

p3 <- qplot(Wilderness_Area, Soil_Type, data=forest.tidy.train, geom="jitter", colour=Cover_Type)
p4 <- qplot(Wilderness_Area, Soil_Type, data=forest.tidy.test, geom="jitter", colour=Wilderness_Area)
multiplot(p1, p2, p3, p4, cols=2)
p5 <- qplot(Soil_Type, Elevation, data=forest.tidy.train, geom="jitter", colour=Cover_Type)
p5
p6 <- qplot(Soil_Type, Elevation, data=forest.tidy.test, geom="jitter")
p6
multiplot(p5, p6, cols=2)

# Check that training data is representative of test data for variables
# we are interested in.


# Split the data into dev and test

# Dev data set
elevation.dev.data <- as.data.frame(forest.data.train$Elevation)
train.error <- rep(0, 50)
index <- rep(0, 50)
for (i in 1:50) {
  result <- knn(elevation.dev.data, elevation.dev.data, forest.data.train$Cover_Type1,
                k=i)
  train.error[i] <- mean(result != forest.data.train$Cover_Type1)
  index[i] = 50 - i
}

qplot(index, train.error, geom="line", xlab="50-K")

# Test data set
set.seed(7575)
data.size <- nrow(forest.data.train)
training.size <- data.size / 2
test.error <- rep(0, 50)
train <- sample(data.size, training.size)

elevation.train.data <- as.data.frame(forest.data.train$Elevation[train])
elevation.test.data <- as.data.frame(forest.data.train$Elevation[-train])
# Do some knn on the data
library(class)
for (i in 1:50) {
  result = knn(elevation.train.data, elevation.test.data, 
               forest.data.train$Cover_Type1[train], k=i)
  test.error[i] = mean(result != forest.data.train$Cover_Type1[-train])
}
library(reshape)
plot.data <- melt(data.frame(id=50:1,train.error = train.error, test.error=test.error), id="id")
qplot(id, value, data = plot.data, geom="line", colour=variable) + scale_x_continuous("50-K")
