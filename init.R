# Forest Cover Project
# 1. Load in the data
forest.data.train <- read.csv("Desktop/MRes/COMP777/Project/forestdata/raw/shuffled.csv")

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

# Now compute a transform set containing all the predictors
forest.tx.train <- forest.tidy.train

# Remove id as predictor
forest.tx.train$Id = NULL


forest.tx.train$EVDH = forest.tx.train$Elevation - forest.tx.train$Vertical_Distance_To_Hydrology
forest.tx.train$EHDH = forest.tx.train$Elevation - (0.2 * forest.tx.train$Horizontal_Distance_To_Hydrology)

forest.tx.train$Highwater <- forest.tx.train$Vertical_Distance_To_Hydrology < 0
forest.tx.train$Highwater <- as.factor(forest.tx.train$Highwater)

forest.tx.train$Hydrology_Distance <- sqrt(forest.tx.train$Horizontal_Distance_To_Hydrology^2 +
                                             forest.tx.train$Vertical_Distance_To_Hydrology^2)

forest.tx.train$Hydro_Fire_1 <- forest.tx.train$Horizontal_Distance_To_Hydrology +
  forest.tx.train$Horizontal_Distance_To_Fire_Points
forest.tx.train$Hydro_Fire_2 <- forest.tx.train$Horizontal_Distance_To_Hydrology -
  forest.tx.train$Horizontal_Distance_To_Fire_Points

forest.tx.train$Hydro_Road_1 <- forest.tx.train$Horizontal_Distance_To_Hydrology +
  forest.tx.train$Horizontal_Distance_To_Roadways
forest.tx.train$Hydro_Road_2 <- forest.tx.train$Horizontal_Distance_To_Hydrology -
  forest.tx.train$Horizontal_Distance_To_Roadways

forest.tx.train$Fire_Road_1 <- forest.tx.train$Horizontal_Distance_To_Fire_Points +
  forest.tx.train$Horizontal_Distance_To_Roadways
forest.tx.train$Fire_Road_2 <- forest.tx.train$Horizontal_Distance_To_Fire_Points -
  forest.tx.train$Horizontal_Distance_To_Roadways

# Scale all of the predictors

forest.tx.train$Elevation <- scale(forest.tx.train$Elevation)
forest.tx.train$Slope <- scale(forest.tx.train$Slope)
forest.tx.train$Vertical_Distance_To_Hydrology <- scale(forest.tx.train$Vertical_Distance_To_Hydrology)
forest.tx.train$Aspect <- scale(forest.tx.train$Aspect)
forest.tx.train$Horizontal_Distance_To_Hydrology <- scale(forest.tx.train$Horizontal_Distance_To_Hydrology)
forest.tx.train$Horizontal_Distance_To_Fire_Points <- scale(forest.tx.train$Horizontal_Distance_To_Fire_Points)
forest.tx.train$Hillshade_Noon <- scale(forest.tx.train$Hillshade_Noon)
forest.tx.train$EVDH <- scale(forest.tx.train$EVDH)
forest.tx.train$Hydro_Fire_1 <- scale(forest.tx.train$Hydro_Fire_1)
forest.tx.train$Hydro_Road_1 <- scale(forest.tx.train$Hydro_Road_1)
forest.tx.train$Fire_Road_1 <- scale(forest.tx.train$Fire_Road_1)
forest.tx.train$Horizontal_Distance_To_Roadways <- scale(forest.tx.train$Horizontal_Distance_To_Roadways)
forest.tx.train$Hillshade_9am <- scale(forest.tx.train$Hillshade_9am)
forest.tx.train$Hillshade_3pm <- scale(forest.tx.train$Hillshade_3pm)
forest.tx.train$EHDH <- scale(forest.tx.train$EHDH)
forest.tx.train$Hydrology_Distance <- scale(forest.tx.train$Hydrology_Distance)
forest.tx.train$Hydro_Fire_2 <- scale(forest.tx.train$Hydro_Fire_2)
forest.tx.train$Hydro_Road_2 <- scale(forest.tx.train$Hydro_Road_2)
forest.tx.train$Fire_Road_2 <- scale(forest.tx.train$Fire_Road_2)

# Write out the transformed set now

write.csv(forest.tx.train, file="Desktop/MRes/COMP777/Project/forestdata/raw/transformed.csv",
          row.names=TRUE)

# Let's get rid of the predictors we don't need for regression
forest.regr.train <- forest.tx.train
forest.regr.train$Wilderness_Area <- NULL
forest.regr.train$Soil_Type <- NULL

# Write this out as the new regression data
write.csv(forest.regr.train, file="Desktop/MRes/COMP777/Project/forestdata/raw/tx_regression.csv",
          row.names=TRUE)
