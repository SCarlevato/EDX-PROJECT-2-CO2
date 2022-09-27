# Installations :

install.packages("naniar")
install.packages("visdat")
install.packages("corrplot")
install.packages("mapview")
install.packages('caTools')
install.packages("quantable")
install.packages("fastDummies")
install.packages("ISLR")
install.packages("glmnet")
install.packages("caret")
install.packages("randomForest")
install.packages("Metrics")

# Imports :

library(naniar)
library(visdat)
library(ggplot2)
library(RColorBrewer)
library(corrplot)
library(mapview)
library(tidyverse)
library(sf)
library(mapview)
library(caTools)
library('fastDummies')
library(ISLR)
library(glmnet)
library(tidyr)
library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library(xgboost) 
library(caret) 
library(glmnet)
library(Metrics)
library(randomForest)

##### I INTRODUCTION : Understanding Of The Dataset : #####

#### A- Load Data And Overview : ####

### 1- Loading csv file "Building 2016" : ###

building2016 <- read.csv("/Users/sylvaincarlevato/PRISONNER666/2016_Building_Energy_Benchmarking.csv")

### 2- Visualization of the "Building 2016" Dataframe : ###

head(building2016)

### 3- Names of Columns in the Dataset : ###

names(building2016)

### 4- Number of Rows and Columns of the Initial Dataset : ###

dim(building2016)

### 5- Types of Qualitative and Quantitative Variables of the Initial Dataset : ###

str(building2016)

### 6- Quantification of Duplicate Values of the Initial Dataset : ###

sum(duplicated(building2016))

### 7- Statistical Informations of the Initial Dataset : ###

summary(building2016)

#### B- Missing Values : ####

### 1- Location of NaNs in the Dataset : ###

is.na(building2016)

### 2- Sum of NaNs in Dataset : ###

sum(is.na(building2016)) 

### 3- List of Variables and their Types of the Entire Dataset : ###

vis_dat(building2016)

### 4- Percentage of Missing Values : ###

vis_miss(building2016)

### 5- Other Visualizations of Missing Dataset Values : ### 

gg_miss_var(building2016)

#### C- Executive Summary : Understanding Targets : ####

### ALPHA - TotalGHGEmissions : ###

### 1- Visualization of the First 10 Values of the TotalGHGEmissions Column : ###

ghge <- building2016 %>% select(starts_with("TotalGHGEmissions"))

ghge[1:10,]

### 2- Descriptive Statistics of the TotalGHGEmissions Column : ###

summary(ghge)

### BETA - SiteEnergyUseWN(kBtu) : ###

### 1- Visualization of the First 10 Values of the SiteEnergyUseWN(kBtu) Column : ###

energ <- building2016 %>% select(starts_with("SiteEnergyUseWN.kBtu."))

energ[1:10,]

### 2- Descriptive Statistics of the SiteEnergyUseWN(kBtu) Column : ###

summary(energ)

#### D- Selection of the type of Dwellings : ####

### 1- Type of Buildings to choose : ### 

building2016 %>% select(BuildingType)

### 2- Basic Piechart of Dwellings : ###

pie(table(building2016$BuildingType), main = "Type of Habitations")

### 3- Selection of Buildings not intended for Housing : ###

building2016nonres <- filter(building2016, BuildingType == "NonResidential")

building2016nonres

#### E- Feature Engineering : ####

### 1- Creating "BuildingAge" Column : ###

building2016nonres$BuildingAge <- building2016nonres$DataYear - building2016nonres$YearBuilt

building2016nonres$BuildingAge

building2016nonres

### 2- Moving Column BuildingAge : ###

building2016nonres <- building2016nonres %>% relocate(BuildingAge, .before = BuildingType)

building2016nonres

### 3- New Variables : ###

building2016nonres$GFAPerBuilding <- (building2016nonres$PropertyGFATotal / building2016nonres$NumberofBuildings)

building2016nonres$GFAPerFloor <- (building2016nonres$PropertyGFATotal / building2016nonres$NumberofFloors)

building2016nonres$EnergyUsePerFloor <- (building2016nonres$SiteEnergyUseWN.kBtu. / building2016nonres$NumberofFloors)

building2016nonres$EnergyUsePerBuilding <- (building2016nonres$SiteEnergyUseWN.kBtu. / building2016nonres$NumberofBuildings)

building2016nonres$TotalGHGEmissionsPerFloor <- (building2016nonres$TotalGHGEmissions / building2016nonres$NumberofFloors)

building2016nonres$GHGEmissionsIntensityPerFloor <- (building2016nonres$GHGEmissionsIntensity / building2016nonres$NumberofFloors)

building2016nonres

### 4- Replacement of NaN by Mean : ###

building2016nonres <- subset(building2016nonres, select = - c(YearsENERGYSTARCertified, Comments))

building2016nonres$SecondLargestPropertyUseTypeGFA[is.na(building2016nonres$SecondLargestPropertyUseTypeGFA)] <- mean(building2016nonres$SecondLargestPropertyUseTypeGFA, na.rm = TRUE)

building2016nonres$ThirdLargestPropertyUseTypeGFA[is.na(building2016nonres$ThirdLargestPropertyUseTypeGFA)] <- mean(building2016nonres$ThirdLargestPropertyUseTypeGFA, na.rm = TRUE)

building2016nonres$GFAPerBuilding[is.na(building2016nonres$GFAPerBuilding)] <- mean(building2016nonres$GFAPerBuilding, na.rm = TRUE)

building2016nonres$GFAPerFloor[is.na(building2016nonres$GFAPerFloor)] <- mean(building2016nonres$GFAPerFloor, na.rm = TRUE)

building2016nonres$EnergyUsePerBuilding[is.na(building2016nonres$EnergyUsePerBuilding)] <- mean(building2016nonres$EnergyUsePerBuilding, na.rm = TRUE)

building2016nonres$EnergyUsePerFloor[is.na(building2016nonres$EnergyUsePerFloor)] <- mean(building2016nonres$EnergyUsePerFloor, na.rm = TRUE)

building2016nonres$TotalGHGEmissionsPerFloor[is.na(building2016nonres$TotalGHGEmissionsPerFloor)] <- mean(building2016nonres$TotalGHGEmissionsPerFloor, na.rm = TRUE)

building2016nonres$GHGEmissionsIntensityPerFloor[is.na(building2016nonres$GHGEmissionsIntensityPerFloor)] <- mean(building2016nonres$GHGEmissionsIntensityPerFloor, na.rm = TRUE)

building2016nonres$NumberofBuildings[is.na(building2016nonres$NumberofBuildings)] <- mean(building2016nonres$NumberofBuildings, na.rm = TRUE)

building2016nonres$LargestPropertyUseTypeGFA[is.na(building2016nonres$LargestPropertyUseTypeGFA)] <- mean(building2016nonres$LargestPropertyUseTypeGFA, na.rm = TRUE)

building2016nonres$PropertyGFATotal[is.na(building2016nonres$PropertyGFATotal)] <- mean(building2016nonres$PropertyGFATotal, na.rm = TRUE)

building2016nonres$PropertyGFAParking[is.na(building2016nonres$PropertyGFAParking)] <- mean(building2016nonres$PropertyGFAParking, na.rm = TRUE)

building2016nonres$PropertyGFABuilding.s.[is.na(building2016nonres$PropertyGFABuilding.s.)] <- mean(building2016nonres$PropertyGFABuilding.s., na.rm = TRUE)

building2016nonres$SiteEUI.kBtu.sf.[is.na(building2016nonres$SiteEUI.kBtu.sf.)] <- mean(building2016nonres$SiteEUI.kBtu.sf., na.rm = TRUE)

building2016nonres$SiteEUIWN.kBtu.sf.[is.na(building2016nonres$SiteEUIWN.kBtu.sf.)] <- mean(building2016nonres$SiteEUIWN.kBtu.sf., na.rm = TRUE)

building2016nonres$SourceEUI.kBtu.sf.[is.na(building2016nonres$SourceEUI.kBtu.sf.)] <- mean(building2016nonres$SourceEUI.kBtu.sf., na.rm = TRUE)

building2016nonres$SourceEUIWN.kBtu.sf.[is.na(building2016nonres$SourceEUIWN.kBtu.sf.)] <- mean(building2016nonres$SourceEUIWN.kBtu.sf., na.rm = TRUE)

building2016nonres$SiteEnergyUse.kBtu.[is.na(building2016nonres$SiteEnergyUse.kBtu.)] <- mean(building2016nonres$SiteEnergyUse.kBtu., na.rm = TRUE)

building2016nonres$SiteEnergyUseWN.kBtu.[is.na(building2016nonres$SiteEnergyUseWN.kBtu.)] <- mean(building2016nonres$SiteEnergyUseWN.kBtu., na.rm = TRUE)

building2016nonres$SteamUse.kBtu.[is.na(building2016nonres$SteamUse.kBtu.)] <- mean(building2016nonres$SteamUse.kBtu., na.rm = TRUE)

building2016nonres$Electricity.kWh.[is.na(building2016nonres$Electricity.kWh.)] <- mean(building2016nonres$Electricity.kWh., na.rm = TRUE)

building2016nonres$Electricity.kBtu.[is.na(building2016nonres$Electricity.kBtu.)] <- mean(building2016nonres$Electricity.kBtu., na.rm = TRUE)

building2016nonres$NaturalGas.therms.[is.na(building2016nonres$NaturalGas.therms.)] <- mean(building2016nonres$NaturalGas.therms., na.rm = TRUE)

building2016nonres$NaturalGas.kBtu.[is.na(building2016nonres$NaturalGas.kBtu.)] <- mean(building2016nonres$NaturalGas.kBtu., na.rm = TRUE)

building2016nonres$TotalGHGEmissions[is.na(building2016nonres$TotalGHGEmissions)] <- mean(building2016nonres$TotalGHGEmissions, na.rm = TRUE)

building2016nonres$GHGEmissionsIntensity[is.na(building2016nonres$GHGEmissionsIntensity)] <- mean(building2016nonres$GHGEmissionsIntensity, na.rm = TRUE)

building2016nonres$GFAPerBuilding[is.na(building2016nonres$GFAPerBuilding)] <- mean(building2016nonres$GFAPerBuilding, na.rm = TRUE)

building2016nonres$GFAPerFloor[is.na(building2016nonres$GFAPerFloor)] <- mean(building2016nonres$GFAPerFloor, na.rm = TRUE)

building2016nonres$EnergyUsePerBuilding[is.na(building2016nonres$EnergyUsePerBuilding)] <- mean(building2016nonres$EnergyUsePerBuilding, na.rm = TRUE)

building2016nonres$EnergyUsePerFloor[is.na(building2016nonres$EnergyUsePerFloor)] <- mean(building2016nonres$EnergyUsePerFloor, na.rm = TRUE)

building2016nonres$TotalGHGEmissionsPerFloor[is.na(building2016nonres$TotalGHGEmissionsPerFloor)] <- mean(building2016nonres$TotalGHGEmissionsPerFloor, na.rm = TRUE)

building2016nonres$GHGEmissionsIntensityPerFloor[is.na(building2016nonres$GHGEmissionsIntensityPerFloor)] <- mean(building2016nonres$GHGEmissionsIntensityPerFloor, na.rm = TRUE)

sum(is.na(building2016nonres)) 

vis_miss(building2016nonres)

##### II Multivariate Analysis : #####

require(plyr)
require(dplyr)

#### A- Univariate Analysis : ####

### 1- Type of Habitations : ###

building2016 %>% 
  group_by(BuildingType) %>% 
  dplyr:: summarise(count = n()) %>% 
  ggplot(aes(x = reorder(BuildingType,(-count)), y = count)) + 
  geom_bar(stat = 'identity') +
  xlab("BuildingType") + ggtitle("Type of Habitations") 

### 2- Number Of Floors : ###

building2016 %>% 
  group_by(NumberofFloors) %>% 
  dplyr:: summarise(count = n()) %>% 
  ggplot(aes(x = reorder(NumberofFloors,(-count)), y = count)) + 
  geom_bar(stat = 'identity') +
  xlab("NumberofFloors") + ggtitle("Number Of Floors") 

#### B- Bivariate Analysis : ####

### 1- Number of Buildings / Building Type : ###

ggplot(building2016, aes(x=NumberofBuildings, y=BuildingType)) + geom_boxplot() + xlim(0,10)

### 2-  Number of Floors / Building Type : ###

ggplot(building2016, aes(x=NumberofFloors, y=BuildingType)) + geom_boxplot() + xlim(0,50)

### 3- GHGEmissionsIntensity / Building Type : ###

ggplot(building2016, aes(x=GHGEmissionsIntensity, y=BuildingType)) + geom_boxplot() + xlim(0,20)

### 4- TotalGHGEmissions / Building Type : ###

ggplot(building2016, aes(x=TotalGHGEmissions, y=BuildingType)) + geom_boxplot() + xlim(0,500)

### 5- SiteEnergyUseWN.kBtu. / Building Type : ###

building2016$SiteEnergyUseWN.kBtu. <- as.integer(building2016$SiteEnergyUseWN.kBtu.)

ggplot(building2016, aes(x= SiteEnergyUseWN.kBtu., y=BuildingType)) + geom_boxplot() + xlim(0,5000000)

### 6- SiteEnergyUse.kBtu. / Building Type : ###

building2016$SiteEnergyUse.kBtu. <- as.integer(building2016$SiteEnergyUse.kBtu.)

ggplot(building2016, aes(x= SiteEnergyUse.kBtu., y=BuildingType)) + geom_boxplot() + xlim(0,5000000)

#### C- Multivariate Analysis : ####

### Correlation Matrix : ###

corrdataset <- building2016nonres %>% select(NumberofFloors, NumberofBuildings, PropertyGFATotal, PropertyGFAParking, SiteEUI.kBtu.sf., SiteEnergyUse.kBtu., SiteEnergyUseWN.kBtu., TotalGHGEmissions, GHGEmissionsIntensity)

mcor <- cor(corrdataset, method = c("pearson", "kendall", "spearman"))

corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)

#### D- GEOLOCALISATION : ####

### 1- TOTALGHGEmissions : ###

mapCO2 <- building2016nonres %>% select(TotalGHGEmissions, Latitude, Longitude)

mapCO2

mapview(mapCO2, xcol = "Longitude", ycol = "Latitude", crs = 4269, grid = FALSE)

### 2- Energy : ###

mapENERGY <- building2016nonres %>% select(SiteEnergyUseWN.kBtu., Latitude, Longitude)

mapENERGY

mapview(mapENERGY, xcol = "Longitude", ycol = "Latitude",crs = 4269, grid = FALSE)

##### III MACHINE LEARNING : #####

#### A- XG BOOST REGRESSOR : #### 

### 1- TARGET : TotalGHGEmissions : ###

glimpse(building2016nonres)

# Exploration of the Data :

head(building2016nonres)

# Statistical Summary of the Data Columns :

summary(building2016nonres)

# Number of Lines and Columns of the Dataset ;

dim(building2016nonres)

# Train and Test Data :

parts = createDataPartition(building2016nonres$TotalGHGEmissions, p = .8, list = F)

train = building2016nonres[parts, ]

test = building2016nonres[-parts, ]

# Define Predictor and Response Variables in Training Set :

train_x = data.matrix(train[, c('NumberofBuildings', 'NumberofFloors', 'PropertyGFATotal', 'PropertyGFAParking', 'PropertyGFABuilding.s.', 'LargestPropertyUseTypeGFA', 'SecondLargestPropertyUseTypeGFA', 'ThirdLargestPropertyUseTypeGFA', 'SiteEUI.kBtu.sf.', 'SiteEUIWN.kBtu.sf.', 'SourceEUI.kBtu.sf.', 'SourceEUIWN.kBtu.sf.', 'SiteEnergyUse.kBtu.', 'SiteEnergyUseWN.kBtu.', 'SteamUse.kBtu.', 'Electricity.kWh.', 'Electricity.kBtu.', 'NaturalGas.therms.', 'NaturalGas.kBtu.')])  

train_y = train$TotalGHGEmissions

# Define Predictor and Response Variables in Testing Set :

test_x = data.matrix(test[, c('NumberofBuildings', 'NumberofFloors', 'PropertyGFATotal', 'PropertyGFAParking', 'PropertyGFABuilding.s.', 'LargestPropertyUseTypeGFA', 'SecondLargestPropertyUseTypeGFA', 'ThirdLargestPropertyUseTypeGFA', 'SiteEUI.kBtu.sf.', 'SiteEUIWN.kBtu.sf.', 'SourceEUI.kBtu.sf.', 'SourceEUIWN.kBtu.sf.', 'SiteEnergyUse.kBtu.', 'SiteEnergyUseWN.kBtu.', 'SteamUse.kBtu.', 'Electricity.kWh.', 'Electricity.kBtu.', 'NaturalGas.therms.', 'NaturalGas.kBtu.')])  

test_y = test$TotalGHGEmissions

# Define Final Training and Testing Sets :

xgb_train = xgb.DMatrix(data = train_x, label = train_y)

xgb_test = xgb.DMatrix(data = test_x, label = test_y)

# Define Watchlist :

watchlist = list(train=xgb_train, test=xgb_test)

# Fit XGBoost Model and display Training and Testing Data at each round :

model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 100)

# Define Final Model :

model_xgboost = xgboost(data = xgb_train, max.depth = 3, nrounds = 86, verbose = 0)

# Use Model to make Predictions on Test Data :

pred_y = predict(model_xgboost, xgb_test)

# Metrics :

# Find SST and SSE :
  
sst <- sum((test_y - mean(test_y))^2)

sse <- sum((pred_y - test_y)^2)

# Find R-Squared :

R_squared <- 1 - sse/sst

# MSE :

MSE = mean((test_y - pred_y)^2) 

# MAE :

MAE = caret::MAE(test_y, pred_y) 

# RMSE :

RMSE = caret::RMSE(test_y, pred_y) 

#  Results :

Metrics <- c("Model XGBOOST Regressor")

resultsTotalGHGEmissionsxgboost <- data.frame(Metrics, R_squared, MSE, MAE, RMSE)

resultsTotalGHGEmissionsxgboost

### 2- TARGET : SiteEnergyUseWN.kBtu. : ###

glimpse(building2016nonres)

# Exploration of the Data :

head(building2016nonres)

# Statistical Summary of the Data Columns :

summary(building2016nonres)

# Number of Lines and Columns of the Dataset ;

dim(building2016nonres)

# Train and Test Data :

parts = createDataPartition(building2016nonres$SiteEnergyUseWN.kBtu., p = .8, list = F)

train = building2016nonres[parts, ]

test = building2016nonres[-parts, ]

# Define Predictor and Response Variables in Training Set :

train_x = data.matrix(train[, c('NumberofBuildings', 'NumberofFloors', 'PropertyGFATotal', 'PropertyGFAParking', 'PropertyGFABuilding.s.', 'LargestPropertyUseTypeGFA', 'SecondLargestPropertyUseTypeGFA', 'ThirdLargestPropertyUseTypeGFA', 'SiteEUI.kBtu.sf.', 'SiteEUIWN.kBtu.sf.', 'SourceEUI.kBtu.sf.', 'SourceEUIWN.kBtu.sf.', 'SteamUse.kBtu.', 'Electricity.kWh.', 'Electricity.kBtu.', 'NaturalGas.therms.', 'NaturalGas.kBtu.', 'TotalGHGEmissions', 'GHGEmissionsIntensity')])

train_y = train$SiteEnergyUseWN.kBtu.

# Define Predictor and Response Variables in Testing Set :

test_x = data.matrix(test[, c('NumberofBuildings', 'NumberofFloors', 'PropertyGFATotal', 'PropertyGFAParking', 'PropertyGFABuilding.s.', 'LargestPropertyUseTypeGFA', 'SecondLargestPropertyUseTypeGFA', 'ThirdLargestPropertyUseTypeGFA', 'SiteEUI.kBtu.sf.', 'SiteEUIWN.kBtu.sf.', 'SourceEUI.kBtu.sf.', 'SourceEUIWN.kBtu.sf.', 'SteamUse.kBtu.', 'Electricity.kWh.', 'Electricity.kBtu.', 'NaturalGas.therms.', 'NaturalGas.kBtu.', 'TotalGHGEmissions', 'GHGEmissionsIntensity')])

test_y = test$SiteEnergyUseWN.kBtu.

# Define Final Training and Testing Sets :

xgb_train = xgb.DMatrix(data = train_x, label = train_y)

xgb_test = xgb.DMatrix(data = test_x, label = test_y)

# Define Watchlist :

watchlist = list(train=xgb_train, test=xgb_test)

# Fit XGBoost Model and display Training and Testing Data at each round :

model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 100)

# Define Final Model :

model_xgboost = xgboost(data = xgb_train, max.depth = 3, nrounds = 86, verbose = 0)

# Use Model to make Predictions on Test Data :

pred_y = predict(model_xgboost, xgb_test)

# Metrics :

# Find SST and SSE :

sst <- sum((test_y - mean(test_y))^2)

sse <- sum((pred_y - test_y)^2)

# Find R-Squared :

R_squared <- 1 - sse/sst

# MSE :

MSE = mean((test_y - pred_y)^2)

# MAE :

MAE = caret::MAE(test_y, pred_y) 

# RMSE :

RMSE = caret::RMSE(test_y, pred_y) 

#  Results :

Metrics <- c("Model XGBOOST Regressor")

resultsSiteEnergyUseWN.kBtu.xgboost <- data.frame(Metrics, R_squared, MSE, MAE, RMSE)

resultsSiteEnergyUseWN.kBtu.xgboost

#### B- RANDOM FOREST REGRESSOR : #### 

### 1- TARGET : TotalGHGEmissions : ###

glimpse(building2016nonres)

# Exploration of the Data :

head(building2016nonres)

# Statistical Summary of the Data Columns :

summary(building2016nonres)

# Number of Lines and Columns of the Dataset ;

dim(building2016nonres)

# Train and Test Data :

parts = createDataPartition(building2016nonres$TotalGHGEmissions, p = .8, list = F)

train = building2016nonres[parts, ]

test = building2016nonres[-parts, ]

# Define Predictor and Response Variables in Training Set :

train_x = data.matrix(train[, c('NumberofBuildings', 'NumberofFloors', 'PropertyGFATotal', 'PropertyGFAParking', 'PropertyGFABuilding.s.', 'LargestPropertyUseTypeGFA', 'SecondLargestPropertyUseTypeGFA', 'ThirdLargestPropertyUseTypeGFA', 'SiteEUI.kBtu.sf.', 'SiteEUIWN.kBtu.sf.', 'SourceEUI.kBtu.sf.', 'SourceEUIWN.kBtu.sf.', 'SiteEnergyUse.kBtu.', 'SiteEnergyUseWN.kBtu.', 'SteamUse.kBtu.', 'Electricity.kWh.', 'Electricity.kBtu.', 'NaturalGas.therms.', 'NaturalGas.kBtu.')])  

train_y = train$TotalGHGEmissions

# Define Predictor and Response Variables in Testing Set :

test_x = data.matrix(test[, c('NumberofBuildings', 'NumberofFloors', 'PropertyGFATotal', 'PropertyGFAParking', 'PropertyGFABuilding.s.', 'LargestPropertyUseTypeGFA', 'SecondLargestPropertyUseTypeGFA', 'ThirdLargestPropertyUseTypeGFA', 'SiteEUI.kBtu.sf.', 'SiteEUIWN.kBtu.sf.', 'SourceEUI.kBtu.sf.', 'SourceEUIWN.kBtu.sf.', 'SiteEnergyUse.kBtu.', 'SiteEnergyUseWN.kBtu.', 'SteamUse.kBtu.', 'Electricity.kWh.', 'Electricity.kBtu.', 'NaturalGas.therms.', 'NaturalGas.kBtu.')])  

test_y = test$TotalGHGEmissions

# Train the Model :

regr <- randomForest(x = train_x, y = train_y, maxnodes = 10, ntree = 10)

# Use Model to make Predictions on Test Data :

predictions <- predict(regr, test_x)

# Metrics :

# Find SST and SSE :

sst <- sum((test_y - mean(test_y))^2)

sse <- sum((predictions - test_y)^2)

# Find R-Squared :

R_squared <- 1 - sse/sst

# MSE :

MSE = mean((test_y - predictions)^2)

# MAE :

MAE = caret::MAE(test_y, predictions) 

# RMSE :

RMSE = caret::RMSE(test_y, predictions) 

# Results :

Metrics <- c("Model Random Forest Regressor")

resultsTotalGHGEmissionsrandomforest <- data.frame(Metrics, R_squared, MSE, MAE, RMSE)

resultsTotalGHGEmissionsrandomforest

### 2- TARGET : SiteEnergyUseWN.kBtu. : ###

glimpse(building2016nonres)

# Exploration of the Data :

head(building2016nonres)

# Statistical Summary of the Data Columns :

summary(building2016nonres)

# Number of Lines and Columns of the Dataset ;

dim(building2016nonres)

# Train and Test Data :

parts = createDataPartition(building2016nonres$SiteEnergyUseWN.kBtu., p = .8, list = F)

train = building2016nonres[parts, ]

test = building2016nonres[-parts, ]

# Define Predictor and Response Variables in Training Set :

train_x = data.matrix(train[, c('NumberofBuildings', 'NumberofFloors', 'PropertyGFATotal', 'PropertyGFAParking', 'PropertyGFABuilding.s.', 'LargestPropertyUseTypeGFA', 'SecondLargestPropertyUseTypeGFA', 'ThirdLargestPropertyUseTypeGFA', 'SiteEUI.kBtu.sf.', 'SiteEUIWN.kBtu.sf.', 'SourceEUI.kBtu.sf.', 'SourceEUIWN.kBtu.sf.', 'SteamUse.kBtu.', 'Electricity.kWh.', 'Electricity.kBtu.', 'NaturalGas.therms.', 'NaturalGas.kBtu.', 'TotalGHGEmissions', 'GHGEmissionsIntensity')])

train_y = train$SiteEnergyUseWN.kBtu.

# Define Predictor and Response Variables in Testing Set :

test_x = data.matrix(test[, c('NumberofBuildings', 'NumberofFloors', 'PropertyGFATotal', 'PropertyGFAParking', 'PropertyGFABuilding.s.', 'LargestPropertyUseTypeGFA', 'SecondLargestPropertyUseTypeGFA', 'ThirdLargestPropertyUseTypeGFA', 'SiteEUI.kBtu.sf.', 'SiteEUIWN.kBtu.sf.', 'SourceEUI.kBtu.sf.', 'SourceEUIWN.kBtu.sf.', 'SteamUse.kBtu.', 'Electricity.kWh.', 'Electricity.kBtu.', 'NaturalGas.therms.', 'NaturalGas.kBtu.', 'TotalGHGEmissions', 'GHGEmissionsIntensity')])

test_y = test$SiteEnergyUseWN.kBtu.

# Train the Model :

regr <- randomForest(x = train_x, y = train_y, maxnodes = 10, ntree = 10)

# Use Model to make Predictions on Test Data :

predictions <- predict(regr, test_x)

# Metrics :

# Find SST and SSE :

sst <- sum((test_y - mean(test_y))^2)

sse <- sum((predictions - test_y)^2)

# Find R-Squared :

R_squared <- 1 - sse/sst

# MSE :

MSE = mean((test_y - predictions)^2)

# MAE :

MAE = caret::MAE(test_y, predictions) 

# RMSE :

RMSE = caret::RMSE(test_y, predictions) 

# Results :

Metrics <- c("Model Random Forest Regressor")

resultsSiteEnergyUseWN.kBtu.randomforest <- data.frame(Metrics, R_squared, MSE, MAE, RMSE)

resultsSiteEnergyUseWN.kBtu.randomforest

##### IV RESULTS : #####

#### A- XG BOOST REGRESSOR : #### 

### 1- TARGET : TotalGHGEmissions : ###

resultsTotalGHGEmissionsxgboost

### 2- TARGET : SiteEnergyUseWN. : ###

resultsSiteEnergyUseWN.kBtu.xgboost

#### B- RANDOM FOREST REGRESSOR : #### 

### 1- TARGET : TotalGHGEmissions : ###

resultsTotalGHGEmissionsrandomforest

### 2- TARGET : SiteEnergyUseWN. : ###

resultsSiteEnergyUseWN.kBtu.randomforest
