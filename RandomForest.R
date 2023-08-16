#script for running random forest algorithm on ancestry data from entropy
# Written by Amanda Meuser -- Aug 2023

library(tidyverse)
library(stringr)
library(corrplot)
library(corrr)
library(caret)
library(randomForest)

#======================================================================
# Example 1 https://www.simplilearn.com/tutorials/data-science-tutorial/random-forest-in-r
#======================================================================

wine <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"), header = TRUE, sep = ";") # This command is used to load the dataset
head(wine) # Display the head and dimensions of wine dataset
dim(wine)
barplot(table(wine$quality)) # Barplot to see the quality of wines. The output looks like below

# Now, we have to convert the quality values into factors
wine$taste <- ifelse(wine$quality < 5, "bad", "good")
wine$taste[wine$quality == 5] <- "normal"
wine$taste[wine$quality == 6] <- "normal"
wine$taste <- as.factor(wine$taste)
str(wine$taste)
barplot(table(wine$taste)) # Barplot to view the taste of wines. The output is shown below.
table(wine$taste) 

# Next, we need to split the data into training and testing. 80% for training, 20% for testing.
set.seed(123)
samp <- sample(nrow(wine), 0.8 * nrow(wine))
train <- wine[samp, ]
test <- wine[-samp, ]

# Moving onto the Data visualization
#library(ggplot2)
ggplot(wine,aes(fixed.acidity,volatile.acidity))+ geom_point(aes(color=taste))# This command is used to display a scatter plot. The output looks like below
ggplot(wine,aes(alcohol)) + geom_histogram(aes(fill=taste),color='black',bins=50) # This command is used to display a stacked bar chart. The output looks like below


dim(train)
dim(test)  # Checks the dimensions of training and testing dataset
#install.packages('randomForest')
library(randomForest)           # Install the random forest library

# Now that we have installed the randomforest library, let's build the random forest model
model <- randomForest(taste ~ . - quality, data = train, ntree = 1000, mtry = 5)
model
model$confusion

# The next step is to validate our model using the test data
prediction <- predict(model, newdata = test)
table(prediction, test$taste)
prediction


# Now, let's display the predicted vs. the actual values
results<-cbind(prediction,test$taste)
results
colnames(results)<-c('pred','real')
results<-as.data.frame(results)
View(results)

# Finally, let's calculate the accuracy of the model
sum(prediction==test$taste) / nrow(test) # The output is as shown below

#======================================================================
# Example 2 https://www.guru99.com/r-random-forest-tutorial.html (doesn't work properly but i want to copy how they do cross validation)
#======================================================================

#library(dplyr)
data_train <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/train.csv")
glimpse(data_train)
data_test <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/test.csv") 
glimpse(data_test)

# set up cross validataion settings with caret package
library(caret)
library(e1071)

# Define the control
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

# build the model w default values
set.seed(1234)
# Run the model
rf_default <- train(Survived~., # Train a random forest model. Best model is chosen with the accuracy measure
                    data = data_train,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl) # Evaluate the model with a grid search of 10 folder
# Print the results
print(rf_default)

#======================================================================
# Example 3 https://www.edureka.co/blog/random-forest-classifier/
#======================================================================

data_train <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/train.csv")
data_test <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/test.csv") 

head(data_train)
head(data_test)
# the biggest difference b/w the two sets is that the training data has a column that says whether the passengers survived (data_train$Survived) but the testing set does not

# we want to see which variables are the best predictors for "Survived". We first can look at cross-tabs to see if there's a correlation between the variable of passenger class and of survival
table(data_train[,c('Survived', 'Pclass')])
# it looks like a lot of 1st class survived (1) while 3rd class died (0)

# compare distribution of each variable conditioned on survival
#install.packages("fields")
library(fields)
bplot.xy(data_train$Survived, data_train$Age)
# age is on the y and survival on the x. the boxes are almost exactly the same, so it seems age doesn't correspond well w survival. Apparently there's a lot of NAs too so we're not gonna include it

bplot.xy(data_train$Survived, data_train$Fare)
# no NAs in fare and looks like there's a slight difference so we'll include it


# Converting 'Survived' to a factor
train$Survived <- factor(train$Survived)
# Set a random seed
set.seed(51)
# Training using 'random forest' algorithm
model <- train(Survived ~ Pclass + Sex + SibSp +
                 Embarked + Parch + Fare, # Survived is a function of the variables we decided to include
               data = data_train, # Use the train data frame as the training data
               method = 'rf',# Use the 'random forest' algorithm
               trControl = trainControl(method = 'cv', # Use cross-validation
                                        number = 10,
                                        search = "grid")) # Use 5 folds for cross-validation
model

# lets check if there's any missing data in the variables of the testing set
summary(data_test)
# fare has 1 NA so we'll fix it
data_test$Fare <- ifelse(is.na(data_test$Fare), mean(data_test$Fare, na.rm = TRUE), data_test$Fare)

# make predictions on the test set
data_test$Survived <- predict(model, newdata = data_test)
data_test$Survived # this should be 0s and 1s so not sure what's going on...

#======================================================================
# Parentals, 2-sp. hybrids, and multi-sp. hybrids (might focus this to hybrids vs parentals...)
#======================================================================

#import site data and metadata
site_data <- read.csv("../Leuciscid_OFAT_May2023.csv")
colnames(site_data)[1:2] <- c("Waterbody", "Waterbody_Code")
metadata <- read.csv("../Leuciscid_Metadata_May2023.csv")

#edit site_data and metadata
site_data <- site_data[1:30,] #remove blank rows
site_data <- site_data[-15,] #remove Costello upstream (only one fish there and sites are <1km apart)
site_data$Waterbody[site_data$Waterbody_Code == "COS"] <- "Costello_Creek"
metadata$Waterbody[metadata$Waterbody_Code == "OP2"] <- "Costello_Creek"
metadata$Waterbody[metadata$Waterbody_Code == "OP7"] <- "Costello_Creek"
metadata$Waterbody[metadata$Waterbody_Code == "Weir DS"] <- "Costello_Creek"
metadata$Waterbody_Code[metadata$Waterbody == "Costello_Creek"] <- "COS"
metadata <- metadata[!(metadata$Waterbody_Code == "CC****"),]

# import data on hybrids
all_fish <- read.csv("../entropy_target_old/old_AMP22_genoID_multispecies_k12_Jun2023.csv")

# simplify hybrid type and plot
all_fish$Hybrid_Type_Simple = NA

for (i in 1:nrow(all_fish)){
 if(str_detect(all_fish[i,15],'x') == TRUE ){
    all_fish$Hybrid_Type_Simple[i] <- "Hybrid" 
  }
  else if(str_detect(all_fish[i,15],'x') == FALSE){
    all_fish$Hybrid_Type_Simple[i] <- "Parental"
  }
}

# create counts of hybrid types
all_fish_details <- merge(all_fish, metadata, by = c("Mandeville_ID", "Common_Name"))
fish_count <- all_fish_details %>% group_by(Waterbody_Code) %>% count(Hybrid_Type_Simple)

# create frequency column
sums <- fish_count %>% 
  group_by(Waterbody_Code) %>%
  summarise(n_sum = sum(n),
            .groups = 'drop')
fish_count <- merge(fish_count, sums, by='Waterbody_Code')
getproportion <- function(row) {
  return (row$n/row$n_sum)
}
fish_count <- fish_count %>% mutate(proportion = n / n_sum)


# expand data to have proportion columns for parentals and hybrids w 1 row per water body
fish_wide <- fish_count[,-(3:4)]
fish_wide <- fish_wide %>% pivot_wider(names_from = Hybrid_Type_Simple, values_from = proportion)


# merge w site data
site_fish_data <- merge(fish_wide, site_data, by = "Waterbody_Code")

#remove variables that I don't need (waterbody name, lab name, and notes, variables in km2)
site_fish_data <- site_fish_data[-c(4:5,23:38,60)]

#create testing and training sets
set.seed(5)
samp <- sample(nrow(site_fish_data), 0.8 * nrow(site_fish_data))
fish_train <- site_fish_data[samp, ]
fish_test <- site_fish_data[-samp, ]

# remove the proportion of hybrids and parentals from the testing data
fish_test_real_response <- fish_test[c(2:3)]
fish_test <- fish_test[-c(2:3)]
# remove proportion of parentals from training data (we can infer proportion of parentals from prop of hybrids)
fish_train <- fish_train[-3]


# convert Hybrid to a factor
#fish_train$Hybrid <- factor(fish_train$Hybrid)

variables <- colnames(fish_train)
#write.csv(variables, "random_forest_variables.csv", quote = F, row.names = F)

# set this 
trControl = trainControl(method = "cv",
                         number = 10,
                         search = "grid") # Use 10 folds and grid search for cross-validation

# TRAIN THE MODEL!!!!!!!!!! (can i use a . to replace all the variable names?)
set.seed(12)
default_model <- train(Hybrid ~ Lat + Long + Drainage_area_km2 + Shape_factor + Length_main_channel_km + Max_channel_elevation_m + Min_channel_elevation_m + Slope_main_channel_m_km + Slope_main_channel_percent + Area_lakes_divide_wetlands_km2 + Area_lakes_km2 + Area_wetlands_km2 + Mean_elevation_m + Max_elevation_m + Mean_slope_percent + Annual_mean_temp_C + Annual_precipitation_mm + Clear_Open_Water_percent + Marsh_percent + Swamp_percent + Fen_percent + Bog_percent + Sparse_Treed_percent + Treed_Upland_percent + Deciduous_Treed_percent + Mixed_Treed_percent + Coniferous_Treed_percent + Plantations_Treed_Cultivated_percent + Hedge_Rows_percent + Tallgrass_Woodland_percent + Sand_Gravel_Mine_Tailings_Extraction_percent + Community_Infrastructure_percent + Agriculture_and_Undifferentiated_Rural_Land_Use_percent + Total_agriculture_percent + Total_urban_and_development_percent + Total_treed_percent + Total_wetland_percent + Open_water_percent, # Hybrid is a function of the variables we decided to include
               data = fish_train, # Use the train data frame as the training data
               method = 'rf',# Use the 'random forest' algorithm
               trControl = trControl)



default_model # best value of mtry is 2: "mtry" is a hyper-parameter of the random forest model that determines how many variables the model uses to split the trees
# i can make the model try all mtry values rather than just 2, 20 and 38, and have it tell me which is best
set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 38))
rf_mtry <- train(Hybrid ~ Lat + Long + Drainage_area_km2 + Shape_factor + Length_main_channel_km + Max_channel_elevation_m + Min_channel_elevation_m + Slope_main_channel_m_km + Slope_main_channel_percent + Area_lakes_divide_wetlands_km2 + Area_lakes_km2 + Area_wetlands_km2 + Mean_elevation_m + Max_elevation_m + Mean_slope_percent + Annual_mean_temp_C + Annual_precipitation_mm + Clear_Open_Water_percent + Marsh_percent + Swamp_percent + Fen_percent + Bog_percent + Sparse_Treed_percent + Treed_Upland_percent + Deciduous_Treed_percent + Mixed_Treed_percent + Coniferous_Treed_percent + Plantations_Treed_Cultivated_percent + Hedge_Rows_percent + Tallgrass_Woodland_percent + Sand_Gravel_Mine_Tailings_Extraction_percent + Community_Infrastructure_percent + Agriculture_and_Undifferentiated_Rural_Land_Use_percent + Total_agriculture_percent + Total_urban_and_development_percent + Total_treed_percent + Total_wetland_percent + Open_water_percent,
                 data = fish_train,
                 method = "rf",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)
print(rf_mtry) # best is actually 1 this time
best_mtry <- rf_mtry$bestTune$mtry  # that value is stored here

# now, i'm gonna optimize the value of maxnodes
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(3:37)) { # try 3 to 37 nodes
  set.seed(1234)
  rf_maxnode <- train(Hybrid ~ Lat + Long + Drainage_area_km2 + Shape_factor + Length_main_channel_km + Max_channel_elevation_m + Min_channel_elevation_m + Slope_main_channel_m_km + Slope_main_channel_percent + Area_lakes_divide_wetlands_km2 + Area_lakes_km2 + Area_wetlands_km2 + Mean_elevation_m + Max_elevation_m + Mean_slope_percent + Annual_mean_temp_C + Annual_precipitation_mm + Clear_Open_Water_percent + Marsh_percent + Swamp_percent + Fen_percent + Bog_percent + Sparse_Treed_percent + Treed_Upland_percent + Deciduous_Treed_percent + Mixed_Treed_percent + Coniferous_Treed_percent + Plantations_Treed_Cultivated_percent + Hedge_Rows_percent + Tallgrass_Woodland_percent + Sand_Gravel_Mine_Tailings_Extraction_percent + Community_Infrastructure_percent + Agriculture_and_Undifferentiated_Rural_Land_Use_percent + Total_agriculture_percent + Total_urban_and_development_percent + Total_treed_percent + Total_wetland_percent + Open_water_percent,
                      data = fish_train,
                      method = "rf",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry) # everything between 5 and 37 have the same RMSE so i guess it doesn't really matter how many nodes? i guess i'll just go with 5 then, for simplicity's sake

# now try to optimize number of trees
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(Hybrid ~ Lat + Long + Drainage_area_km2 + Shape_factor + Length_main_channel_km + Max_channel_elevation_m + Min_channel_elevation_m + Slope_main_channel_m_km + Slope_main_channel_percent + Area_lakes_divide_wetlands_km2 + Area_lakes_km2 + Area_wetlands_km2 + Mean_elevation_m + Max_elevation_m + Mean_slope_percent + Annual_mean_temp_C + Annual_precipitation_mm + Clear_Open_Water_percent + Marsh_percent + Swamp_percent + Fen_percent + Bog_percent + Sparse_Treed_percent + Treed_Upland_percent + Deciduous_Treed_percent + Mixed_Treed_percent + Coniferous_Treed_percent + Plantations_Treed_Cultivated_percent + Hedge_Rows_percent + Tallgrass_Woodland_percent + Sand_Gravel_Mine_Tailings_Extraction_percent + Community_Infrastructure_percent + Agriculture_and_Undifferentiated_Rural_Land_Use_percent + Total_agriculture_percent + Total_urban_and_development_percent + Total_treed_percent + Total_wetland_percent + Open_water_percent,
                       data = fish_train,
                       method = "rf",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 5,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree) # best value (when looking at max RMSE) seems to be 1000

best_rf <- train(Hybrid ~ Lat + Long + Drainage_area_km2 + Shape_factor + Length_main_channel_km + Max_channel_elevation_m + Min_channel_elevation_m + Slope_main_channel_m_km + Slope_main_channel_percent + Area_lakes_divide_wetlands_km2 + Area_lakes_km2 + Area_wetlands_km2 + Mean_elevation_m + Max_elevation_m + Mean_slope_percent + Annual_mean_temp_C + Annual_precipitation_mm + Clear_Open_Water_percent + Marsh_percent + Swamp_percent + Fen_percent + Bog_percent + Sparse_Treed_percent + Treed_Upland_percent + Deciduous_Treed_percent + Mixed_Treed_percent + Coniferous_Treed_percent + Plantations_Treed_Cultivated_percent + Hedge_Rows_percent + Tallgrass_Woodland_percent + Sand_Gravel_Mine_Tailings_Extraction_percent + Community_Infrastructure_percent + Agriculture_and_Undifferentiated_Rural_Land_Use_percent + Total_agriculture_percent + Total_urban_and_development_percent + Total_treed_percent + Total_wetland_percent + Open_water_percent,
                 data = fish_train,
                method = "rf",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 14,
                ntree = 1000,
                maxnodes = 5)

best_rf <- randomForest(Hybrid ~ Lat + Long + Drainage_area_km2 + Shape_factor + Length_main_channel_km + Max_channel_elevation_m + Min_channel_elevation_m + Slope_main_channel_m_km + Slope_main_channel_percent + Area_lakes_divide_wetlands_km2 + Area_lakes_km2 + Area_wetlands_km2 + Mean_elevation_m + Max_elevation_m + Mean_slope_percent + Annual_mean_temp_C + Annual_precipitation_mm + Clear_Open_Water_percent + Marsh_percent + Swamp_percent + Fen_percent + Bog_percent + Sparse_Treed_percent + Treed_Upland_percent + Deciduous_Treed_percent + Mixed_Treed_percent + Coniferous_Treed_percent + Plantations_Treed_Cultivated_percent + Hedge_Rows_percent + Tallgrass_Woodland_percent + Sand_Gravel_Mine_Tailings_Extraction_percent + Community_Infrastructure_percent + Agriculture_and_Undifferentiated_Rural_Land_Use_percent + Total_agriculture_percent + Total_urban_and_development_percent + Total_treed_percent + Total_wetland_percent + Open_water_percent,
                 data = fish_train,
                 method = "rf",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 1000,
                 maxnodes = 5)

prediction <- predict(best_rf, fish_test)
confusionMatrix(prediction, fish_test) #not working, idk how it's meant to



fish_test$Hybrid <- predict(best_rf, newdata = fish_test)
fish_test$Hybrid
fish_test_real_response$Hybrid

# merge real response back into test dataframe
fish_test$Hybrid_real <- fish_test_real_response$Hybrid

# calculate the mean difference in prediction vs reality
fish_test$Diff <- abs(fish_test$Hybrid - fish_test$Hybrid_real)
mean(fish_test$Diff) # 16% ish

varImpPlot(best_rf, main = "Importance of variables", cex = 0.55)

#save as a PDF
pdf("Random_forest_importance_39_variables.pdf", width = 24, height = 12)
varImpPlot(best_rf, main = "Importance of variables", cex = 1, lcolor = "violet", pch = 19)
dev.off()

varImp(best_rf) # not displaying in order - also why does example show % ?
?varImp()

# creating a matrix between the variables to see if any are linked. i can remove linked ones to cut down on the number of variables
cor <- cor((site_fish_data[-1]), method = c("pearson"))
#plot
(corrplot <- corrplot(cor, method="color", type = "lower", tl.cex = 0.75))
#pdf("correlogram_random_forest_variables.pdf")
#corrplot
#dev.off()
#print 25 most correlated variables
cor %>%
  correlate() %>% 
  stretch() %>% 
  arrange(r) %>%
  print(n=25)


# try running recursive feature elimination to reduce variable correlation

# Define the control using a random forest selection function
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

#create testing and training sets (i need the response variable in it's own data frame)
set.seed(500)
samp <- sample(nrow(site_fish_data), 0.8 * nrow(site_fish_data))
x_train <- site_fish_data[samp, ]
x_test <- site_fish_data[-samp, ]

# copy and remove the proportion of hybrids and parentals from the testing and training data, as well as site names
y_train <- as.matrix(x_train[2])
y_test <- as.matrix(x_test[2])

x_train <- x_train[-c(1,3)]
x_test <- x_test[-(1:3)]



# Run RFE
result_rfe1 <- rfe(x = x_train, 
                   y = y_train, 
                   sizes = c(1:39),
                   rfeControl = control)

# Print the results
result_rfe1

# Print the results visually
(RFE_MSE_variables <- ggplot(data = result_rfe1, metric = "RMSE") + theme_bw()) # best is 19 variables!

# Print the selected features
(variables_new <- predictors(result_rfe1))
#write.csv(variables_new, "random_forest_variables_RFE.csv", row.names = F)

varimp_data <- data.frame(feature = row.names(varImp(result_rfe1))[1:19],
                          importance = varImp(result_rfe1)[1:19, 1])

(RFE_variables_importance <- ggplot(data = varimp_data, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)))

pdf("RFE_variables.pdf", width = 12, height = 8)
RFE_MSE_variables
RFE_variables_importance
dev.off()




# Rerun the model with only the chosen variables!
set.seed(5648)
default_model <- train(Hybrid ~ Lat + Annual_mean_temp_C + Total_treed_percent + Annual_precipitation_mm + Deciduous_Treed_percent + Total_wetland_percent + Community_Infrastructure_percent + Total_urban_and_development_percent + Mixed_Treed_percent + Max_channel_elevation_m + Max_elevation_m + Treed_Upland_percent + Agriculture_and_Undifferentiated_Rural_Land_Use_percent + Mean_elevation_m + Total_agriculture_percent + Marsh_percent + Hedge_Rows_percent + Coniferous_Treed_percent + Swamp_percent, # Hybrid is a function of the variables we decided to include
                       data = fish_train, # Use the train data frame as the training data
                       method = 'rf',# Use the 'random forest' algorithm
                       trControl = trControl)



default_model # best value of mtry is 19

set.seed(879)
tuneGrid <- expand.grid(.mtry = c(1: 19))
rf_mtry <- train(Hybrid ~ Lat + Annual_mean_temp_C + Total_treed_percent + Annual_precipitation_mm + Deciduous_Treed_percent + Total_wetland_percent + Community_Infrastructure_percent + Total_urban_and_development_percent + Mixed_Treed_percent + Max_channel_elevation_m + Max_elevation_m + Treed_Upland_percent + Agriculture_and_Undifferentiated_Rural_Land_Use_percent + Mean_elevation_m + Total_agriculture_percent + Marsh_percent + Hedge_Rows_percent + Coniferous_Treed_percent + Swamp_percent,
                 data = fish_train,
                 method = "rf",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)
print(rf_mtry) # best is still 1 
best_mtry <- rf_mtry$bestTune$mtry  # that value is stored here

# now, i'm gonna optimize the value of maxnodes
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(2:18)) { # try 3 to 37 nodes
  set.seed(1234)
  rf_maxnode <- train(Hybrid ~ Lat + Annual_mean_temp_C + Total_treed_percent + Annual_precipitation_mm + Deciduous_Treed_percent + Total_wetland_percent + Community_Infrastructure_percent + Total_urban_and_development_percent + Mixed_Treed_percent + Max_channel_elevation_m + Max_elevation_m + Treed_Upland_percent + Agriculture_and_Undifferentiated_Rural_Land_Use_percent + Mean_elevation_m + Total_agriculture_percent + Marsh_percent + Hedge_Rows_percent + Coniferous_Treed_percent + Swamp_percent,
                      data = fish_train,
                      method = "rf",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry) # again everything between 5 and 19 have the same RMSE so i guess it doesn't really matter how many nodes? i guess i'll just go with 5 then, for simplicity's sake

# now try to optimize number of trees
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000, 2500, 3000)) {
  set.seed(5678)
  rf_maxtrees <- train(Hybrid ~ Lat + Annual_mean_temp_C + Total_treed_percent + Annual_precipitation_mm + Deciduous_Treed_percent + Total_wetland_percent + Community_Infrastructure_percent + Total_urban_and_development_percent + Mixed_Treed_percent + Max_channel_elevation_m + Max_elevation_m + Treed_Upland_percent + Agriculture_and_Undifferentiated_Rural_Land_Use_percent + Mean_elevation_m + Total_agriculture_percent + Marsh_percent + Hedge_Rows_percent + Coniferous_Treed_percent + Swamp_percent,
                       data = fish_train,
                       method = "rf",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 5,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree) # best value (when looking at max RMSE) seems to be 2000

best_rf <- randomForest(Hybrid ~ Lat + Annual_mean_temp_C + Total_treed_percent + Annual_precipitation_mm + Deciduous_Treed_percent + Total_wetland_percent + Community_Infrastructure_percent + Total_urban_and_development_percent + Mixed_Treed_percent + Max_channel_elevation_m + Max_elevation_m + Treed_Upland_percent + Agriculture_and_Undifferentiated_Rural_Land_Use_percent + Mean_elevation_m + Total_agriculture_percent + Marsh_percent + Hedge_Rows_percent + Coniferous_Treed_percent + Swamp_percent,
                 data = fish_train,
                 method = "rf",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 2000,
                 maxnodes = 5)

prediction <- predict(best_rf, fish_test)
confusionMatrix(prediction, fish_test) #not working, idk how it's meant to



fish_test$Hybrid <- predict(best_rf, newdata = fish_test)
fish_test$Hybrid
fish_test_real_response$Hybrid

# merge real response back into test dataframe
fish_test$Hybrid_real <- fish_test_real_response$Hybrid

# calculate the mean difference in prediction vs reality
fish_test$Diff <- abs(fish_test$Hybrid - fish_test$Hybrid_real)
mean(fish_test$Diff) # 16% ish

varImpPlot(best_rf, main = "Importance of variables", cex = 0.55)

#save as a PDF
pdf("Random_forest_importance_19_variables.pdf", width = 24, height = 12)
varImpPlot(best_rf, main = "Importance of variables", cex = 1, lcolor = "violet", pch = 19)
dev.off()

varImp(best_rf) # not displaying in order - also why does example show % ?
?varImp()

#======================================================================
# Example 4 (RFE) https://towardsdatascience.com/effective-feature-selection-recursive-feature-elimination-using-r-148ff998e4f7
#======================================================================

library("dplyr")
library("faux")
library("DataExplorer")
library("caret")
library("randomForest")

# Import the dataset
file = "https://raw.githubusercontent.com/okanbulut/tds/main/feature_selection_rfe/heart.csv"
data <- read.csv(file, header = TRUE)
head(data)

# Set the seed for reproducibility
set.seed(2021)

# Add four pseudo variables into the data
data <- mutate(data,
               # random categorical variable
               catvar = as.factor(sample(sample(letters[1:3], nrow(data), replace = TRUE))),
               
               # random continuous variable (mean = 10, sd = 2, r = 0)
               contvar1 = rnorm(nrow(data), mean = 10, sd = 2),
               
               # continuous variable with low correlation (mean = 10, sd = 2, r = 0.2)
               contvar2 = rnorm_pre(data$target, mu = 10, sd = 2, r = 0.2, empirical = TRUE),
               
               # continuous variable with moderate correlation (mean = 10, sd = 2, r = 0.5)
               contvar3 = rnorm_pre(data$target, mu = 10, sd = 2, r = 0.5, empirical = TRUE))

data <- data %>%
  # Save categorical features as factors
  mutate_at(c("sex", "cp", "fbs", "restecg", "exang", "slope", "thal", "target", "catvar"), 
            as.factor) %>%
  # Center and scale numeric features
  mutate_if(is.numeric, scale)

# Features
x <- data %>%
  select(-target, -catvar, -contvar1, -contvar2, -contvar3) %>%
  as.data.frame()

# Target variable
y <- data$target

# Training: 80%; Test: 20%
set.seed(2021)
inTrain <- createDataPartition(y, p = .80, list = FALSE)[,1]

x_train <- x[ inTrain, ]
x_test  <- x[-inTrain, ]

y_train <- y[ inTrain]
y_test  <- y[-inTrain]

#======================================================================
# Big 3 hybrids (F1s, F2s, BC1s, etc)
#======================================================================

BNDxCC_hybrids <- read.csv("AMP22_hybrid_types_persite_BNDxCC.csv")
BNDxCS_hybrids <- read.csv("AMP22_hybrid_types_persite_BNDxCS.csv")
CSxCC_hybrids <- read.csv("AMP22_hybrid_types_persite_CSxCC.csv")
