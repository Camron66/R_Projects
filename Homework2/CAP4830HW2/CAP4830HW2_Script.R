#=============================================================================
# PROGRAMMER: Camron Cisneros
# PANTHER ID: 6187231
#
# CLASS: CAP4830
# SECTION: RVC
# SEMESTER: Spring 2023
# CLASSTIME: N/A
# CERTIFICATION: 
#           I understand FIU’s academic policies, and I certify that this 
#           work is my own and that none of it is the work of any other person.

#=============================================================================


    ###########  1  ############

#install.packages("xlsx") # Install Reader
library(xlsx)

rm(list = ls())

#Read Data
modelData <- read.xlsx("CAP4830_HW2_Data.xlsx", sheetIndex = 1)


    ###########  2  ############

#Output Names
names(modelData)


    ###########  3  ############

#linear regression model
model1 <- lm(UNRATE_PCH ~ DFII10_PCH + CPILFESL_PCH + XTEITT01CNM156S_PCH +
               DCOILWTICO_PCH + PCOPPUSDM_PCH + PCE_PCH + WPU101_PCH + GPDIC1_PCH +
               RRVRUSQ156N_PCH, data = modelData)
summary(model1)


    ###########  4  ############

#  (Intercept)         0.006960 ** 
#  DFII10_PCH          1.83e-06 ***
#  XTEITT01CNM156S_PCH 0.000280 ***
#  DCOILWTICO_PCH      0.000709 ***
#  PCE_PCH             2.91e-09 ***
  


    ###########  5  ############

#Density Plot
plot(density(model1$residuals), main= "Residual Density Plot") #Plot residual density


    ###########  6  ############

#Shapiro Test
shapiro.test(model1$residuals)


    ###########  7  ############

#model2 with only pvalues < .55
model2 <- lm(formula = UNRATE_PCH ~ DFII10_PCH + XTEITT01CNM156S_PCH + 
               DCOILWTICO_PCH + PCOPPUSDM_PCH + PCE_PCH + WPU101_PCH +
               GPDIC1_PCH, data = modelData)

summary(model2)


    ###########  8  ############

#Adjusted R-Squared(model1) = .8217              sqrt(.8217) = .90647
#Adjusted R-Squared(model2) = .8266              sqrt(.8293) = .90918
#         0. 8266 - 0.8217 = 0.0049               .90918- .90647 = .00271 


    ##########  9  #############

# First, split the data into training and test sets
set.seed(100) # for reproducibility
trainIndex <- sample(nrow(modelData), floor(0.8*nrow(modelData)))
trainData <- modelData[trainIndex, ]
testData <- modelData[-trainIndex, ]


# Train the model using the training data
trainModel <- lm(UNRATE_PCH ~ DFII10_PCH + XTEITT01CNM156S_PCH + DCOILWTICO_PCH + PCE_PCH, data = trainData)

# Predict the response variable using the test data
predicted <- predict(trainModel,testData)

actuals_preds <- data.frame(cbind(index = seq(1: nrow(testData)), 
                                  actuals= testData$UNRATE_PCH,
                                  predicteds=predicted))  
# Calculate prediction accuracy metrics
accuracy <- cor(actuals_preds$actuals,actuals_preds$predicteds)  
error_rate <- 1 - cor(actuals_preds$predicteds,actuals_preds$actuals)

# Print the results
cat("Accuracy:", round(accuracy*100, 2), "%\n")
cat("Error rate:", round(error_rate*100, 2), "%\n")


    ##########  10  ############

model3 <- lm(UNRATE_PCH ~ DFII10_PCH + XTEITT01CNM156S_PCH
             + PCE_PCH, data = modelData)
summary(model3)


    ##########  11  ############

# row indices for training data
trainingIndex <- sample(1:nrow(modelData), 0.6*nrow(modelData)) 
trainingData <- modelData[trainingIndex, ]
testingData <- modelData[-trainingIndex, ]

model4 <- lm(UNRATE_PCH ~ DFII10_PCH + CPILFESL_PCH + XTEITT01CNM156S_PCH +
               DCOILWTICO_PCH + PCOPPUSDM_PCH + PCE_PCH + WPU101_PCH + GPDIC1_PCH +
               RRVRUSQ156N_PCH, data = trainingData)
   
summary(model4)
  

    ##########  12  ############

# make predictions on the testing data
distPred <- predict(model4, testingData)
head(model4)


    ##########  13  ############


# calculate the accuracy and error rates
actuals_predsModel4 <- data.frame(cbind(index = seq(1: nrow(testingData)), 
                                  actuals= testingData$UNRATE_PCH,
                                  predicteds=distPred))  
accuracyModel4 <-  cor(actuals_predsModel4$actuals,actuals_predsModel4$predicteds)
errorRateModel4 <- 1 - accuracyModel4

# create plot
library(ggplot2)
gg <- ggplot(data = actuals_predsModel4, aes(index))  + 
  geom_point(aes(y = actuals), color = "red") + 
  geom_point(aes(y = predicteds), color = "blue") +
  labs( title = "Actual vs Predicted Values")
gg


    ##########  14  ###########

#install.packages('caret')
library(caret)

controlled <- trainControl(method = "cv", number = 10)
k10model <- train(UNRATE_PCH ~ DFII10_PCH + XTEITT01CNM156S_PCH + 
                    DCOILWTICO_PCH + PCOPPUSDM_PCH + PCE_PCH + WPU101_PCH +
                    GPDIC1_PCH, data = modelData, method = "lm", trControl = controlled)
names(k10model)
print(k10model)

