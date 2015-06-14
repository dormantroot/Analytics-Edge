###############################################################
### Predicting Wine Quality using Linear Regression ###########
###############################################################
setwd("c:/Work/Analytics-Edge/Data")

# Read in data
wine = read.csv("wine.csv")
str(wine)
summary(wine)

# Linear Regression (one variable)
model1 = lm(Price ~ AGST, data=wine)
summary(model1)  # notice the multiple r-squared value? This is the r-squared for the model created using all of the independent variables. As you add more variables, the 'mulitple r-squared' term will increase. Notice, however, the 'adjusted R-squared' term? This term is always lower than  'multiple R-Squared' b/c it accounts for quality all of the independent variables used in the model. That is, if you add an independent variable that doesn't help the model then 'adjusted R-squared' value will decrease.

# Calculating SSE (Sum of Squared Errors)
model1$residuals
SSE = sum(model1$residuals^2)
SSE

# Adding a new variable to our model (two variables) - Linear Regression with multiple variables
model2 = lm(Price ~ AGST + HarvestRain, data=wine) # notice how adding the new variable increase the quality of our model? This is can clearly be seen with the increase in 'Mulitple R-Squared' and 'Adjusted R-Squared'
summary(model2)

# Calculated SSE (Sum of Squared Errors)
SSE = sum(model2$residuals^2)
SSE  # notice how the SSE is smaller than the previous model? This indicates that this model is better than the previous model.

# Linear Regression (all variables)
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(model3)

# Sum of Squared Errors
SSE = sum(model3$residuals^2) # This model is much better than the above two models. This is clearly seen from 'multiple r-squared' and 'adjusted r-squared' values
SSE


# Understanding relationship between the Model and the Coefficients. 
# Remove FrancePop
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)




# Correlations and colinearity
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)

# Remove Age and FrancePop
model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model5)



# predicting
# Read in test set
wineTest = read.csv("wine_test.csv")
str(wineTest)

# Make test set predictions
predictTest = predict(model4, newdata=wineTest)
predictTest

# Compute R-squared to quantify the accuracy of our model
SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
1 - SSE/SST



## Money Ball Prediction
# Read in data
baseball = read.csv("baseball.csv")
str(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# Scatterplot to check for linear relationship
plot(moneyball$RD, moneyball$W)

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)


str(moneyball)

# Regression model to predict runs scored
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)
