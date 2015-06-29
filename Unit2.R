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



###############################################################
######################## Recitation ###########################
###############################################################
# Read in the data
NBA = read.csv("NBA_train.csv")
str(NBA)

# How many wins to make the playoffs?
table(NBA$W, NBA$Playoffs)

# Compute Points Difference
NBA$PTSdiff = NBA$PTS - NBA$oppPTS

# Check for linear relationship
plot(NBA$PTSdiff, NBA$W)

# Linear regression model for wins
WinsReg = lm(W ~ PTSdiff, data=NBA)
summary(WinsReg)

# Linear regression model for points scored
PointsReg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data=NBA)
summary(PointsReg)

# verify how good the model is, using the following
# Sum of Squared Errors
PointsReg$residuals
SSE = sum(PointsReg$residuals^2)
SSE

# Root mean squared error
RMSE = sqrt(SSE/nrow(NBA))
RMSE

# Average number of points in a season
mean(NBA$PTS) # as you can see the RMSE is 184.40, but the average pts in a season is 8370. So considering that value, the RMSE is not that bad. However, there is still room for improvement.



# Remove insignifcant variables
summary(PointsReg)

# remove variable 'TOV' b/c of its pvalue
PointsReg2 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data=NBA)
summary(PointsReg2)

PointsReg3 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data=NBA)
summary(PointsReg3)

PointsReg4 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data=NBA)
summary(PointsReg4)

# Compute SSE and RMSE for new model
SSE_4 = sum(PointsReg4$residuals^2)
RMSE_4 = sqrt(SSE_4/nrow(NBA))
SSE_4
RMSE_4   # as you can see the new RMSE is 184.493, which is very close to model PointsReg. However, this model has less amount of variable and is more interpretable.



# now, let's use the above model 'PointsReg4' to predict
# Read in test set
NBA_test = read.csv("NBA_test.csv")

# Make predictions on test set
PointsPredictions = predict(PointsReg4, newdata=NBA_test)

# well, we have the prediction - PointsPredictions. But how good is it? For that we,
# Compute out-of-sample R^2
SSE = sum((PointsPredictions - NBA_test$PTS)^2)
SST = sum((mean(NBA$PTS) - NBA_test$PTS)^2)
R2 = 1 - SSE/SST
R2  # so the R2 is 0.8127142, which not very close to 1, but OK.

# Compute the RMSE
RMSE = sqrt(SSE/nrow(NBA_test))
RMSE  #196.3723 is higher than what we got before '184.483'. But it is still not bad.