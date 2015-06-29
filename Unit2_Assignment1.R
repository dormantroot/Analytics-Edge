#######################################################
### Predicting Climate Change #########################
#######################################################
setwd("c:/Work/Analytics-Edge/Data")
#1.1
#First, read in the data and split it using the subset command:  
climate = read.csv("climate_change.csv")

train = subset(climate, Year <= 2006)
test = subset(climate, Year > 2006)

#Then, you can create the model using the command:  
climatelm = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=train)

#Lastly, look at the model using summary(climatelm). The Multiple R-squared value is 0.7509.

#1.2
# If you look at the model we created in the previous problem using summary(climatelm), all of the variables have at least one star except for CH4 and N2O. So MEI, CO2, CFC.11, CFC.12, TSI, and Aerosols are all significant.
summary(climatelm)


#2.1
# The linear correlation of N2O and CFC.11 with other variables in the data set is quite large. The first explanation does not seem correct, as the warming effect of nitrous oxide and CFC-11 are well documented, and our regression analysis is not enough to disprove it. The second explanation is unlikely, as we have estimated eight coefficients and the intercept from 284 observations.

#2.2
#You can calculate all correlations at once using cor(train) where train is the name of the training data set.

#3
# We can create this simplified model with the command:
# 
# You can get the coefficient for N2O and the model R-squared by typing summary(LinReg).
# 
# We have observed that, for this problem, when we remove many variables the sign of N2O flips. The model has not lost a lot of explanatory power (the model R2 is 0.7261 compared to 0.7509 previously) despite removing many variables. As discussed in lecture, this type of behavior is typical when building a model where many of the independent variables are highly correlated with each other. In this particular problem many of the variables (CO2, CH4, N2O, CFC.11 and CFC.12) are highly correlated, since they are all driven by human industrial development.
LinReg = lm(Temp ~ MEI + N2O + TSI + Aerosols, data=train)

#4
# You can create a model using the step function by typing:
#   
StepModel = step(climatelm)
summary(StepModel)
# 
# where "climateLM" is the name of the full model.
# 
# # If you look at the summary of the model with summary(StepModel), you can see that the R-squared value is 0.75, and only CH4 was
# R provides a function, step, that will automate the procedure of trying different combinations of variables to find a good compromise of model simplicity and R2. This trade-off is formalized by the Akaike information criterion (AIC) - it can be informally thought of as the quality of the model with a penalty for the number of variables in the model.removed.


#5
#The R code to calculate the R-squared can be written as follows (your variable names may be different):
  
tempPredict = predict(climateStep, newdata = test)

SSE = sum((tempPredict - test$Temp)^2)

SST = sum( (mean(train$Temp) - test$Temp)^2)

R2 = 1 - SSE/SST