#######################################################
############## DETECTING FLU EPIDEMICS VIA SEARCH ENGINE QUERY DATA ####################
#######################################################
setwd("c:/Work/Analytics-Edge/Data")
mvt = read.csv("FluTrain.csv")

#1.2
hist(FluTrain$ILI)


#1.3
plot(FluTrain$Queries, log(FluTrain$ILI))

#2.2
FluTrend1 = lm(log(ILI)~Queries, data=FluTrain)
summary(FluTrend1)


#2.3
# To test these hypotheses, we first need to compute the correlation between the independent variable used in the model (Queries) and the dependent variable (log(ILI)). This can be done with
# 
# Correlation = cor(FluTrain$Queries, log(FluTrain$ILI))
# 
# The values of the three expressions are then:
#   
#   Correlation^2 = 0.7090201
# 
# log(1/Correlation) = 0.1719357
# 
# exp(-0.5*Correlation) = 0.6563792
# 
# It appears that Correlation^2 is equal to the R-squared value. It can be proved that this is always the case.

#3.1
# To obtain the predictions, we need can run
# 
# PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
# 
# Next, we need to determine which element in the test set is for March 11, 2012. We can determine this with:
#   
#   which(FluTest$Week == "2012-03-11 - 2012-03-17")
# 
# Now we know we are looking for prediction number 11. This can be accessed with:
#   

#4.1
summary(FluTrain$ILILag2)

#   PredTest1[11]
