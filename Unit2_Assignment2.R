#######################################################
############## READING TEST SCORES ####################
#######################################################
setwd("c:/Work/Analytics-Edge/Data")
mvt = read.csv("pisa2009train.csv") 

#1.1  
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")
str(pisaTrain)

#1.2
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

#1.3
summary(pisaTrain)

#1.4
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
str(pisaTrain) 
str(pisaTest)

#2.1
# Male only has 2 levels (1 and 0). There is no natural ordering between the different values of raceeth, so it is an unordered factor. Meanwhile, we can order grades (8, 9, 10, 11, 12), so it is an ordered factor.

#2.2


#2.3
# An Asian student will have raceethAsian set to 1 and all other raceeth binary variables set to 0. Because "White" is the reference level, a white student will have all raceeth binary variables set to 0.

#3.1
lmScore = lm(readingScore~., data=pisaTrain)
summary(lmScore)

#3.2
SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE / nrow(pisaTrain))
sqrt(mean(lmScore$residuals^2))

#3.3
# The coefficient 29.54 on grade is the difference in reading score between two students who are identical other than having a difference in grade of 1. Because A and B have a difference in grade of 2, the model predicts that student A has a reading score that is 2*29.54 larger.

#3.4
# The only difference between an Asian student and white student with otherwise identical variables is that the former has raceethAsian=1 and the latter has raceethAsian=0. The predicted reading score for these two students will differ by the coefficient on the variable raceethAsian.

#4.1
predTest = predict(lmScore, newdata=pisaTest)
# From summary(predTest), we see that the maximum predicted reading score is 637.7, and the minimum predicted score is 353.2. Therefore, the range is 284.5


#4.2
sum((predTest-pisaTest$readingScore)^2)
sqrt(mean((predTest-pisaTest$readingScore)^2))

#4.3
baseline = mean(pisaTrain$readingScore)
sum((baseline-pisaTest$readingScore)^2)

#4.4
# The test-set R^2 is defined as 1-SSE/SST, where SSE is the sum of squared errors of the model on the test set and SST is the sum of squared errors of the baseline model. For this model, the R^2 is then computed to be 1-5762082/7802354