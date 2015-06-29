#######################################################
### Motor Vehicle Theft Analysis in Chicago ###########
#######################################################
setwd("c:/Work/Analytics-Edge/Data")
mvt = read.csv("mvtWeek1.csv") 

# Dimensions
dim(mvt)

# Summary
summary(mvt)
str(mvt)

# ID Maximum
max(mvt$ID)

# Beat Minimum
min(mvt$Beat)

# Number of observations with Arrest Variable=TRUE
dim(mvt[which(mvt$Arrest==TRUE),])

# Number of crimes that occurred in the Alley
dim(mvt[which(mvt$LocationDescription=='ALLEY'),])

# Date of crime
mvt$Date

# median date
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)

# Custom date fields
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

# Which month did the fewest motor vehicle thefts occur?
min(table(mvt$Month))

# Which week did the fewest motor vehicle thefts occur?
table(mvt$Weekday)
max(table(mvt$Weekday))

# Number of observations with Arrest Variable=TRUE
s = mvt[which(mvt$Arrest==TRUE),]
table(s$Month)
max(s$Month)

# Histogram
hist(mvt$Date, breaks=100)

# Boxplot
s = mvt[which(mvt$Arrest==TRUE),]
boxplot(mvt$Date~mvt$Arrest,mvt)

# Proportion of arrests over the years
s = mvt[which(mvt$Arrest==TRUE),]
table(mvt$Arrest, format(mvt$Date, "%Y"))


# Which locations are the top five locations for motor vehicle thefts
table(mvt$LocationDescription)
tail(sort(table(mvt$LocationDescription)),6)

# Retrieve from the above top five locations
dim(mvt[which(mvt$LocationDescription == "ALLEY"|mvt$LocationDescription == "DRIVEWAY - RESIDENTIAL"|mvt$LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)"|mvt$LocationDescription == "STREET"|mvt$LocationDescription == "GAS STATION"),])

Top5 = mvt[which(mvt$LocationDescription == "ALLEY"|mvt$LocationDescription == "DRIVEWAY - RESIDENTIAL"|mvt$LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)"|mvt$LocationDescription == "STREET"|mvt$LocationDescription == "GAS STATION"),]
Top5$LocationDescription = factor(Top5$LocationDescription)
str(Top5)
table(Top5$Arrest, Top5$LocationDescription)


# On which day of the week do the most motor vehicle thefts at gas stations happen?
gasStationInc = mvt[mvt$LocationDescription %in% "GAS STATION",]
table(gasStationInc$Weekday)


# On which day of the week do the fewest motor vehicle thefts in residential driveways happen?
residential = mvt[mvt$LocationDescription %in% "DRIVEWAY - RESIDENTIAL",]
table(residential$Weekday)
