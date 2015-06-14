#######################################################
################## DEMOGRAPHICS AND EMPLOYMENT IN THE UNITED STATES #####################
#######################################################
setwd("c:/Work/Analytics-Edge/Data")
CPS = read.csv("CPSData.csv") 


# most common industry
summary(CPS$Industry)

# which has the fewest interviewees
sort(table(CPS$State))


# proportion of citizens
dim(CPS[which(CPS$Citizenship=='Native Citizen'|CPS$Citizenship=='Naturalized Citizen'),])
table(CPS$Citizenship)
