#######################################################
############### Internet Privacy Poll ##################
#######################################################
setwd("c:/Work/Analytics-Edge/Data")
CPS = read.csv("AnonymityPoll.csv") 

str(CPS)

table(CPS$Smartphone)


table(CPS$Sex, poll$Region)