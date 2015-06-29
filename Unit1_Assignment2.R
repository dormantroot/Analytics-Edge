#######################################################
################## Stock Dynamics #####################
#######################################################
setwd("c:/Work/Analytics-Edge/Data")
IBM = read.csv("IBMStock.csv") 
GE = read.csv("GEStock.csv") 
ProcterGamble = read.csv("ProcterGambleStock.csv") 
CocaCola = read.csv("CocaColaStock.csv") 
Boeing = read.csv("BoeingStock.csv") 

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")


tail(IBM[order(IBM$Date, decreasing=TRUE), ])
sd(ProcterGamble$StockPrice)


### Visualizing Stock Dynamics
plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
#abline(v=as.Date(c("2000-03-01")), lwd=2)


plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="blue")
lines(GE$Date[301:432], GE$StockPrice[301:432], col="green")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="orange")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="purple")
abline(v=as.Date(c("2000-03-01")), lwd=1)
abline(v=as.Date(c("2000-04-01")), lwd=1)


### Monthly Trends
tapply(IBM$StockPrice, months(IBM$Date), mean)
plot(tapply(IBM$StockPrice, months(IBM$Date), mean), type="l")
