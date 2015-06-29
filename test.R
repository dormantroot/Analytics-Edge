library(ggplot2)
setwd("c:/Work/Analytics-Edge/Data")
data = read.csv("CSVFile_2015-06-25T13_10_30.csv")

summary(data)
str(data)

d = table(data$DESC)
str(d)
df = as.data.frame(d)
df

ggplot(data=df, aes(x=df$Var1, y=df$Freq)) +  geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(df, aes(x=df$Var1, y=df$Freq, fill = variable)) +
  geom_bar(stat="identity", ymin=0, aes(y=value, ymax=value), position="dodge") +
  geom_text(aes(x=filename, y=value, ymax=value, label=value, 
                hjust=ifelse(sign(value)>0, 1, 0)), 
            position = position_dodge(width=1)) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip()



data = read.csv("CSVFile_2015-06-25T13_10_30.csv")
df = data[which(data$ALCOHOL_BLOOD=="Y" |data$ALCOHOL_BREATH=="Y"| data$DRUG_BLOOD=="Y" | data$DRUG_UR=="Y"),]
df = table(df$DESC)
df = as.data.frame(df)
ggplot(data=df, aes(x=df$Var1, y=df$Freq)) +  geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))


data = read.csv("CSVFile_2015-06-25T13_10_30.csv")
df = data[which(data$ALCOHOL_BLOOD=="Y" |data$ALCOHOL_BREATH=="Y"| data$DRUG_BLOOD=="Y" | data$DRUG_UR=="Y"),]
df = table(df$LINESEG_CITY)
df = as.data.frame(df)
df = df[which(df$Freq>10),]
ggplot(data=df, aes(x=df$Var1, y=df$Freq)) +  geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
str(df)



data = read.csv("CSVFile_2015-06-25T13_10_30.csv")
df = data[which((data$ALCOHOL_BLOOD=="Y" & data$ALCOHOL_BLOOD_POS=="Y" )|(data$ALCOHOL_BREATH=="Y" & data$ALCOHOL_BREATH_POS=="Y")| (data$DRUG_BLOOD=="Y" & data$DRUG_BLOOD_POS=="Y") | (data$DRUG_UR=="Y" & data$DRUG_UR_POS=="Y")),]
df = table(df$LINESEG_CITY)
df = as.data.frame(df)
df = df[which(df$Freq>0),]
ggplot(data=df, aes(x=df$Var1, y=df$Freq)) +  geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
df
