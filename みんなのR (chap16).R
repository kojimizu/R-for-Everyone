
#16 linear regression
require(UsingR)
require(ggplot2)
head(father.son)
ggplot(father.son, aes(x = fheight, y = sheight)) + geom_point()+
  geom_smooth(method = "lm") + labs (x = "Fathers", y = "sons")

#16.1 Simple linear regression
require(UsingR)
heightLM <- lm(sheight ~ fheight, data = father.son)
heightLM
summary(heightLM)

#16.1.1 ANOVA alternative
require(UsingR)
data(tips, package = "reshape2")
head(tips)
tipAnova <-aov(tip ~ day -1, tips)
tipsLM <- lm(tip ~ day - 1, data = tips)
summary(tipAnova)
summary(tipsLM)

require(plyr)
tipsByDay <- ddply(tips,"day",summarize,
                   tip.mean = mean(tip), tip.sd=sd(tip),
                   Length = NROW(tip), 
                   tfrac = qt(p=.90, df= Length-1),
                   Lower = tip.mean - tfrac*tip.sd / sqrt(Length),
                   Upper = tip.mean + tfrac*tip.sd / sqrt(Length)
                   )
ggplot(tipsByDay, aes(x=tip.mean, y=day)) + geom_point()+
  geom_errorbarh(aes(xmin=Lower, xmax=Upper), height =.3)+
  ggtitle("Tips by day calculated manually")

tipsInfo <- summary(tipsLM)
tipsCoef <- as.data.frame(tipsInfo$coefficients[,1:2])
tipsCoef <- within (tipsCoef, {+
            Lower <- Estimate + qt(p=.90, df=tipsInfo$df[2])*'Std.Error'+
            Upper <- Estimate + qt(p=.90, df=tipsInfo$df[2])*'Std.Error'+
            day <- rownames(tipsCoef)})

ggplot(tipsCoef, aes(x=Estimate, y=day))+geom_point()+
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height=.3)+
  ggtitle("Tips by day calculated from regression model")

#16.2 Multiple Regression
housing <- read.table("http://jaredlander.com/data/housing.csv
", sep = ",", header = TRUE, stringsAsFactors = FALSE) 

# variable name change
names(housing)
names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt", 
                    "SqFt", "Income", "IncomePerSqFt", "Expense",
                    "ExpensePerSqFt","NetIncome", "Value", "ValuePerSqFt",
                    "Boro")
head(housing)

#data visualization
require(ggplot2)
ggplot(housing, aes(x=ValuePerSqFt)) +
  geom_histogram(binwidth = 10) + labs(x = "Value per Squire Foot")
# ggplot by boro
ggplot(housing, aes(x=ValuePerSqFt, fill = Boro)) +
  geom_histogram(binwidth = 10)+ labs(x="Value per Square Foot")
ggplot(housing, aes(x=ValuePerSqFt, fill = Boro)) +
  geom_histogram(binwidth = 10)+ labs(x="Value per Square Foot")+
  facet_wrap(~Boro)
# ggplot - histogram by SqFt/Units
ggplot(housing, aes(x=SqFt)) + geom_histogram()
ggplot(housing, aes(x=Units)) + geom_histogram()
ggplot(housing[housing$Units<1000,], aes(x=SqFt))+geom_histogram()
ggplot(housing[housing$Units<1000,],aes(x=Units)) + geom_histogram()

##ggplot - point by SqFt/Units
ggplot(housing, aes(x =SqFt, y = ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x =Units, y = ValuePerSqFt)) + geom_point()
ggplot(housing[housing$Units<1000,], aes(x =SqFt, y = ValuePerSqFt)) + geom_point()
ggplot(housing[housing$Units<1000,], aes(x =Units, y = ValuePerSqFt)) + geom_point()

#Exclude outlier
sum(housing$Units>=1000)
housing1 <- housing[housing$Units < 1000, ]

#log-conversion
require(ggplot2)
#by SqFt
ggplot(housing, aes(x =SqFt, y = ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x =log(SqFt), y = ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x =SqFt, y = log(ValuePerSqFt))) + geom_point()
ggplot(housing, aes(x =log(SqFt), y = log(ValuePerSqFt))) + geom_point()

ggplot(housing, aes(x =Units, y = ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x =log(Units), y = ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x =Units, y = log(ValuePerSqFt))) + geom_point()
ggplot(housing, aes(x =log(Units), y = log(ValuePerSqFt))) + geom_point()

#regression analysis
house2 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data=housing1)
summary(house2)
house2$coefficients

require(ggplot2)
install.packages("coefplot")
require(coefplot)
coefplot(house2)

#Inclusing of interaction term
house3 <- lm(ValuePerSqFt ~ Units*SqFt + Boro, data=housing1)
house4 <- lm(ValuePerSqFt ~ Units:SqFt + Boro, data=housing1)
house3$coefficients
house4$coefficients
coefplot(house3)
coefplot(house4)

house5 <- lm(ValuePerSqFt ~ Units*SqFt*Boro, data=housing1)
house5$coefficients

house6<- lm(ValuePerSqFt ~ Class*Boro, data=housing1)
house6$coefficients

house7 <- lm(ValuePerSqFt ~ I(SqFt/Units)+Boro, data=housing1)
house7$coefficients

house8 <- lm(ValuePerSqFt ~ (Units + SqFt)^2, data = housing1)
house8$coefficients
house9 <- lm(ValuePerSqFt ~ Units*SqFt, data = housing1)
identical(house8$coefficients, house9$coefficients)

house10 <- lm(ValuePerSqFt ~ I(Units + SqFt)^2, data=housing1)
house10$coefficients

#model coefficient comparison
multiplot(house2, house3, house4)

housingNew <- read.table("http://www.jaredlander.com/data/housingNew.csv", sep=",",header=TRUE,stringsAsFactors=FALSE)
housePredict <- predict(house2, newdata = housingNew, se.fit = TRUE, interval ="prediction", level=.95)
head(housePredict$fit)
head(housePredict$se.fit)
house2$coefficients

