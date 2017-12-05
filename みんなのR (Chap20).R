

# Chap20: Non-linear model

# SS 20.1 Non-linear OLS
#http://jaredlander.com/data/wifi.rdata"
load("C:/Users/kojikm.mizumura/Downloads/wifi.rdata")
head(wifi)

# ggplot x-axis:device location ,y-axis:device location, color:distance from hotspot
require(ggplot2)
ggplot(wifi,aes(x=x,y=y,color=Distance))+geom_point()+
  scale_color_gradient2(low="blue",mid="white",high="red",
                        midpoint=mean(wifi$Distance))

# nls setting
wifitMod1 <- nls(Distance ~ sqrt((betaX-x)^2 +(betaY-y)^2),
                 data=wifi, start=list(betaX=50,betaY=50))
summary(wifitMod1)

# optimal point plotting in ggplot2
ggplot(wifi,aes(x=x, y=y, color=Distance))+geom_point()+
  scale_color_gradient2(low="blue",mid="white",high="red",
                        midpoint=mean(wifi$Distance))+
  geom_point(data=as.data.frame(t(coef(wifitMod1))),
             aes(x=betaX,y=betaY,size=5,color="green"))

# 20.2 Spline
data (diamonds)
diaSpline1<- smooth.spline(x=diamonds$carat,y=diamonds$price)
diaSpline2<- smooth.spline(x=diamonds$carat,y=diamonds$price, df=2)
diaSpline3<- smooth.spline(x=diamonds$carat,y=diamonds$price, df=10)
diaSpline4<- smooth.spline(x=diamonds$carat,y=diamonds$price, df=20)
diaSpline5<- smooth.spline(x=diamonds$carat,y=diamonds$price, df=50)
diaSpline6<- smooth.spline(x=diamonds$carat,y=diamonds$price, df=100)

get.spline.info <- function(object)
{
 data.frame(x=object$x,y=object$y, df=object$df) 
}

require(plyr)
splineDF<-ldply(list(diaSpline1,diaSpline2,diaSpline3,
                     diaSpline4,diaSpline5,diaSpline6),
                get.spline.info)
head(splineDF)
g<- ggplot(diamonds,aes(x=carat,y=price))+geom_point()
g + geom_line(data=splineDF,
              aes(x=x,y=y,color=factor(round(df,0)),
                  group=df))+scale_color_discrete("Digrees of \nFreedom")

# B-Spline
require(splines)
head(ns(diamonds$carat,df=1))
head(ns(diamonds$carat,df=2))
head(ns(diamonds$carat,df=3))
head(ns(diamonds$carat,df=4))

g<-ggplot(diamonds,aes(x=carat,y=price))+geom_point()
g+stat_smooth(method="lm",formula=y~ns(x,6),color="blue")
g+stat_smooth(method="lm",formula=y~ns(x,3),color="blue")

# 20.3 GAM (Generalized Additive Model)
## creation of column vector
creditNames <- c("Checking","Duration","CreditHistory","Purpose",
                 "CreditAmount","Savings","Employment","InstallmentRate",
                 "GenderMartal","OtherDebtors","YearsAtResidence","RealEstate",
                 "Age","OtherInstallment","Housing","ExistingCredits",
                 "Job","NumLiable","Phone","Foregin","Credit")

## read.table() to read the file (no header)
## CreditNames is to be used for columnName
theURL<-"http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"
credit<-read.table(theURL, sep="",header=FALSE,
                   col.names=creditNames,
                   stringsAsFactors = FALSE)
head(credit)

## data conversion
head(credit[,c("CreditHistory","Purpose","Employment","Credit")])
creditHistory<-c(A30="All Paid",A31="All Paid This Bank",
                 A32="Up To Date", A33="Late Payment",
                 A34="Critical Account")
purpose<-c(A40="car(new)",A41="car(used)",
           A42="furniture/equipment", A43="radio/television",
           A44="domestic appliances",A45="repairs",
           A46="education",A47="(vacation - does not exist?)",
           A48="retraining",A49="business",A410="others")
employment<-c(A71="unemployed",A72="<1 year",
              A73="1-4 years",A74="4-7 years",A75=">= 7 years")
credit$CreditHistory<-creditHistory[credit$CreditHistory]
credit$Purpose<-purpose[credit$Purpose]
credit$Employment<-employment[credit$Employment]

## change of credit format (Good/Bad)
credit$Credit<-ifelse(credit$Credit==1,"Good","Bad")
credit$Credit<-factor(credit$Credit,levels=c("Good","Bad"))

head(credit[,c("CreditHistory","Purpose","Employment","Credit")])

## ggplot
require(useful)
ggplot(credit, aes(x=CreditAmount,y=Credit))+
  geom_jitter(position=position_jitter(height=.2))+
  facet_grid(CreditHistory ~ Employment)+
  xlab("Credit Amount")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5))+
  scale_x_continuous(labels=multiple)

ggplot(credit,aes(x=CreditAmount,y=Age))+
  geom_point(aes(color=Credit))+
  facet_grid(CreditHistory~Employment)+
  xlab("Credit Amount")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5))+
  scale_x_continuous(labels=multiple)

# GAM model application
require(mgcv)
creditGam<- gam(Credit~te(CreditAmount)+s(Age)+CreditHistory+Employment,
                data=credit,family=binomial(link="logit"))
summary(creditGam)

## smoothing function (spline,tensor) visualization
plot(creditGam,select=1,se=TRUE,shade=TRUE)
plot(creditGam,select=2,se=TRUE,shade=TRUE)

#20.4 Decision Tree Model
require(rpart)
creditTree <- rpart(Credit~CreditAmount+Age+CreditHistory+Employment, data=credit)
creditTree
## decision tree visualization
install.packages("rpart.plot")
require(rpart.plot)
rpart.plot(creditTree,extra=4)

# 20.5 Ramdom Forest
require(useful)
install.packages("randomForest")
require(randomForest)
creditFormula<- Credit~CreditHistory+Purpose+Employment+Duration+Age+CreditAmount
creditX<-build.x(creditFormula,data=credit)
creditY<-build.y(creditFormula,data=credit)
edit(creditY)

## random forest application
creditForest<-randomForest(x=creditX,y=creditY)
creditForest


