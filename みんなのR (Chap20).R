

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
?smooth.spline
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
