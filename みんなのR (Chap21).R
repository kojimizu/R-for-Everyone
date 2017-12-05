
# Chap21: Time-series and auto-regressive
## Section 21.1: Auto-regressive (AR) / Moving Average (MA)

install.packages("WDI")
require(WDI)

#data collection
gdp<-WDI(country=c("US","CA","GB","DE","CN","JP","SG","IL"),
         indicator=c("NY.GDP.PCAP.CD","NY.GDP.MKTP.CD"),
         start=1960,end=2011)
names(gdp)<-c("iso2c","country","Year","PerCapGDP","GDP")

## data visualize
head(gdp)
require(ggplot2)
require(scales)
# Y: GDP per Capital
ggplot(data=gdp,aes(Year,PerCapGDP,color=country,linetype=country))+
  geom_line()+scale_y_continuous(label=dollar)

## Y: Total GDP 
require(useful)
ggplot(data=gdp,aes(Year,GDP,color=country,linetype=country))+
  geom_line()+scale_y_continuous(label=multiple_format(extra=dollar,multiple="M"))

## US data extract
us <- gdp$PerCapGDP[gdp$country=="United States"]
## data conversion to time-series data
us <- ts(us,start=min(gdp$Year),end=max(gdp$Year))
us

plot(us,xlab="Year", ylab="Per capital GDP")

## ACF(auto covariance function) and PACF(partial ACF)
acf(us)
pacf(us)

## differentials (DID)
x<-c(1,4,8,2,6,6,5,3)
diff(x,differences = 1)
diff(x,differences = 2)
diff(x,lag = 1)
diff(x,lag = 2)

install.packages("forecast")
require(forecast)
ndiffs(x=us)
plot(diff(us,2))

# ARIMA Model
usBest<-auto.arima(x=us) 
usBest

# residual ACF/PACF
acf(usBest$residuals)
pacf(usBest$residuals)

coef(usBest)
summary(usBest)

predict(usBest,n.ahead=5, se.fit=TRUE)
theForecast<-forecast(object=usBest,h=5)
plot(theForecast)
theForecast

# 21.2 VAR
## data frame
require(reshape2)
gdpCast<-dcast(Year~country,data=gdp[,c("country","Year","PerCapGDP")],
               value.var="PerCapGDP")
edit(gdpCast)

## Germany: insufficient data(10 years)
gdpCast <-subset(gdpCast,Germany !="NA")
gdpTS<- ts(data=gdpCast[,-1],start=min(gdpCast$Year),
           end=max(gdpCast$Year))

plot(gdpTS, plot.type="single",col=1:8)
legend("topleft",legend=colnames(gdpTS),ncol=2,lty=1,
       col=1:8,cex=.9)

gdpTS <-gdpTS[,which(colnames(gdpTS)!="Germany")]

numDiffs <-ndiffs(gdpTS)
numDiffs

gdpDiffed<-diff(gdpTS, differences=numDiffs)
plot(gdpDiffed, plot.type="single",col=1:7)
legend("bottomleft", legend=colnames(gdpDiffed),ncol=2,lty=1,
        col=1:7,cex=.9)

# VAR model application
install.packages("vars")
require(vars)
gdpVar<-VAR(gdpDiffed,lag.max=12)
gdpVar$p

names(gdpVar$varresult)
class(gdpVar$varresult$Canada)
class(gdpVar$varresult$Japan)
head(coef(gdpVar$varresult$Canada))
head(coef(gdpVar$varresult$Japan))

require(coefplot)
coefplot(gdpVar$varresult$Canada)
coefplot(gdpVar$varresult$Japan)

predict(gdpVar,n.ahead=5)

# 21.3 GARCH
require(quantmod)
att<-getSymbols("T",auto.assign=FALSE)

require(xts)
head(att)
plot(att)
chartSeries(att)
addBBands()
addMACD(32,50,12)

attClose <- att$T.Close
class(attClose)
head(attClose)

# rugarch function
install.packages("rugarch")
require(rugarch)
attSpec<-ugarchspec(variance.model=list(model="sGARCH",
                                        garchOrder=c(1,1)),
mean.model=list(armaOrder=c(1,1)),
distribution.model="std")

attGarch <- ugarchfit(spec=attSpec,data=attClose)
attGarch
b
## attGarch - S4 object, slot fit - list (element accessible by $)
plot(attGarch@fit$residuals,type="l")
plot(attGarch, which=10)

# AIC comparsion - model accuracy 
## ARMA(1,1)
attSpec1 <-ugarchspec(variance.model=list(model="sGARCH",
                                          garchOrder=c(1,1)),
                      mean.model=list(armaOrder=c(1,1)),
                                      distribution.model="std")
## ARMA(0,0)
attSpec2 <-ugarchspec(variance.model=list(model="sGARCH",
                                          garchOrder=c(1,1)),
                      mean.model=list(armaOrder=c(1,1)),
                      distribution.model="std")

## ARMA(0,2)
attSpec3 <-ugarchspec(variance.model=list(model="sGARCH",
                                          garchOrder=c(1,1)),
                      mean.model=list(armaOrder=c(1,1)),
                      distribution.model="std")




