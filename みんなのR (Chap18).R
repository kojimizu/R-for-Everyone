
#Chap18: model evaluation

##dataset: housing data
housing <- read.table("http://jaredlander.com/data/housing.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt", 
                    "SqFt", "Income", "IncomePerSqFt", "Expense",
                    "ExpensePerSqFt","NetIncome", "Value", "ValuePerSqFt",
                    "Boro")
##outlier exclusion
housing <-housing[housing$Units < 1000,]
head(housing)

# Model estimation
house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data = housing)
summary(house1)
require(coefplot)
coefplot(house1)

# residual plot
require(ggplot2)
## check enforced linear model
head(fortify(house1))
h1 <- ggplot(aes(x=.fitted, y=.resid), data=house1) +
  geom_point() + geom_hline(yintercept =0) + geom_smooth(se = FALSE)+  
  labs(x="Fitted Value", y="Residuals")
h1
h1 + geom_point(aes(color=Boro))

#Visulization by basic function
plot (house1, which=1)
# color change by Boro function
plot (house1, which=1, col = as.numeric(factor(house1$model$Boro)))
#label
legend("topright", legend=levels(factor(house1$model$Boro)), pch=1,
       col=as.numeric(factor(levels(factor(house1$model$Boro)))),
       text.col=as.numeric(factor(levels(factor(house1$model$Boro)))),title="Boro")

# Q-Q Plot
plot(house1, which = 2)
ggplot(house1, aes(sample = .stdresid))+stat_qq()+geom_abline()

# Residual plot by histogram
ggplot(house1, aes(x=.resid)) + geom_histogram()

## 18.2 Model comparison
house2<-lm(ValuePerSqFt ~ Units + SqFt + Boro, data = housing) 
house3<-lm(ValuePerSqFt ~ Units + SqFt*Boro + Class, data = housing)
house4<-lm(ValuePerSqFt ~ Units + SqFt*Boro + SqFt*Class, data = housing)
house5<-lm(ValuePerSqFt ~ Boro + Class, data = housing)

require(coefplot)
multiplot(house2, house3, house4, house5, pointSize = 2)
anova(house2, house3, house4, house5)
AIC(house2, house3, house4, house5)
BIC(house2, house3, house4, house5)

# data categorization <ValuePerSqFt >= 150>
housing$HighValue <- housing$ValuePerSqFt >= 150

high1<-glm(HighValue ~ Units + SqFt + Boro, data = housing, family=binomial(link = "logit")) 
high2<-glm(HighValue ~ Units * SqFt + Boro, data = housing, family=binomial(link = "logit")) 
high3<-glm(HighValue ~ Units + SqFt * Boro + Class, data = housing, family=binomial(link = "logit")) 
high4<-glm(HighValue ~ Units + SqFt * Boro + SqFt * Class, data = housing, family=binomial(link = "logit")) 
high5<-glm(HighValue ~ Boro + Class, data = housing, family=binomial(link = "logit")) 
anova (high1, high2, high3, high4, high5)
AIC (high1, high2, high3, high4, high5)
BIC (high1, high2, high3, high4, high5)

##Cross-valudation
require(boot)
#glm model set
houseG1 <-glm(ValuePerSqFt ~ Units + SqFt + Boro, data=housing, family=gaussian(link = "identity"))
houseG1

identical(coef(house1),coef(houseG1))
houseCV1 <- cv.glm(housing,houseG1, K=5)
houseCV1$delta

##model selection
houseG2 <-glm(ValuePerSqFt ~ Units + SqFt + Boro, data=housing, family=gaussian(link = "identity"))
houseG3 <-glm(ValuePerSqFt ~ Units + SqFt * Boro + Class, data=housing, family=gaussian(link = "identity"))
houseG4 <-glm(ValuePerSqFt ~ Units + SqFt + Boro + SqFt * Class, data=housing, family=gaussian(link = "identity"))
houseG5 <-glm(ValuePerSqFt ~ Boro + Class, data=housing, family=gaussian(link = "identity"))

houseCV2 <- cv.glm(housing,houseG2, K=5)
houseCV3 <- cv.glm(housing,houseG3, K=5)
houseCV4 <- cv.glm(housing,houseG4, K=5)
houseCV5 <- cv.glm(housing,houseG5, K=5)

cvResults <- as.data.frame(rbind(houseCV1$delta, houseCV2$delta, 
                           houseCV3$delta, houseCV4$delta,
                           houseCV5$delta))
names(cvResults) <- c("Error", "Ajudsted.Error")
cvResults$Model <- sprintf("houseG%s", 1:5)
cvResults

##visualization model comparison (ANOVA, AC, cross-validation)
#ANOVA test
cvANOVA <- anova(houseG1, houseG2, houseG3, houseG4, houseG5)
cvResults$ANOVA <- cvANOVA$`Resid. Dev`
edit(cvResults)
# AIC calculation
cvResults$AIC<-AIC(houseG1, houseG2, houseG3, houseG4, houseG5)$AIC
cvResults
# data.frame forming
require(reshape2)
cvMelt<-melt(cvResults, id.vars="Model", 
              variable.name="Measure", value.name="Value")
cvMelt
ggplot(cvMelt, aes(x=Model, y=Value))+
  geom_line(aes(group=Measure, color=Measure))+
  facet_wrap(~Measure, scales="free_y")+
  theme(axis.text.x=element_text(angle=90,vjust=.5))+
  guides(color=FALSE)

# general cross-validation
cv.work<-function(fun, k=5, data, 
                    cost = function(y,yhat) mean((y -yhat)^2),
                    response="y",...)
{
  # generation of folds variables
  folds<-data.frame(Fold=sample(rep(x=1:k, length.out=nrow(data))),
                    Row=1:nrow(data))
  # takes error=0
  error<- 0
  
  #loop each folds and model application to training data
  #forecast based on test data
  #error calculation
  for(f in 1:max(folds$Fold))
  {
    #extract rows corresponding to test data
    theRows <- folds$Row[folds$Fold == f]
    
    ## application of fun to data[-theRows,]
    ## forecast against data[theRows,]
    mod <- fun(data=data[-theRows,],...)
    pred <- predict(mod, data[theRows,])
    
    # accumulate error weighted by rows corresponding to fold
    error <- error +
      cost(data[theRows,response],pred)*
      (length(theRows)/nrow(data))
      }
  return(error)
  }

cv1<-cv.work(fun=lm, k=5, data=housing, response="ValuePerSqFt",
             formula =ValuePerSqFt ~ Units + SqFt + Boro)
cv2<-cv.work(fun=lm, k=5, data=housing, response="ValuePerSqFt",
             formula =ValuePerSqFt ~ Units * SqFt + Boro)
cv3<-cv.work(fun=lm, k=5, data=housing, response="ValuePerSqFt",
             formula =ValuePerSqFt ~ Units + SqFt * Boro + Class)
cv4<-cv.work(fun=lm, k=5, data=housing, response="ValuePerSqFt",
             formula =ValuePerSqFt ~ Units + SqFt * Boro + SqFt * Class)
cv5<-cv.work(fun=lm, k=5, data=housing, response="ValuePerSqFt",
             formula =ValuePerSqFt ~ Boro + Class)
cvResults <- data.frame(Model=sprintf("house%s", 1:5),
                        Error=c(cv1, cv2, cv3, cv4, cv5))
cvResults

##18.4 Bootsrap
require(plyr)
baseball <- baseball[baseball$year>=1990,]
head(baseball)

bat.avg <- function(data, indices=1:NROW(data), hits="h",
                    at.bats="ab")
{
  sum(data[indices,hits], na.rm=TRUE) /
    sum(data[indices,at.bats],nar.rm=TRUE)
}
bat.avg(baseball)

# Use of bootstrap method 
# 1200 loop (data:baseball, bat.avg function)
install.packages("boot")
require(boot)
avgBoot<-boot(data=baseball, statistic=bat.avg, R=1200, stype="i")
avgBoot
boo.ci(avgBoot, conf=.95, type="norm")

ggplot()+
  geom_histogram(aes(avgBoot$t), fill="grey",color="grey")+
  geom_vline(xintercept=avgBoot$t0 + c(-1,1)*2*sqrt(var(avgBoot$t)),
             linetype=2)

##18.5 stepwise variable selection method 
nullModel<-lm(ValuePerSqFt ~1, data=housing)
fullModel<-lm(ValuePerSqFt~Units+SqFt*Boro+Boro*Class, data=housing)

houseStep<-step(nullModel, scope=list(lower=nullModel, upper=fullModel),
                direction="both")
houseStep


