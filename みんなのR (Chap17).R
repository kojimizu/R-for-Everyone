
# Chap17: Logistic Regression

acs <- read.table("http://jaredlander.com/data/acs_ny.csv", sep=",", header=TRUE, stringsAsFactors = FALSE)
acs$income <- with (acs, FamilyIncome >= 150000)
require(ggplot2)
require(useful)
ggplot(acs, aes(x=FamilyIncome)) + 
  geom_density(fill = "grey", color = "grey") +
  geom_vline(xintercept = 150000)
  scale_x_continuous(label = multiple.dollar, limits = c(0,1000000))
                     
head(acs)

# logistic analysis
income1 <- glm(income ~ HouseCosts + NumWorkers +OwnRent + NumBedrooms +
                 FamilyType, family = binomial(link = "logit"), data=acs)
summary(income1)
require(coefplot)
coefplot(income1)

# inverse logit conversion
invlogit <- function (x)
{
  1/(1+exp(-x))
}
invlogit(income1$coefficients)

#17.2 Poisson Regression
ggplot(acs, aes(x=NumChildren)) + geom_histogram(binwidth = 1)
Children1 <- glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent, data=acs, family = poisson(link = "log"))
summary(Children1)
coefplot(Children1)

# residual standardization 
z <- (acs$NumChildren - Children1$fitted.values) /
    sqrt(Children1$fitted.values)
# over-variance factor
sum(z^2) / Children1$df.residual
# over-variance p-value
pchisq(sum(z^2), Children1$df.residual)

Children2 <- glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent,
                 data = acs, family=quasipoisson(lin = "log"))
multiplot(Children1, Children2)

#17.4 Survival analysis
require(survival)
head(bladder)
bladder[100:105,]
#objective function check
survObject <-with(bladder[100:105,], Surv(stop,event))
survObject
survObject[,1:2]

cox1 <- coxph(Surv(stop,event) ~ rx + number + size + enum,
              data = bladder)
summary(cox1)
plot(survfit(cox1), xlab="Days",ylab="Survival Rate", conf.int=TRUE)

cox2 <- coxph(Surv(stop,event) ~ strata(rx) + number + size + enum,
    data =bladder)
summary(cox2)
plot(survfit(cox2), xlab="Days",ylab="Survival Rate", conf.int=TRUE, col=1:2)
legend ("bottomleft", legend=c(1,2), lty=1, col=1:2, text.col=1:2, title="rx")
