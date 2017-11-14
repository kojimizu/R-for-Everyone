
##Chap13 - word conversion
#13.1 Paste function
paste ("Hello","Jared","and Others")
paste ("Hello","Jared","and Others",sep = "/")
paste(c("Hello","Hej","Howdy"), c("Jared","Bob","David"))
       
vectorOfText <- c("Hello","Everyone","out there",".")
paste(vectorOfText, collapse = " ")
paste(vectorOfText, collapse= "*")

#13.2 Sprintf function
person <- "Jared"
partySize <- "eight"
waitTime <- 25
sprintf("Hello %s your party of %s will be seated in %s minutes",person,partySize,waitTime)

sprintf("Hello %s, your party of %s will be seated in %s minutes", 
        c("Jared","Bob"),c("eight",16,"four",10), waitTime)

#13.3 Text Extract 

require (XML)
load("data/presidents.rdata")
theURL <- "http://www.loc.gov/rr/print/list/057_chron.html"
pr
esindents <- readHTMLTable((theURL, which=3, as.data.frame=TRUE, skip.rows=1, header=TRUE, stringsAsFactors=FALSE))
  

#14 Probaility Distribution
#14.1 Normal (Gaus) Distribution
rnorm(n=10)
randNorm10 <- rnorm(10)
randNorm10
dnorm(randNorm10)
dnorm(c(-1,0,1))

# data generation from normal distribution
randNorm <- rnorm(30000)
# distribution density calculation
randDensity <- dnorm(randNorm)
#ggplot2 download
require(ggplot2)
#chart generation
ggplot(data.frame(x=randNorm, y=randDensity)) +
aes(x=x, y=y) + geom_point() + labs(x="Random Normal Variables", y="Density")

pnorm(randNorm10)
pnorm(c(-3,0,3))
pnorm(-1)
pnorm(1) - pnorm(0)
pnorm(1) - pnorm(-1)

#pnorm function visualization
p <- ggplot(data.frame(x=randNorm, y=randDensity)) + aes(x=x, y=y) + geom_point() + labs(x="x", y="Density")

# calculation of left edge to -1 for shadow space generation
neg1Seq <- seq(from=min(randNorm), to=-1, by=.1)

# x-axix by data.frame / calcultion of y values based on x-axix
lessThanNeg1 <- data.frame(x=neg1Seq,y=dnorm(neg1Seq))
head (lessThanNeg1)

#connection of end-point from left-edge and right-edge at height 0
lessThanNeg1 <- rbind(c(min(randNorm), 0),lessThanNeg1,c(max(lessThanNeg1$x),0))
p + geom_polygon(data=lessThanNeg1,aes(x=x,y=y))

#---------------------------------------------
#sequential values generation from -1to 1
neg1Pos1Seq <- seq(from=-1, to=1, by=.1)

# x value by data frame and calucation of y value density
neg1To1 <-data.frame(x=neg1Pos1Seq,y=dnorm(neg1Pos1Seq))
head(neg1To1)

# consolidation of left-edge and right-edge end point at height of 0 
neg1To1 <-rbind(c(min(neg1To1$x), 0),neg1To1,c(max(neg1To1$x),0))
p +geom_polygon(data=neg1To1,aes(x=x,y=y))

# cummulative density function
randProb <- pnorm(randNorm)
ggplot (data.frame(x=randNorm, y=randProb)) +aes(x=x, y=y) + 
  geom_point() + labs (x="Random Normal Variables", y="Probability")

#14.2 Binary Distribution
rbinom(n=1, size=10, prob=0.4)
rbinom(n=5, size=10, prob=0.4)

#bernoulli distribution
rbinom(n=5, size=1, prob=0.4)

#visualization of binary distribution
bnomData <- data.frame(Success = rbinom(n=10000, size=10,prob=0.3))
require(ggplot2)
ggplot(bnomData, aes(x=Success)) + geom_histogram(binwidth = 1)

#size=5
binom5<- data.frame(Successes = rbinom (n=10000,size=5,prob=0.3), Size=5)
dim(binom5)
head(binom5)

binom10<-data.frame(Successes = rbinom(n=10000,size=10,prob=0.3), Size=10)
dim(binom10)
head(binom10)

binom100<-data.frame(Successes = rbinom(n=10000,size=100,prob=0.3), Size=10)

binom1000<-data.frame(Successes = rbinom(n=10000,size=1000,prob=0.3), Size=10)

binomAll <- rbind(binom5,binom10,binom100,binom1000)
dim(binomAll)

head(binomAll, 10)
tail(binomAll, 10)

# plot 
ggplot(binomAll, aes(x=Successes)) + geom_histogram()+
  facet_wrap(~Size, scales="free")

# 10 test / 0.3 success probability / 3 is casted probability
dbinom(x =3, size = 10, prob = 0.3)

# 10 test / 0.3 success probability / less than 3 is casted cumulative prob
pbinom(q = 3, size = 10, prob = 0.3)

#vector 
dbinom (x=1:10, size=10, prob=0.3)
pbinom (q=1:10, size=10, prob=0.3)
qbinom (p=c(0.3, 0.35, 0.4,0.5,0.6), size=10, prob=0.3)

## Poission distribution
pois1 <- rpois(n=10000, lambda=1)
pois2 <- rpois(n=10000, lambda=2)
pois5 <- rpois(n=10000, lambda=5)
pois10 <- rpois(n=10000, lambda=10)
pois20 <- rpois(n=10000, lambda=20)
pois <-data.frame(lambda.1=pois1,lambda.2=pois2,
                  lambda.5=pois5,lambda.10=pois10, lambda.20=pois20)

#reshape package download
require(reshape2) #
# data format change to "long"
pois<- melt(data=pois, variable.name = "lambda", value.name = "x") 
#column name brush-up
require(stringr)
## clean λ and focus on values
pois$lambda < as.factor(as.numeric(str_extract(string = pois$lambda, pattern = "\\d+")))

tail(pois)

#graph of poisson distribution by λ
require(ggplot2)
ggplot(pois, aes(x=x)) + geom_histogram(binwidth = 1) + facet_wrap(~lambda)+ggtitle("Probability Mass Function")

# overlapping graph of poisson distribution 
ggplot(pois, aes(x=x)) + geom_density(aes(group=lambda, color=lambda, fill=lambda),adjust =4, alpha = 1/2) +
  scale_color_discrete() + scale_fill_discrete() + ggtitle("Probability Mass Function")

#Chap.15 Basic statistics
x <- sample(x=1:100, size=100, replace=TRUE)
mean (x)
y<-x 
y[sample(x=1:100, size=20, replace=FALSE)] <- NA
y
mean (y, na.rm=TRUE)
grades <- c(95,72,87,66)
weights <- c(1/2,1/4,1/8,1/8)
mean(grades)
weighted.mean(x=grades,w=weights)
var(x)
sum((x-mean(x))^2)/(length(x)-1)
sqrt(var(x))
sd(x)

summary(x)
quantile(x,probs=c(0.25,0.75))
quantile(x, probs=c(0.1, 0.25, 0.5, 0.75, 0.99))

#15.2 covariance / correlation
require(ggplot2)
head(economics)
cor(economics$pce, economics$psavert)

# components calculation of correlation
xPart <- economics$pce - mean(economics$pce)
yPart <- economics$psavert - mean(economics$psavert)
nMinusOne <- (nrow(economics)-1)
xSD <- sd(economics$pce)
ySD <- sd(economics$psavert)
sum(xPart*yPart)/(nMinusOne*xSD*ySD)

cor (economics[,c(2,4:6)])
install.packages("GGally")
GGally::ggpairs(economics, economics[,c(2,4:6)], parm=list(labelSize=8))

# correlation heatmap
require(reshape2)
require(scales)
econCor <-cor(economics[,c(2,4:6)])
econMelt <- melt(econCor, varnames = c("x","y"),
                 value.name = "Correlation")
econMelt <- econMelt[order(econMelt$Correlation),]
ggplot (econMelt, aes(x=x, y=y)) + 
  geom_tile(aes(fill =Correlation))+
  scale_fill_gradient2(low=muted("red"),mid="white",
                       high = "steelblue", 
                       guide=guide_colorbar(ticks=FALSE, barheight=10),
                                            limits=c(-1,1)) +
  theme_minimal() +
  labs(x=NULL, y=NULL)

# missing value delete
m <- c(9,9,NA,3,NA,5,8,1,10,4)
n <- c(2,NA,1,6,6,4,1,1,6,7)
p <- c(8,4,3,9,10,NA,3,NA,9,9)
q <- c(10,10,7,8,4,2,8,5,5,2)
r <- c(1,9,7,6,5,6,2,7,9,10)
#combine by cbind
theMat <- cbind(m,n,p,q,r)
theMat1 <- rbind(m,n)
# multiple options to delete NA
cor(theMat, use="everything")
cor(theMat, use="complete.obs")
cor(theMat, use="na.or.complete")
cor(theMat[c(1,4,7,9,10),])

#copmarison results
identical(cor(theMat, use="na.or.complete"),cor(theMat[c(1,4,7,9,10),]))

#pariwise.complete
cor(theMat, use="pairwise.complete.obs")
cor(theMat[,c("m","n")], use="complete.obs")
cor(theMat[,c("m","p")],use="complete.obs")

#ggpairs charts
require(reshape2)
data("tips",package="reshape2")
head(tips)
GGally::ggpairs(tips)

#15.3 t test
head(tips)
unique(tips$sex)
unique(tips$day)

#15.3.1 one sample variable case (t-test)
t.test(tips$tip, alternative="two.sided",mu=2.5)
# ditribution graph
randT<-rt(3000,df=NROW(tips)-1)
tipTTest<-t.test(tips$tip, alternative="two.sided",mu=2.50)
require(ggplot2)
ggplot(data.frame(x=randT))+
  geom_density(aes(x=x), fill="grey",color="grey")+
  geom_vline(xintercept = tipTTest$statistic)+
  geom_vline(xintercept = mean(randT)+c(-2,2)*sd(randT),linetype=2)

t.test(tips$tip, alternative="greater", mu=2.5)

#15.3.2 two sample variables case
aggregate(tip~sex, data=tips,var)
shapiro.test(tips$tip)
shapiro.test(tips$tip[tips$sex == "Female"])
shapiro.test(tips$tip[tips$sex == "Male"])
ggplot(tips, aes(x=tip,fill=sex))+
         geom_histogram(binwidth=.5,alpha=1/2)
ansari.test(tip~sex,tips)
t.test(tip~sex, data=tips,var.equal = TRUE)

#simple s.d. comparison
require(plyr)
tipsummary<-ddply(tips,"sex",summarize,
                  tip.mean=mean(tip),tip.sd=sd(tip),
                  Lower=tip.mean-2*tip.sd /sqrt(NROW(tip)),
                  Upper=tip.mean+2*tip.sd /sqrt(NROW(tip)))
tipsummary
#visualize
ggplot(tipsummary, aes(x=tip.mean, y=sex))+geom_point()+
  geom_errorbarh(aes(xmin=Lower,xmax=Upper), height=.2)

#15.3.3 t-test : two variables case 
require(UsingR)
head(father.son)
t.test(father.son$fheight,father.son$sheight, paried=TRUE)

heightDiff <- father.son$fheight - father.son$sheight
ggplot(father.son, aes(x=fheight - sheight)) + 
  geom_density()+
  geom_vline(xintercept = mean(heightDiff))+
  geom_vline(xintercept = mean(heightDiff) + 2*c(-1,1)*sd(heightDiff)/sqrt(nrow(father.son)),linetype = 2)


#15.4 ANOVA
tipAnova <-aov(tip ~ day -1, tips)
tipintersept <- aov(tip~day, tips)
tipAnova$coefficients
tipintersept$coefficients
summary(tipAnova)

require(plyr)
tipsByDay <-ddply(tips,"day",summarize,
                  tip.mean=mean(tip), tip.sd=sd(tip),
                  Length = NROW(tip),
                  tfrac = qt(p=.90, df = Length -1),
                  Lower = tip.mean - tfrac*tip.sd/sqrt(Length),
                  Upper = tip.mean + tfrac*tip.sd/sqrt(Length))
ggplot(tipsByDay, aes(x = tip.mean, y = day)) + geom_point()+
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height=0.3)

#16 linear regression
require(UsingR)
require(ggplot2)
head(father.son)
ggplot(father.son, aes(x = fheight, y = sheight)) + geom_point()+
  geom_smooth(method = "lm") + labs (x = "Fathers", y = "sons")

heightLM <- lm


