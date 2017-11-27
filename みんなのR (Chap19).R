
## Chap19: Shrinkage

acs <- read.table("http://jaredlander.com/data/acs_ny.csv", sep=",",
                  header=TRUE, stringsAsFactors=FALSE)
testFrame <- data.frame(First=sample(1:10,20,replace=TRUE),
                        Second=sample(1:20,20,replace=TRUE),
                        Third=sample(1:10,20,replace=TRUE),
                        Fourth=factor(rep(c("Alice", "Bob", "Charlie","David"),
                                          5)),
                        Fifth=ordered(rep(c("Edward","Frank","Georgia","Hank","Isaac"),4)),
                        Sixth=rep(c("a","b"),10),stringsAsFactors = F)
head(testFrame)
head(model.matrix(First~Second+Fourth+Fifth,data=testFrame))

# use levels for all variables
require(useful)
head (build.x(First~Second+Fourth+Fifth, data=testFrame, contrasts=FALSE))
#use levels only for Fourth
head (build.x(First~Second+Fourth+Fifth, data=testFrame, contrasts=c(Fourth=FALSE,Fifth=TRUE)))

acs$Income <- with(acs, FamilyIncome>=150000)
head(acs)

#predicted matrix ("—\‘ªˆöqs—ñ")
# axis is automatically added by glmnet function
acsX <- build.x(Income ~ NumBedrooms + NumChildren + NumPeople + 
                  NumRooms + NumUnits + NumVehicles + NumWorkers +
                  OwnRent * YearBuilt + ElectricBill + FoodStamp +
                  HeatingFuel + Insurance + Language -1,
                data = acs, contrasts = FALSE)

# Confirmation of class and level
class(acsX)
dim(acsX)
topleft(acsX, c=6)
topright(acsX, c=6)

# generate response predict factor
acsY <- build.y (Income ~NumBedrooms + NumChildren + NumPeople +
                   NumRooms + NumUnits + NumVehicles + NumWorkers+
                   OwnRent + YearBuilt + ElectricBill + FoodStamp +
                   HeatingFuel + Insurance + Language -1, data=acs)
head(acsY)
tail(acsY)

# Cross-validation package installation
install.packages("glmnet")
require(glmnet)
set.seed(1863561)
#cross-validation by glmnet
acsCV1 <- cv.glmnet(x=acsX, y=acsY, family="binomial", nfold=5)
acsCV1$lambda.min
acsCV1$lambda.1se
plot(acsCV1)
# coefficient plot
coef(acsCV1, s="lambda.1se")

# coefficient path plot
plot(acsCV1$glmnet.fit, xvar="lambda")
abline(v=log(c(acsCV1$lambda.min,acsCV1$lambda.1se)),lty=2)
## insert a line ƒÉ minimizing error, another line max ƒÉ wityhin 1se from error

# Ridge model 
set.seed(71623)
acsCv2<-cv.glmnet(x=acsX,y=acsY, family="binomial", nfold=5,
                  alpha=0)
## check ƒÉ value
acsCv2$lambda.min
acsCv2$lambda.1se

## check coefficient value
coef(acsCv2, s="lambda.1se")

## cross-valudation error
plot(acsCv2)

## coefficient path
plot(acsCv2$glmnet.fit,xvar="lambda")
abline(v=log(c(acsCv2$lambda.min,acsCv2$lambda.1se)),lty=2)


## computation of optimal ƒ¿
require(parallel)
install.packages ("doParallel")
require(doParallel)

## random number generation
set.seed(2834673)
##  
theFolds <-sample(rep(x=1:5,length.out = nrow(acsX)))
## set marix of ƒ¿
alphas<-seq(from=0.5,to=1,by=0.05)

## random number set(reproductivity of random results)
set.seed(5127151)
# start cluster in two workers
cl <- makeCluster(2)
## resiter cluster
registerDoParallel(cl)
## record beginning time
before <- Sys.time()

## set foreach loop 

acsDouble <- foreach(i=1:length(alphas),.errorhandling = "remove",
                     .inorder = FALSE, .multicombine = TRUE,
                     .export = c("acsX","acsY","alphas","theFolds"),
                     .packages = "glmnet") %dopar%

{ 
  print(alphas[i])
  cv.glmnet(x=acsX, y=acsY, family=binomial,nfolds=5,
            foldid=theFolds, alpha=alphas[i])
  }


# record stop time
after <- Sys.time()
# stop cluster
stopCluster(cl)

# stop time - beginning time
after - before

sapply(acsDouble, class)
extractGlmnetInfo <- function(object)
{
  lambdaMin <- object$lambda.min
  lambda1se <- object$lambda.1se
  
  whichMin <- which(object$lambda == lambdaMin)
  which1se <- which(object$lambda == lambda1se)
  
  data.frame(lambda.min=lambda$Min, err.min=object$cvm[whichMin],
             lambda.1se=lambda1se,error.1se=object$cvm[which1se])
  }

alphaInfo <- Reduce(rbind,lapply(acsDouble,extractGlmnetInfo))
alphaInfo2 <- plyr::ldply(acsDouble, extractGlmnetInfo)
identical (alphaInfo,alphaInfo2)

alphaInfo$alpha <-alphas
alphaInfo

require(reshape2)
require(stringr)

alphaMelt <- melt(alphaInfo, id.vars="alpha", value.name="Value",
                  variable.name="Measure")
alphaMelt$Type <- str_extract(string=alphaMelt$Measure,
                              pattern="(min|(1se")
alphaMelt$Measure <- str_replace(string=alphaMelt$Measure,
                                 pattern="nn.(min|1se)",
                                 replacement="")
alphaMelt$Cast <- dcast(alphaMelt,alpha+Type ? Measure, 
                                 value.var="Value")

ggplot(alphaCast, aes(x=Alpha, y=error))+
  geom_line(aes(group=Type))+
  facet_wrap(~Type,scales="free_y",ncol=1)+
  geom_point(aes(size=lambda))


set.seed(5127151)
acsCV3 <- cv.glmnet(x=acsX,y=acsY,family="binomial",nfold=5,
                    alpha=alphaInfo$alpha[which.min(alphaInfo$error.1se)])


## 19.2 Beysian Shrinkage
load("C:/Users/kojikm.mizumura/Downloads/ideo.rdata")
head(ideo)

theYears <- unique(ideo$Year)
results <- vector(mode="list",length=length(theYears))
names(results) <-theYears

for (i in theYears)
{
  results[[as.character(i)]] <- glm(Vote~Race+Income+Gender+
                                      Education,
                                    data=ideo,subset=Year==i,
                                    family=binomial(link="logit"))
}

#coefficient plot
require(coefplot)
voteInfo <- multiplot(results, coefficients="Raceblack",plot=FALSE)
head(voteInfo)
multiplot(results, coefficients="Raceblack", secret.weapon = TRUE)+
  coord_flip(xlim=c(-20,10))

install.packages("arm")
resultsB <- vector(mode="list",length=length(theYears))
names(resultsB) <- theYears

for(i in theYears)
{ resultsB[as.character(i)]<-
   arm::bayesglm(Vote~Race+Income+Gender+Education,
                data=ideo[ideo$Year==i,],
                family=binomial(link="logit"),
                prior.scale=2.5,prior.df=1)}

# coefficient plot
multiplot(resultsB, coefficients="Raceblack",secret.weapon = TRUE)







