# include libraries
library(tidyr)
library(base)
library(class)
library(curl)
library(devtools)
library(DMwR)
library(dplyr)
library(e1071)
library(gbm)
library(randomForest)
library(MASS)
library(stringr)
library(stringi)
library(ggplot2)
library(klaR)

welldf <- read.csv("1718_wellness.csv", header=TRUE)
###### clean data set to make variables usable
## set bed time and wake time as usable times 
welldf$Bed.Time <- as.character(welldf$Bed.Time) 
welldf$Wake.Time <- as.character(welldf$Wake.Time)
convertTimes <- function(timevector){
  for(i in 1:length(timevector)){
    hr <- str_extract(timevector[i], ".*:")
    hr <- gsub(":","",hr)
    minute <- str_extract(timevector[i], ":.+[0-9]")
    minute <- gsub(":","",minute)
    if(minute == "15"){
      minute <- "25"
    }else if(minute == "30"){
      minute <- "50"
    }else if(minute == "45"){
      minute <- "75"
    }
    hr <- as.double(hr)
    minute <- as.double(minute)
    
    if(str_detect(timevector[i], "PM")){
      hr <- hr + 12
    }
    if(hr == 12){ hr <- 0}
    if(hr == 24){ hr <- 12 }
    timevector[i] <- as.double(paste(hr,minute,sep="."))
  }
  return(as.double(timevector))
}

welldf$name <- as.factor(welldf$name)
welldf$Bed.Time <- convertTimes(welldf$Bed.Time)
welldf$Wake.Time <- convertTimes(welldf$Wake.Time)

fillSleepHours <- function(df){
  # col nums for Bed.Time(7), Wake.Time(8), Sleep.hours(9)
  # find number of rows
  nrows <- length(df[,9])
  for(i in 1:nrows){
    if(is.na(df[i,9])){
      df[i,9] <- calculateSleepTime(df[i,7], df[i,8])
    }
  }
  return(df)
}

calculateSleepTime <- function(bedtime, waketime){
  if(bedtime > waketime){
    sleeptime <- (24-bedtime)+waketime
  }else{
    sleeptime <- waketime - bedtime
  }
  return(sleeptime)
}
welldf <- fillSleepHours(welldf)  # fill empty sleep hours

## order all ordered factors
# they come ordered
# order fatigue
welldf$Fatigue <- factor(welldf$Fatigue, levels=c("1- Exhausted",
                                                  "2",
                                                  "3",
                                                  "4- Average",
                                                  "5",
                                                  "6",
                                                  "7- Fresher Than Usual"),
                         ordered = TRUE)
welldf$Desire <- factor(welldf$Desire, levels=c("1- Lower Than Usual",
                                                "2",
                                                "3",
                                                "4- Average",
                                                "5",
                                                "6",
                                                "7- Higher Than Usual"),
                        ordered = TRUE)

welldf$Soreness <- factor(welldf$Soreness, levels=c("1- Sorer Than Usual",
                                                    "2",
                                                    "3",
                                                    "4- Average",
                                                    "5",
                                                    "6",
                                                    "7- Better Than Usual"),
                          ordered = TRUE)

welldf$Irritablity <- factor(welldf$Irritablity, levels=c("1- More Irritable Than Usual",
                                                          "2",
                                                          "3",
                                                          "4- Average",
                                                          "5",
                                                          "6",
                                                          "7- Better Mood Than Usual"),
                             ordered = TRUE)

welldf$Sleep.Quality <- factor(welldf$Sleep.Quality, levels=c("1- Restless",
                                                              "2",
                                                              "3",
                                                              "4- Average",
                                                              "5",
                                                              "6",
                                                              "7- Deep & Restful"),
                               ordered = TRUE)



welldf$date <- as.character(welldf$date)
# create month category to easily check things by month
extractMonths <- function(datevec){
  monthvec <- str_extract_all(datevec, "-[0-9][0-9]-")
  monthvec <- gsub("-","",monthvec)
  monthvec <- as.numeric(monthvec)
  return(monthvec)
}
welldf %>% mutate(month = extractMonths(date)) -> welldf # create month column

#### adjust monitoring score to remove fatigue to avoid accidentally using it in models
welldf %>% mutate(Monitoring.Score = as.numeric(Soreness)+as.numeric(Desire)+
                    as.numeric(Irritablity)+as.numeric(Sleep.Quality)) -> welldf

### fatigue by month ###
welldf %>% group_by(month) %>% summarise(
  meanFatigue = mean(as.numeric(Fatigue)),
  medFatigue = median(as.numeric(Fatigue)),
  sdFatigue = sd(as.numeric(Fatigue)) ) -> fatigueSummaryByMonth # fatigue by month

welldf %>% group_by(Fatigue) %>% summarise( count = n()) -> fatiguedf

# impute NA Desire & Sleep.Quality
welldf %>% group_by(name, month) %>% 
  summarise(DesXmonthXname = median(as.numeric(Desire), na.rm=FALSE ), 
          SQXmonthXname = median(as.numeric(Sleep.Quality), na.rm=TRUE)) -> DesnSQTable

for(i in 1:length(welldf$Sleep.Quality)){
  if( is.na(welldf[i, 10])){
    
  }
}
  

## check for any remaining missing data 
# lots in USG and menstruation

###### split training set and testing set
createTrainTestSet <- function(df){
  totalvec <- c(1:length(df[,1]))
  trainvec <- sample(totalvec, size = length(totalvec)*0.7, replace = FALSE)
  trainvec <- trainvec[order(trainvec, decreasing = FALSE)]
  testvec <- totalvec[which(!(totalvec %in% trainvec))]
  out <- list()
  out$trainset <- df[trainvec,]
  out$testset <- df[testvec,]
  return(out)
}
newdf <- createTrainTestSet(welldf)
traindf <- newdf$trainset # new train set
testdf <- newdf$testset # new test set
str(traindf) # 3813 x 21
str(testdf) # 1635 x 21 
str(welldf) # 5448 * 21

# check accuracy of model 
checkAccuracy <- function(truevals, predvals){
  countright = 0
  for(i in 1:length(truevals)){
    if(as.numeric(truevals[i])==as.numeric(predvals[i])){
      countright = countright+1
    }
  }
  return(countright/length(truevals))
}

welldf$name <- as.factor(welldf$name)
## mse function 
mymse <- function(truevals, predvals){
  truevals <- as.numeric(unlist(truevals))
  predvals <- as.numeric(unlist(predvals))
  sumdifsquared <- 0
  for(i in 1:length(truevals)){
    sumdifsquared <- (abs(truevals[i]-predvals[i])^2)+sumdifsquared
  }
  mseout <- sumdifsquared/length(truevals)
  return(mseout)
}

## compute all rmse's 
## simulate numSims times for all models
# extremely slow - dont go above 20
simModelTestN <- function(ovrdf, numSims){
  summarystats <- NULL
  polrMSE <- NULL
  bayesMSE <- NULL
  forestMSE <- NULL
  polrAcc <- NULL
  bayesAcc <- NULL
  forestAcc <- NULL
  for(i in 1:numSims){
    # create train/test set
    tempovr <- createTrainTestSet(ovrdf)
    temptrain <- tempovr$trainset
    temptest <- tempovr$testset
    # train models
    tempPropOddsSimple <- polr(formula = Fatigue~Sleep.Quality+Desire+month+
                                 name,
                               data=temptrain,
                               method="logistic")
    tempForestMod <- randomForest(Fatigue~Sleep.Quality+Desire+month+
                                    name,
                                  data=temptrain)
    tempNaiveModel <- naiveBayes(Fatigue~Sleep.Quality+Desire+month+
                                   name, 
                                 data=temptrain )
    # run models 
    tempPropPredict <- predict(tempPropOddsSimple, temptest)
    tempForestPredict <- predict(tempForestMod, temptest)
    tempNaivePredict <- predict(tempNaiveModel, temptest)
    # take mse's of each
    polrMSE[i] <- mymse(temptest$Fatigue, tempPropPredict)
    forestMSE[i] <- mymse(temptest$Fatigue, tempForestPredict)
    bayesMSE[i] <- mymse(temptest$Fatigue, tempNaivePredict)
    # add as row i to summary stats
    polrAcc[i] <- checkAccuracy(temptest$Fatigue, tempPropPredict)
    forestAcc[i] <- checkAccuracy(temptest$Fatigue, tempForestPredict)
    bayesAcc[i] <- checkAccuracy(temptest$Fatigue, tempNaivePredict)
    ## OR add each to vector of model RMSE vector
  }
  ## cbind all model rmse vectors
  summarystats <- cbind(polrMSE,forestMSE,bayesMSE,polrAcc,forestAcc,bayesAcc)
  return(summarystats)
}

# individual player data
welldf %>% filter(name == "5") -> player5
welldf %>% filter(name == "16") -> player16 
welldf %>% filter(name == "3") -> player3
welldf %>% filter(name== "15") -> player15
welldf %>% filter(name=="11") -> player11
welldf %>% filter(name=="13") -> player13
welldf %>% filter(name=="2") -> player2
welldf %>% filter(name=="14") -> player14
welldf %>% filter(name=="7") -> player7
welldf %>% filter(name=="4") -> player4
welldf %>% filter(name=="10") -> player10
welldf %>% filter(name=="1") -> player1
welldf %>% filter(name=="6") -> player6
welldf %>% filter(name=="17") -> player17
welldf %>% filter(name=="8") -> player8
welldf %>% filter(name=="12") -> player12
welldf %>% filter(name=="9") -> player9


## explain model training methods 
forestExplained <- "Random Forest Classification is a machine-learning method for 
classification.  In short this method generates decision trees each using a different
subset of your training data. The trees are used to make a decision on the classification
of the response variable by selecting the most common output of the variables associated with
the response. Essentially the random forest predicts multiple outcomes from a single given 
input but will use these outcomes like votes to see how it should classify this input."
bayesExplained <- "Naive Bayes Classification produces the conditional probability of each
fatigue level based on the unique combination of factors from that observation. As can 
be assumed by the name it uses Bayes theorem to give probabilities to the classification
of the response variable such that each level of response has a probability of belonging
to a given set of inputs and whichever level has the highest probability in this case is 
selected."
regressionExplained <- "Ordinal Multinomial Regression or Multinomial Logistic Regression
are methods of logistic regression which are used to model predictions when the response
is an ordered factor. The method we used in particular was the proportional odds method of
multinomial logistic regression."
justificationExplained <- "To examine and compare our models, for each model we separated
the data randomly into training and test sets - 70% and 30% of the data respectively.
 We trained our 3 modelling methods on training sets and used Mean Square Error of these
 models' predictions on our test sets as well as an actual accuracy rate (proportion
 predicted correctly) as a sort of two-pronged approach for model analysis.  One to view
actual accuracy and one to see the average squared error side by side.  We also wanted to 
examine how these methods stood up against eachother with different training and test sets
so we could be very confident in our evaluations.  We created a simulation which creates
a new train and test set, with the same proportions as before. We fit our models on the 
training set and make our predictions on the test set. We summarize and store the MSE and
accuracy rate and repeat all of this n times. This is a very effective method to confidently
evaluate your different modelling method options however it is very time consuming."



