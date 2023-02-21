########################################################
### Case: 2008 Democratic Primaries - Clinton vs. Obama
########################################################
source("DataAnalyticsFunctions.R")
# read data into R
election_data <- read.csv("ElectionDataAlone.csv")

# Next use the function summary to inspect the data
summary(election_data)

##############################################
# Cleaning up the data
# Write a function that replaces NAs with the mean of the non-missing data 
# in the column. This function can be called for different data sets to 
# impute the data.
impute_data <- function(vec, mn) {
  ifelse(is.na(vec), mn, vec)
}
# Find the means for all the numeric columns. 
# The function sapply automatically runs the mean function 
# (specified as second argument) on the columns 10 through 41. 
# The means are then saved in the vector named train_data_mean. 
# We use the argument na.rm=TRUE to ask the function to ignore NA entries.
data_mean <- sapply(election_data[,10:41],mean, na.rm=TRUE)

# Run this command to look at means for all the columns we found by running the sapply function
(data_mean)

# Impute the missing data. Loop through all the rows and 
# for each column call the function impute_train_data.
for(i in 10:41) {
  election_data[,i]<-impute_data(election_data[,i],data_mean[i-9])
}
# Run the summary function again. Now you see that no demographic/county columns have NA entries.
summary(election_data)

# Create two separate data sets from the data in electionData.
election_data$ElectionDate <- as.Date(election_data$ElectionDate, format="%m/%d/%Y")
election_data_train <- election_data[election_data$ElectionDate < as.Date("2/19/2008", format="%m/%d/%Y"), ]
election_data_test <- election_data[election_data$ElectionDate >= as.Date("2/19/2008", format="%m/%d/%Y"), ]

# If you want to write these data sets back out into spreadsheets, 
# use the following "write" commands in R.
write.csv(electionDataTrain, "electionDataTrain.csv")
write.csv(electionDataTest, "electionDataTest.csv")

##########################################################
### End of Data Cleaning up
##########################################################
#
# Create some possible variables that might be of interest.
# (things we might like to predict in a regression using the demographic information). 
# These variables directly become a part of our data set election_data_train. You can use the command names(election_data_train) to see that these are added as columns in our data set.
election_data_train$Obama_margin <- election_data_train$Obama - election_data_train$Clinton
election_data_train$Obama_margin_percent <- 100*election_data_train$Obama_margin/election_data_train$TotalVote
election_data_train$Obama_wins <- ifelse(election_data_train$Obama_margin >0, 1,0)
names(election_data_train)
###
### Based on the data, to account for the size of possible delegates on each county
### we will work with election_data_train$Obama_margin_percent to be the target out models.
###

###
### Question 1: Provide a visualization (very flexible format, 
### it does not need to be related to the election)
### 
state_farm_area <- c()

for (state in states) {
  print(mean(election_data_train[election_data_train$State == state,]$FarmArea))
  state_farm_area <- append(state_farm_area, mean(election_data_train[election_data_train$State == state,]$FarmArea))
}

library("ggplot2")

ggplot(election_data_train, aes(x = Pop, y = MedianIncome, col = Obama_wins)) + geom_point()

qplot(data = election_data_train, x = State, y = MedianIncome, geom = "boxplot")
qplot(data = election_data_train, x = State, y = MedianIncome, geom = "boxplot")

boxplot(election_data$Age35to65[election_data$TotalVote], election_data$AgeBelow35[election_data$TotalVote], election_data$Age65andAbove[election_data$TotalVote], col="cornflowerblue")
boxplot(election_data$Age35to65[election_data$Obama], election_data$AgeBelow35[election_data$Obama], election_data$Age65andAbove[election_data$Obama], col="cornflowerblue")
boxplot(election_data$Age35to65[election_data$Clinton], election_data$AgeBelow35[election_data$Clinton], election_data$Age65andAbove[election_data$Clinton], col="pink")
boxplot(election_data$Age35to65[election_data$Clinton], election_data$AgeBelow35[election_data$Clinton], election_data$Age65andAbove[election_data$Clinton], col="pink")


###
### Question 2: Prediction. No additional script beyond the cleaning up the data
### provided above. (Hint: FIPS variable behaves as an identifier for each observation
### so you might not want to include it in a linear regression.)
###
library(glmnet)
library(randomForest)

drops <- c("County", "State", "Region", "FIPS", "ElectionDate", "ElectionType", "Obama_wins", "Obama_margin", "TotalVote", "Clinton", "Obama")

train_new <- election_data_train
train_new <- election_data_train[, !(names(election_data_train) %in% drops)]

nfold <- 10
n <- nrow(election_data_train)

foldid <- rep(1:nfold, each = ceiling(n/nfold))[sample(1:n)]

results <- data.frame(linear.model = rep(NA,nfold), linear.model.lasso = rep(NA,nfold), 
                      random.forest = rep(NA,nfold), model.null = rep(NA, nfold))

My <- train_new$Obama_margin_percent
Mx <- model.matrix(Obama_margin_percent ~ ., data = train_new)[,-1]


for (k in 1:nfold) {
  train <- which(foldid != k)
  
  linear.model <- glm(Obama_margin_percent ~ ., 
                      data = train_new, subset = train)
  linear.model.lasso <- glmnet(Mx, My, data = train_new, subset = train, family = "gaussian")
  
  random.forest.reg <- randomForest(Obama_margin_percent ~ .,
                                    data = train_new, subset = train, importance = TRUE)
  
  model.null <- glm(Obama_margin_percent ~ 1, data = train_new)
  
  predict.linear <- predict(linear.model, newdata = train_new[-train,])
  predict.linear.lasso <- predict(linear.model.lasso, newx = Mx[-train,], family = "gaussian")
  predict.random.forest.reg <- predict(random.forest.reg, newdata = train_new[-train,], family = "gaussian")
  predict.null <- predict(model.null, newdata = train_new[-train,])
  
  results$linear.model[k] <- R2(train_new$Obama_margin_percent[-train], pred = predict.linear)
  results$linear.model.lasso[k] <- R2(My[-train], pred = predict.linear.lasso, family = "gaussian")
  results$random.forest[k] <- R2(train_new$Obama_margin_percent[-train], pred = predict.random.forest.reg)
  results$model.null[k] <- R2(train_new$Obama_margin_percent[-train], pred = predict.null)
  
  summary(linear.model.lasso)
  linear.model.lasso$lambda[1:5]
  
  results$linear.model[k]
  results$linear.model.lasso[k]
  results$random.forest[k]
  results$model.null[k]
}

boxplot(results[-2])


##LASSO TRY 2
source("DataAnalyticsFunctions.R")
library(randomForest)
library(libcoin)
library(partykit)
library(glmnet)

# We drop the columns since they will not help us with our predictive modeling
cols_to_drop <- c("County", "State", "Region", "FIPS", "ElectionDate", "ElectionType", 
                  "Obama_wins", "TotalVote", "Clinton", "Obama", "Obama_margin")
# Our new dataset
train_data <- election_data_train[,!(names(election_data_train) %in% cols_to_drop)]

# Matrices for regression
Mx <- model.matrix(train_data$Obama_margin_percent ~ ., data = train_data)[,-1]
My <- train_data$Obama_margin_percent

# Finding optimal value of lambda for lasso

lasso <- glmnet(Mx,My, family="gaussian")
summary(lasso)

# 1) Lambda Theory
num.features <- ncol(Mx)
n <- nrow(Mx)
num.Obama_wins <- sum(My > 0)
w <- (num.Obama_wins/n)*(1-(num.Obama_wins/n))
#### For the binomial case, a theoretically valid choice is
lambda.theory <- sqrt(w*log(num.features/0.05)/n)

lassoTheory <- glmnet(Mx,My, family="gaussian",lambda = lambda.theory)


# 2) searching for lambda value
lassoCV <- cv.glmnet(Mx, My, family = "gaussian")

# Plotting the fitting graph 
par(mar=c(1.5,1.5,2,1.5))
par(mai=c(1.5,1.5,2,1.5))
plot(lassoCV, main="Fitting Graph for CV Lasso \n \n # of non-zero coefficients  ", xlab = expression(paste("log(",lambda,")")))

# We will also take the minimum of the mean values stored in lambda.min
# and 1se to the right stored in lambda.1se
lambda.min <- lassoCV$lambda.min
lambda.1se <- lassoCV$lambda.1se

OOS <- data.frame(PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold),
                  L.min=rep(NA,nfold), L.1se=rep(NA,nfold), L.theory=rep(NA,nfold),
                  LM=rep(NA,nfold), LM_Int=rep(NA,nfold), RF=rep(NA,nfold)) 

features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
length(features.min)
features.1se <- support(lasso$beta[,which.min( (lassoCV$lambda-lassoCV$lambda.1se)^2)])
length(features.1se) 
features.theory <- support(lassoTheory$beta)
length(features.theory)

data.min <- data.frame(Mx[,features.min],My)
data.1se <- data.frame(Mx[,features.1se],My)
data.theory <- data.frame(Mx[,features.theory],My)

nfold <- 10
foldid <- rep(1:nfold, each = ceiling(n/nfold))[sample(1:n)]

for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ### Post Lasso Estimates
  rmin <- glm(My ~., data = data.min, subset = train, family="gaussian")
  if ( length(features.1se) == 0) {  
    r1se <- glm(train$Obama_margin_percent  ~mean(train$Obama_margin_percent), data=churndata, subset=train, family="gaussian") 
  } 
  else {r1se <- glm(My~., data=data.1se, subset=train, family="gaussian")
  }
  
  if ( length(features.theory) == 0){ 
    rtheory <- glm(train$Obama_margin_percent~mean(train$Obama_margin_percent), data=churndata, subset=train, family="gaussian") 
  } else {rtheory <- glm(My~., data=data.theory, subset=train, family="gaussian") }
  
  
  predmin <- predict(rmin, newdata=data.min[-train,])
  pred1se  <- predict(r1se, newdata=data.1se[-train,])
  predtheory <- predict(rtheory, newdata=data.theory[-train,])
  
  OOS$PL.min[k] <- R2(y=My[-train], pred=predmin, family="gaussian")
  OOS$PL.1se[k] <- R2(y=My[-train], pred=pred1se, family="gaussian")
  OOS$PL.theory[k] <- R2(y=My[-train], pred=predtheory, family="gaussian")
  
  
  ### Lasso
  lassoMin  <- glmnet(Mx[train,], My[train], family="gaussian", lambda = lambda.min)
  lasso1se  <- glmnet(Mx[train,], My[train], family="gaussian", lambda = lambda.1se)
  lassoTheory <- glmnet(Mx[train,], My[train], family="gaussian", lambda = lambda.theory)
  
  predlassomin <- predict(lassoMin, newx=Mx[-train,])
  predlasso1se  <- predict(lasso1se, newx=Mx[-train,])
  predlassotheory <- predict(lassoTheory, newx=Mx[-train,])
  
  OOS$L.min[k] <- R2(y=My[-train], pred=predlassomin, family="gaussian")
  OOS$L.1se[k] <- R2(y=My[-train], pred=predlasso1se, family="gaussian")
  OOS$L.theory[k] <- R2(y=My[-train], pred=predlassotheory, family="gaussian")
  
  # Linear Model with and without interactions, Random Forest
  LM <- glm(Obama_margin_percent ~., data = train_data, subset = train, family = "gaussian")
  LM_Int <- glm(Obama_margin_percent ~.^2, data = train_data, subset = train, family = "gaussian")
  RF <- randomForest(Obama_margin_percent ~ ., data = train_data, subset = train, importance = TRUE)
  
  predLM <- predict(LM, newdata=train_data[-train,])
  predLM_Int <- predict(LM_Int, newdata=train_data[-train,])
  predRF <- predict(RF, newdata=train_data[-train,])
  
  OOS$LM[k] <- R2(y=train_data$Obama_margin_percent[-train], pred=predLM, family="gaussian")
  OOS$LM_Int[k] <- R2(y=train_data$Obama_margin_percent[-train], pred=predLM_Int, family="gaussian")
  OOS$RF[k] <- R2(y=train_data$Obama_margin_percent[-train], pred=predRF, family="gaussian")
  
  print(k)
}


par( mar=  c(8, 4, 4, 2) + 0.6 )
barplot(colMeans(OOS), las=2,xpd=FALSE, ylim=c(0,1) , xlab="", ylab = bquote( "Average Out of Sample " ~ R^2))

par( mar=  c(8, 4, 4, 2) + 0.6 )
boxplot(OOS, las=2,xpd=FALSE, ylim=c(0,1) , xlab="", ylab = bquote( "Boxplot Out of Sample " ~ R^2))

### Clearly Random Forest is the winner
RF_Final <- randomForest(Obama_margin_percent ~ ., data = train_data, importance = TRUE)


###
### Question 3: Keep in mind that unsupervised learning 
### is used to explore the data. Feel free to consider only a subset of the 
### demographic variables. 
###

###
### Question 4(a) impact of changing hispanic demographic
###
#### Model with 1771 controls to measure the impact of 10% larger Hispanic demographic
y <- election_data_train$Obama_margin_percent
x <- model.matrix( Obama_margin_percent ~ .-Hispanic-Obama_wins-Obama_margin-FIPS-ElectionDate-TotalVote-Clinton-Obama, data = election_data_train )
d <- election_data_train$Hispanic
####
#### Feel free to compare/contrast your results with the following simple regression model
#### 
HispanicSimple <- glm( Obama_margin_percent ~ Hispanic, data = election_data_train )
summary(HispanicSimple)

####
### Question 4(b) impact of changing black demographic
####
#### Model with 1771 controls to measure the impact of 10% larger Black demographic
y <- election_data_train$Obama_margin_percent
x <- model.matrix( Obama_margin_percent ~ .-Black-Obama_wins-Obama_margin-FIPS-ElectionDate-TotalVote-Clinton-Obama, data = election_data_train )
d <- election_data_train$Black
####
#### Feel free to compare/contrast your results with the following simple regression model
#### 
BlackSimple <- glm( Obama_margin_percent ~ Black, data = election_data_train )
summary(BlackSimple)
####


####
#### Question 5: No additional R code. Keep in mind that you can build upon your previous 
#### analysis or develop new analysis.
####

