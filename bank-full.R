# LOADING THE DATA SET
bank <- read.csv(file.choose())
View(bank)
colnames(bank)

# CONVERTING UNKNOWN FACTOR TO NA
bank$education[bank$education=='unknown'] <- NA  
bank$contact[bank$contact=='unknown'] <- NA
bank$poutcome[bank$poutcome=='unknown'] <-  NA
bank$job[bank$job=='unknown'] <- NA
bank$age[bank$age=='unknown'] <- NA
bank$marital[bank$marital=='unknown'] <- NA
bank$balance[bank$balance=='unknown'] <- NA


# CONVERTING THE FACTOR DATA TO NUMERICAL
bank$marital <- as.numeric(bank$marital)
bank$education <- as.numeric(bank$education)
bank$housing <- ifelse(bank$housing=='yes',1,0)
bank$loan <- ifelse(bank$loan=='yes',1,0)
bank$month <- as.numeric(bank$month)
bank$y <- ifelse(bank$y=='yes',1,0)

summary(bank)
dim(bank)
sum(is.na(bank)) # [1] 52124 because of large number of NA we cant ommit


# IMPUTATION TO FILL NA VALUES
library(Hmisc)
bank$job<- impute(bank$job,median)
bank$age <- impute(bank$age,mean)
bank$marital <- impute(bank$marital,median)
bank$education <- impute(bank$education,median)
bank$default <- impute(bank$default,median)
bank$housing <- impute(bank$housing,median)
bank$loan <- impute(bank$loan,median)
bank$contact <- impute(bank$contact,median)
bank$poutcome <- impute(bank$poutcome,median)

sum(is.na(bank))


# DIVIDING DATA INTO TRAIN AND TEST
library(caTools)
splitt <- sample.split(bank,SplitRatio = 0.8)
splitt

train <- subset(bank,splitt=='TRUE')
test <- subset(bank,splitt=='FALSE')

# MODEL CREATION
model <- glm(train$y~.,train,family = 'binomial')
summary(model)

res <- predict(model,train,type='response')
confusion<- (table(actualvalue=train$y,predictedvalue=res>0.3))
confusion
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy  #[1] 0.9010817


##finding threshold value########
library(ROCR)
ROCRpred <- prediction(res,train$y)
ROCRpref <- performance(ROCRpred,'tpr','fpr')

plot(ROCRpref,colorize=TRUE,print.cutoffs.at=seq(0.1 ,by = 0.1))

# TESTING DATA
model1 <- glm(test$y~.,test,family = 'binomial')
summary(model)

res <- predict(model1,test,type='response')
res 
test

