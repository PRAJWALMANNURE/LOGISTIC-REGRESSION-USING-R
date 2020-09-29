ccard <- read.csv("C:/Users/PJ/Desktop/Assignmens/logistic regression/creditcard.csv")
View(ccard)
ccard <- ccard[,-1]
ccard$card <- ifelse(ccard$card=='yes',1,0)

sum(is.na(ccard))

dim(ccard)

library(caTools)
split <- sample.split(ccard,SplitRatio = 0.8)
split

train <- subset(ccard,split='TRUE')
test <- subset(ccard,split='FALSE')

model <- glm(card~.,train,family = 'binomial')
summary(model)

res <- predict(model,train,type='response')
confusion<- (table(actualvalue=train$card,predictedvalue=res>0.9))
confusion
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy #[1] 0.8597422 its a better model with all the inputs are significant

##finding threshold value########
library(ROCR)
ROCRpred <- prediction(res,train$card)
ROCRpref <- performance(ROCRpred,'tpr','fpr')

plot(ROCRpref,colorize=TRUE,print.cutoffs.at=seq(0.1 ,by = 0.1))

model <- glm(card~.,test,family = 'binomial')
summary(model)

res <- predict(model,test,type='response')
res 
test

#################################################################
