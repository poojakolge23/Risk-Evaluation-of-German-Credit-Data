

install.packages("lift")
library(tidyverse)
library(readxl)
library(dplyr)
library(rpart)
library(rpart.plot)
library(purrr)
library(tidyr)
library(ggplot2)
library(rpivotTable)
library(lift)

#Loading dataset

df<-read_excel("GermanCredit_assgt1_S19.xls")
str(df)

#CHecking for missing values
sapply(df,function(x) sum(is.null(x))) #No NUll Values.

#Let's check for NA now
sapply(df,function(x) sum(is.na(x))) %>% 

colSums(is.na(df))/nrow(df) > 0.2
#NEW_CAR, USED CAR, FURNITURE, RADIO , EDUCATION, RETRAINING, PERSONAL_STATUS have more than 20% NA values.

#Replacing NA value with 0 for the following columns : #NEW_CAR, USED CAR, FURNITURE, RADIO , EDUCATION, RETRAINING
df$NEW_CAR[is.na(df$NEW_CAR)] <- 0
df$USED_CAR[is.na(df$USED_CAR)] <- 0
df$FURNITURE[is.na(df$FURNITURE)] <- 0
df$`RADIO/TV`[is.na(df$`RADIO/TV`)] <- 0
df$EDUCATION[is.na(df$EDUCATION)] <- 0
df$RETRAINING[is.na(df$RETRAINING)] <- 0


#For Personal Status column lets impute the median.
summary(df$PERSONAL_STATUS) 
df$PERSONAL_STATUS[is.na(df$PERSONAL_STATUS)] <- median(df$PERSONAL_STATUS, na.rm=TRUE) 

#For Age column lets impute the median.
summary(df$AGE) 
df$AGE[is.na(df$AGE)] <- median(df$AGE, na.rm=TRUE) 




#------------------------------ Checking for correct data types ---------------------------------

glimpse(df)
df$`OBS#`<- as.numeric(df$`OBS#`)
df$CHK_ACCT<- as.factor(df$CHK_ACCT)
df$DURATION<- as.numeric(df$DURATION)
df$HISTORY<- as.factor(df$HISTORY)
df$NEW_CAR<- as.factor(df$NEW_CAR)
df$USED_CAR<- as.factor(df$USED_CAR)
df$FURNITURE<- as.factor(df$FURNITURE)
df$`RADIO/TV`<- as.factor(df$`RADIO/TV`)                         
df$EDUCATION<- as.factor(df$EDUCATION)                         
df$RETRAINING<- as.factor(df$RETRAINING)
df$AMOUNT<- as.numeric(df$AMOUNT)                         
df$SAV_ACCT<- as.factor(df$SAV_ACCT)
df$EMPLOYMENT<- as.factor(df$EMPLOYMENT)
df$INSTALL_RATE<- as.numeric(df$INSTALL_RATE)
df$PERSONAL_STATUS<- as.factor(df$PERSONAL_STATUS)
df$`CO-APPLICANT`<- as.factor(df$`CO-APPLICANT`)
df$GUARANTOR<- as.factor(df$GUARANTOR)
df$PRESENT_RESIDENT<- as.factor(df$PRESENT_RESIDENT)
df$REAL_ESTATE<- as.factor(df$REAL_ESTATE)
df$PROP_UNKN_NONE<- as.factor(df$PROP_UNKN_NONE)
df$AGE<- as.numeric(df$AGE)
df$OTHER_INSTALL<- as.factor(df$OTHER_INSTALL)
df$RENT<- as.factor(df$RENT)
df$OWN_RES<- as.factor(df$OWN_RES)
df$NUM_CREDITS<- as.numeric(df$NUM_CREDITS)
df$JOB<- as.factor(df$JOB)
df$NUM_DEPENDENTS<- as.factor(df$NUM_DEPENDENTS)
df$TELEPHONE<- as.factor(df$TELEPHONE)
df$FOREIGN<- as.factor(df$FOREIGN)
df$RESPONSE<- as.factor(df$RESPONSE)

glimpse(df)


k <- df[df$NEW_CAR==0 & df$USED_CAR==0 & df$FURNITURE==0 & df$`RADIO/TV`==0 & df$EDUCATION==0 & df$RETRAINING==0,]

credit <- df[-c(k$`OBS#`),]


glimpse(credit)


#---------------------------- UNivariate Analysis -----------------------
summary(credit)

#CHK_ACCT --> having too few values in '2' Level
#HISTORY --> values not equally distributed
#SAV_ACCT --> values not equally distributed
#EMPLOYMENT --> too few values in '0' level
#Arent below 2 variables just opposite of each other?
#REAL_ESTATE	--> Applicant owns real estate	Binary	 0: No, 1: Yes
#PROP_UNKN_NONE-->	Applicant owns no property (or unknown)	Binary	 0: No, 1: Yes

#663 are good cases, 
#282 are bad cases

#Let\s try and examine the density plots of the numeric variables.

m1<- credit %>%  keep(is.numeric) %>%  gather()


  ggplot(credit$RESPONSE,m1) +
  facet_wrap(~ key, scales = "free") +
  geom_density()

dev.off()

credit %>%  keep(is.factor) %>%  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar()

#---------------------------- Bi-variate Analysis -----------------------

credit %>% rpivotTable()

credit %>%  keep(is.numeric) %>%  gather()
install.packages("reshape2")
library(reshape2)
dev.off()
m <- melt(credit, id.vars = c("AGE", "AMOUNT", "DURATION","NUM_CREDITS","INSTALL_RATE"), measure.vars = c("RESPONSE"))
boxplot(AGE + AMOUNT + DURATION + NUM_CREDITS + INSTALL_RATE ~ value, m)

table(as.factor(credit$NUM_CREDITS))
t1<- table(credit$RESPONSE,credit$CHK_ACCT,credit$HISTORY)
plot(t1)
dev.off()
df1 <- credit %>%  keep(is.factor)
df.melt<-melt(df1,id="RESPONSE")
bar <- group_by(df.melt, variable, RESPONSE)%>%summarise(mean=mean(value))
ggplot(bar, aes(x=variable, y=mean, fill=factor(RESPONSE)))+geom_bar()



credit %>%
  select(DURATION, AMOUNT, INSTALL_RATE,AGE,NUM_CREDITS,NUM_DEPENDENTS,RESPONSE) %>%
  gather() %>%
  ggplot(aes(x = factor(credit$RESPONSE)
             , y = Value)) +
  geom_boxplot() +
  facet_wrap(~credit$RESPONSE
             , scales = "free_y")

dev.off()

credit %>%  keep(is.factor) %>%  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar()

#---------------------------- Creating Decision Trees -----------------------

rmodel1 <- rpart(RESPONSE ~ . , data = credit , method = "class", parms = list(split = 'information'))
rpart.plot::prp(rmodel1,type=5,extra = 1)
print(rmodel1)

plot(rmodel1,  uniform=TRUE,  main="Decision Tree for German Credit Rating")
text(rmodel1, use.n=TRUE, all=TRUE, cex=.7)

levels(credit$PRESENT_RESIDENT)



summary(rmodel1)
plotcp(rmodel1)
rmodel1$cptable

ptree<- prune(rmodel1,cp= rmodel1$cptable[which.min(rmodel1$cptable[,"xerror"]),"CP"])

ptree<- prune(rmodel1,cp= 0.01241135)
fancyRpartPlot(ptree, uniform=TRUE, main="Pruned Classification Tree")


plot(ptree,  uniform=TRUE,  main="Decision Tree for German Credit Rating")
text(ptree, use.n=TRUE, all=TRUE, cex=.7)
class(rmodel1)



rmodel1$cptable[which.min(rmodel1$cptable[,"xerror"]),"CP"]

rmodel1$variable.importance
rm(pred1)
pred1 <- predict(ptree, credit, type='prob')
colnames(pred1) <- c("Non-Default","Default")
p <- round(pred1)
pred <- cbind(pred1,p[,2])
colnames(pred) <- c("Non-Default","Default","Outcome")
pred <- as.data.frame(pred)
class(pred)
pred$Outcome

#Plotting the Decile Lift Chart
plotLift(credit$RESPONSE,pred$Outcome, cumulative = TRUE, n.buckets = 10)
TopDecileLift(pred$Outcome, credit$RESPONSE)

lift <- function(depvar, predcol, groups=10) {
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)}
  if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
  if(is.factor(predcol)) predcol <- as.integer(as.character(predcol))
  helper = data.frame(cbind(depvar, predcol))
  helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(depvar), funs(total = n(),
                                    totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}


dt = lift(credit$RESPONSE, pred$Outcome, groups = 10)

#Lift curve
valid1 <- credit$RESPONSE
valid1 <- as.data.frame(valid1)
valid1 <- cbind(valid1,pred$Outcome)
colnames(valid1) <- c("Outcome","Prediction")

order(valid1$Prediction, decreasing = T)

str(valid1)
valid1$Outcome <- as.character(valid1$Outcome)
valid1$Outcome <- as.numeric(valid1$Outcome)

valid1$cumsum <- cumsum(valid1$Outcome)


plot(seq(nrow(valid1)), valid1$cumsum, type = "l", xlab='#cases', ylab='#default')

#Expermenting with rpart. control params
#Checking for various values of minsplit = 5,10,15,20
rm(rmodel2)
dev.off()
rmodel2 <- rpart(RESPONSE ~ . , data = credit , method = "class",
                 parms = list(split = 'gini'),control= rpart.control(minsplit=10, maxdepth=20))
rmodel2$cptable
plotcp(rmodel2)        

plot(rmodel2,  uniform=TRUE,  main="Decision Tree for German Credit Rating")
text(rmodel2, use.n=TRUE, all=TRUE, cex=.7)
plotcp(rmodel2)        



#-----------------------------Sampling -------------------------------------------------
credit_old <- credit
credit <- credit[,2:30]
rm(creditTrn);rm(creditTst)

#655944638
set.seed(655944638)

nr=nrow(credit)

trnIndex = sample(1:nr, size = round(0.7*nr), replace=FALSE)  #get a random 70% sample of row-indices

creditTrn=credit[trnIndex,]                                       #training data with the randomly selected row-indices

creditTst = credit[-trnIndex,]                                    #test data with the other row-indicesdim(creditTrn)

dim(creditTrn)
dim(creditTst)


####------------------------Theoretical Threshold -----------------
costmat <- cbind(c(0,500),c(100,0))
colnames(costmat) <- c("Good","Bad")
rownames(costmat) <- c("Good","Bad")

#CTHRESH <- costmat[2,1]/(costmat[2,1]+costmat[1,2])
CTHRESH=0.4
pred1=predict(rmodel_1_ptree, creditTst, type='prob')

pred_thresh = ifelse(pred1[, '1'] >= CTHRESH, 1, 0)
cmTst = table( true=creditTst$RESPONSE,pred = pred_thresh)
mean(pred_thresh==creditTst$RESPONSE) #Accuracy of model on Test data

scoreTst = pred1[,'1']
rocPredTst = prediction(scoreTst, creditTst$RESPONSE)
perfROCTst=performance(rocPredTst, "tpr", "fpr")
plot(perfROCTst, main = "ROC Curve",col="steelblue")
lines(plot(perfROCTst, main = "ROC Curve",col="red"))

aucPerf = performance(rocPredTst, measure = "auc")
aucPerf@y.values

library(rpivotTable)
credit %>% rpivotTable()


manav_factor <- credit %>% keep(is.factor)
manav_num <- credit %>% keep(is.numeric)
save.image("manav.Rdata")


print(rmodel_1)

install.packages("ROCR")
library(ROCR)
######-------------------- Building the tree using rpart -------------------------------------------
dev.off()

par(mfrow=c(1,2))
rmodel_1 <- rpart(RESPONSE ~ . , data = creditTrn , method = "class", 
                  parms = list(split = 'information')
                  ,control= rpart.control(minsplit=10,minbucket = round(10/3),cp=0,maxdepth=5))
rmodel_1$variable.importance
summary(rmodel_1)

#Plotting the model
plot(rmodel_1,  uniform=TRUE,  main="Decision Tree for German creditTrn Rating")
text(rmodel_1, use.n=TRUE, all=TRUE, cex=.7)

#Checking the cp table & plotting Cp vs error graph
rmodel_1$cptable
plotcp(rmodel_1,main = "Cp vs Xerror : Full Decision tree")

######--------------------#Pruning the tree to take min xerror value -----------------------------
rmodel_1_ptree<- prune(rmodel_1,cp= rmodel_1$cptable[which.min(rmodel_1$cptable[,"xerror"]),"CP"])
rmodel_1_ptree$variable.importance

#Plotting the pruned decision tree
plot(rmodel_1_ptree,  uniform=TRUE,  main="Pruned Decision Tree for German creditTrn Rating")
text(rmodel_1_ptree, use.n=TRUE, all=TRUE, cex=.7)
rmodel_1_ptree$cptable
plotcp(rmodel_1_ptree,main = "Cp vs Xerror : Pruned tree") #Plotting the Cp vs Xerror for the pruned tree

#######--------------------Predicting the probabilities using the refined model ######--------------------
predTrn=predict(rmodel_1_ptree, creditTrn, type='class') #Predicting values for Training data
pred1 <- predict(rmodel_1_ptree, creditTst, type='prob') #Doing the same for test 
predTst=predict(rmodel_1_ptree, creditTst, type='class') #Predicting value for just '1' response variable  in test data

#######--------------------Creating the Confusion Matrix to find out the accuracy ######--------------------
table(true=creditTrn$RESPONSE,pred = predTrn) #Confusion matrix for Training data
cmTst <- table(true=creditTst$RESPONSE,pred = predTst) #Confusion matrix for Test data


#Accuracy

mean(predTrn==creditTrn$RESPONSE) #Accuracy of model on Training data
mean(predTst==creditTst$RESPONSE) #Accuracy of model on Test data


##--------------------- Calculating Accuracy parameter values from Confusion Matrix ------------
tpTst <- cmTst[2,2]
tnTst <- cmTst[1,1]
fpTst <- cmTst[1,2]
fnTst <- cmTst[2,1]

accTst <- (tpTst+tnTst)/(tpTst+tnTst+fpTst+fnTst)
precTst <- tpTst / (tpTst+fpTst);precTst
recTst <- tpTst / (tpTst+fnTst);recTst
f_Tst <- (2*precTst*recTst)/(precTst+recTst);f_Tst



#######-------------------- Creating a validation dataframe with Outcome and Predicted values.######--------------------

valid1 <- creditTst$RESPONSE
valid1 <- as.data.frame(valid1)
valid1 <- cbind(valid1,as.data.frame(predTst))
colnames(valid1) <- c("Outcome","Prediction")

order(valid1$Prediction, decreasing = T)

str(valid1)
valid1$Outcome <- as.character(valid1$Outcome)
valid1$Outcome <- as.numeric(valid1$Outcome)
valid1$Prediction <- as.character(valid1$Prediction)
valid1$Prediction <- as.numeric(valid1$Prediction)

valid1$cumsum <- cumsum(valid1$Outcome)


#######--------------------Creating nd Plotting the Cumulative Decile Lift Table ######--------------------

#Plotting a lift curve

plot(seq(nrow(valid1)), valid1$cumsum, type = "l", xlab='#cases', ylab='#default',main = "Lift Chart")


lift <- function(depvar, predcol, groups=10) {
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)}
  if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
  if(is.factor(predcol)) predcol <- as.integer(as.character(predcol))
  helper = data.frame(cbind(depvar, predcol))
  helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(depvar), funs(total = n(),
                                    totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}


dt = lift(creditTst$RESPONSE, valid1$Outcome, groups = 10)
View(dt)

plotLift(creditTst$RESPONSE,valid1$Outcome, cumulative = TRUE, n.buckets = 10, main = "Cumulative Lift Chart")


#######-------------------- Creating nd Plotting Gain Performance  ######--------------------


pred2<- prediction(valid1$Prediction, valid1$Outcome)
gainPerf<-performance(pred2, "tpr", "rpp")
plot(gainPerf, main="Gain chart")

#######--------------------Creating nd Plotting ROC Curve, AUC and Accuracy ######--------------------

scoreTst = pred1[,'1']
rocPredTst = prediction(scoreTst, creditTst$RESPONSE)
perfROCTst=performance(rocPredTst, "tpr", "fpr")
plot(perfROCTst, main = "ROC Curve")
#lines(plot(perfROCTst, main = "ROC Curve",col="red"))


#Finding the AUC Value
aucPerf = performance(rocPredTst, measure = "auc")
aucPerf@y.values


#Plotting Accuracy vs Cutoff curve
accPerf = performance(rocPredTst, measure = "acc")
plot(accPerf, main = "Accuracy Performance")



#------------------------------ Misclassification Cost Performance ------------------------------

costPerf = performance(rocPredTst, "cost")
rocPredTst@cutoffs[[1]][which.min(costPerf@y.values[[1]])]

table(true=creditTrn$RESPONSE,pred = predTrn)

costPerf = performance(rocPredTst, "cost", cost.fp = 500, cost.fn = 100)
rocPredTst@cutoffs[[1]][which.min(costPerf@y.values[[1]])]


#####------------Creating a dataframe with outcome and predicted probability -----------
valid2 <- as.data.frame(creditTst$RESPONSE)
x <- as.data.frame(pred1)

valid2 <- cbind(valid2,x[,c(2)])
View(valid2)
colnames(valid2) <- c("Outcome","Predicted")
?performance
head(valid2)
valid2<-valid2 %>% select(Outcome,Predicted) %>% arrange(desc(Predicted)) 
View(valid2)


##--------------------- Calculating Accuracy parameter values from Confusion Matrix ------------
tpTst <- cmTst[2,2]
tnTst <- cmTst[1,1]
fpTst <- cmTst[1,2]
fnTst <- cmTst[2,1]

accTst <- (tpTst+tnTst)/(tpTst+tnTst+fpTst+fnTst)
precTst <- tpTst / (tpTst+fpTst)
recTst <- tpTst / (tpTst+fnTst)


mean(pred_thresh==creditTst$RESPONSE) #Accuracy of model on Test data


##---------- C50 Decision Tree -------------

install.packages('libcoin', dependencies = T)
install.packages('C50', dependencies = T)
library(C50)
install.packages("lift")
library(lift)
install.packages("ROCR")
library('ROCR')


set.seed(5599750)
set.seed(6227750)

#Maha
set.seed(654481566)

#Manav
set.seed(655944638)



##------- DATA ------

credit <- subset(credit, select = -c(1))
dim(credit)

nr=nrow(credit)
#trnIndex = sample(1:nr, size = round(0.5*nr), replace=FALSE)
trnIndex = sample(1:nr, size = round(0.7*nr), replace=FALSE)
#trnIndex = sample(1:nr, size = round(0.8*nr), replace=FALSE)
# get a random 50% sample of row-indices
creditTrn=credit[trnIndex,]                                       #training data with the randomly selected row-indices
creditTst = credit[-trnIndex,]                                    #test data with the other row-indices

dim(creditTrn) 
dim(creditTst)


colnames(creditTst)[1]<-"Observation"
colnames(creditTst)<-gsub("/","",colnames(creditTst))
apply(creditTrn,2, function(x) any(is.null(x)))



##---- Generating Tree

## Full tree

c50tree <- C5.0(RESPONSE ~ ., data = credit, trials = 1)
plot(c50tree)
summary(c50tree)
c50tree


## Train Decision tree

c50tree <- C5.0(RESPONSE ~ ., data = creditTrn, trials = 1)
plot(c50tree)
summary(c50tree)
c50tree


## Rule based tree

ruleModel <- C5.0(RESPONSE ~ ., data = creditTrn, rules = TRUE)
summary(ruleModel)


plot(tree_mod, uniform=TRUE,  main="Decision Tree for Credit resp")
text(tree_mod, use.n=TRUE, all=TRUE, cex=.5)
tree_mod$variable.importance


##---- Controlling Parameters
cntl = C5.0Control(winnow = TRUE)
#subset = TRUE, 
#bands = 0, 

#noGlobalPruning = FALSE,
#CF = 0.25, 
#minCases = 2,
#fuzzyThreshold = FALSE, 
#sample = 0, 
#seed = sample.int(4096, size = 1) - 1L, 
#earlyStopping = TRUE, 
#label = "outcome"


c50tree <- C5.0(RESPONSE ~ ., data = creditTrn, control = C5.0Control(CF = 0.3, winnow = TRUE, minCases = 2), trials = 1)
plot(c50tree)
summary(c50tree)
c50tree




##---------------------  Evaluation -------------------##
#mydata <- credit 
#mydata <- creditTrn
mydata <- creditTst


##------- Lift curve

par(mfrow=c(3,2))

## Input Tree 
rm(pred1)

pred1 <- predict(c50tree, mydata, type='prob')

p <- round(pred1)
pred <- cbind(pred1,p[,2])
colnames(pred) <- c("Non-Default","Default","Outcome")
pred <- as.data.frame(pred)
valid1 <- mydata$RESPONSE
valid1 <- as.data.frame(valid1)
valid1 <- cbind(valid1,pred$Outcome)
colnames(valid1) <- c("Outcome","Prediction")
order(valid1$Prediction, decreasing = T)
str(valid1)
valid1$Outcome <- as.character(valid1$Outcome)
valid1$Outcome <- as.numeric(valid1$Outcome)
valid1$cumsum <- cumsum(valid1$Outcome)

plotLift(mydata$RESPONSE,pred$Outcome, cumulative = TRUE, n.buckets = 10, main = "Cumulative Lift Chart")

plot(seq(nrow(valid1)), valid1$cumsum, type = "l", xlab='#cases', ylab='#default',main = "Lift Chart")



#######--------------------Predicting the probabilities using the refined model ######--------------------
predTrn=predict(c50tree, creditTrn, type='class') #Predicting values for Training data
pred1 <- predict(c50tree, creditTst, type='prob') #Doing the same for test 
predTst=predict(c50tree, creditTst, type='class') #Predicting value for just '1' response variable  in test data

#Accuracy

mean(predTrn==creditTrn$RESPONSE) #Accuracy of model on Training data
mean(predTst==creditTst$RESPONSE) #Accuracy of model on Test data



##------ Gain chart

pred2<- prediction(valid1$Prediction, valid1$Outcome)
gainPerf<-performance(pred2, "tpr", "rpp")
plot(gainPerf, main="Gain chart")


##------ ROC Curve

## Input tree

scoreTst=predict(c50tree, mydata, type="prob")[,2]  
rocPredTst = prediction(scoreTst, mydata$RESPONSE)  
perfROCTst = performance(rocPredTst, "tpr", "fpr")      #True and false positive rate
plot(perfROCTst, main="ROC Curve")


##------- AUC 

#Accuracy
accPerf = performance(rocPredTst, measure = "acc")
plot(accPerf, main="Accuracy vs Cutoff")
aucPerf = performance(rocPredTst, measure = "auc")
aucPerf@y.values



##------- Confusion Matrix


cmTst <- table(true=creditTst$RESPONSE,pred = predTst) #Confusion matrix for Test data

## Calculating Accuracy parameter values from Confusion Matrix ------------
tpTst <- cmTst[2,2]
tnTst <- cmTst[1,1]
fpTst <- cmTst[1,2]
fnTst <- cmTst[2,1]

accTst <- (tpTst+tnTst)/(tpTst+tnTst+fpTst+fnTst)
precTst <- tpTst / (tpTst+fpTst);precTst
recTst <- tpTst / (tpTst+fnTst);recTst
f_Tst <- 2*precTst*recTst/(precTst+recTst);f_Tst




##-------  Cost Estimation


##-------  Cost base tree


set.seed(654481566)

cost_mat <- as.data.frame(matrix(c(0,5,1,0), nrow = 2))
rownames(cost_mat) <- colnames(cost_mat) <- c(0,1)
cost_mat

rm(costtree1)
#costtree <- C5.0(RESPONSE ~ ., data = creditTrn, trials = 1, costs = cm)
costtree <- C5.0(RESPONSE ~ ., data=creditTrn, method="class",parms = list(loss = cost_mat),
                 control = C5.0Control(winnow = FALSE,noGlobalPruning = FALSE))
summary(costtree)


c50tree <- C5.0(RESPONSE ~ ., data=creditTrn, rules = FALSE, 
                control = C5.0Control(subset = TRUE , winnow = FALSE,noGlobalPruning = FALSE),
                costs = matrix(c(0,500,100,0), nrow = 2))
summary(c50tree)

#When the cost argument is used in the main function, class probabilities derived 
#from the class distribution in the terminal nodes may not be consistent with the final predicted class. 
#For this reason, requesting class probabilities from a model using unequal costs will throw an error.


#CTHRESH <- costmat[2,1]/(costmat[2,1]+costmat[1,2])

CTHRESH=0.4
pred1=predict(c50tree, creditTst, type='prob')

pred_thresh = ifelse(pred1[, '1'] >= CTHRESH, 1, 0)
cmTst = table( true=creditTst$RESPONSE,pred = pred_thresh)
mean(pred_thresh==creditTst$RESPONSE) #Accuracy of model on Test data

scoreTst = pred1[,'1']
rocPredTst = prediction(scoreTst, creditTst$RESPONSE)
perfROCTst=performance(rocPredTst, "tpr", "fpr")
plot(perfROCTst, main = "ROC Curve",col="steelblue")
lines(plot(perfROCTst, main = "ROC Curve",col="red"))

aucPerf = performance(rocPredTst, measure = "auc")
aucPerf@y.values

tpTst <- cmTst[2,2]
tnTst <- cmTst[1,1]
fpTst <- cmTst[1,2]
fnTst <- cmTst[2,1]

accTst <- (tpTst+tnTst)/(tpTst+tnTst+fpTst+fnTst)
precTst <- tpTst / (tpTst+fpTst);precTst
recTst <- tpTst / (tpTst+fnTst);recTst
f_Tst <- 2*precTst*recTst/(precTst+recTst);f_Tst









##------- MISC ------



#---------------- R PART ------------------------


false_positive_cost <- 500

false_negative_cost <- 100

rpart_model <- rpart(RESPONSE ~.,data = creditTrn,method = "class", parms = list(split = "information",
                                                                                 loss = matrix(c( 0, false_negative_cost,false_positive_cost, 0),byrow = TRUE,nrow = 2)))


print(rpart_model)
plot(rpart_model, uniform = TRUE, main = "DT")
text(rpart_model, use.n=TRUE, all=TRUE, cex =0.7)

pred1=predict(rpart_model, creditTst, type='prob')
rpart_model$cptable
plotcp(rpart_model,main = "Cp vs Xerror : Full Decision tree")

######--------------------#Pruning the tree to take min xerror value -----------------------------
rpart_ptree<- prune(rpart_model,cp= rpart_model$cptable[which.min(rpart_model$cptable[,"xerror"]),"CP"])
rpart_ptree$variable.importance

#Plotting the pruned decision tree
plot(rpart_ptree,  uniform=TRUE,  main="Decision Tree for German creditTrn Rating")
text(rpart_ptree, use.n=TRUE, all=TRUE, cex=.7)
rpart_ptree$cptable
rpart.plot::prp(rpart_ptree,type=4,extra = 2)

plotcp(rpart_ptree,main = "Cp vs Xerror : Pruned tree") #Plotting the Cp vs Xerror for the pruned tree



#######--------------------Predicting the probabilities using the refined model ######--------------------
predTrn=predict(rpart_ptree, creditTrn, type='class') #Predicting values for Training data
pred1 <- predict(rpart_ptree, creditTst, type='prob') #Doing the same for test 
predTst=predict(rpart_ptree, creditTst, type='class') #Predicting value for just '1' response variable  in test data


#######--------------------Creating the Confusion Matrix to find out the accuracy ######--------------------
table(true=creditTrn$RESPONSE,pred = predTrn) #Confusion matrix for Training data
cmTst <- table(true=creditTst$RESPONSE,pred = predTst) #Confusion matrix for Test data


#Accuracy

mean(predTrn==creditTrn$RESPONSE) #Accuracy of model on Training data
mean(predTst==creditTst$RESPONSE) #Accuracy of model on Test data


##--------------------- Calculating Accuracy parameter values from Confusion Matrix ------------
tpTst <- cmTst[2,2]
tnTst <- cmTst[1,1]
fpTst <- cmTst[1,2]
fnTst <- cmTst[2,1]

accTst <- (tpTst+tnTst)/(tpTst+tnTst+fpTst+fnTst)
precTst <- tpTst / (tpTst+fpTst);precTst
recTst <- tpTst / (tpTst+fnTst);recTst
f_Tst <- 2*precTst*recTst/(precTst+recTst);f_Tst



#######-------------------- Creating a validation dataframe with Outcome and Predicted values.######--------------------

valid1 <- creditTst$RESPONSE
valid1 <- as.data.frame(valid1)
valid1 <- cbind(valid1,as.data.frame(predTst))
colnames(valid1) <- c("Outcome","Prediction")
order(valid1$Prediction, decreasing = T)
str(valid1)
valid1$Outcome <- as.character(valid1$Outcome)
valid1$Outcome <- as.numeric(valid1$Outcome)
valid1$Prediction <- as.character(valid1$Prediction)
valid1$Prediction <- as.numeric(valid1$Prediction)
valid1$cumsum <- cumsum(valid1$Outcome)


#######--------------------Creating nd Plotting the Cumulative Decile Lift Table ######--------------------

#Plotting a lift curve

plot(seq(nrow(valid1)), valid1$cumsum, type = "l", xlab='#cases', ylab='#default',main = "Lift Chart")

lift <- function(depvar, predcol, groups=10) {
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)}
  if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
  if(is.factor(predcol)) predcol <- as.integer(as.character(predcol))
  helper = data.frame(cbind(depvar, predcol))
  helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(depvar), funs(total = n(),
                                    totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

dt = lift(creditTst$RESPONSE, pred$Outcome, groups = 10)
View(dt)

plotLift(creditTst$RESPONSE,pred$Outcome, cumulative = TRUE, n.buckets = 10, main = "Cumulative Lift Chart")


#######-------------------- Creating nd Plotting Gain Performance  ######--------------------


pred2<- prediction(valid1$Prediction, valid1$Outcome)
gainPerf<-performance(pred2, "tpr", "rpp")
plot(gainPerf, main="Gain chart")

#######--------------------Creating nd Plotting ROC Curve, AUC and Accuracy ######--------------------

scoreTst = pred1[,'1']
rocPredTst = prediction(scoreTst, creditTst$RESPONSE)
perfROCTst=performance(rocPredTst, "tpr", "fpr")
plot(perfROCTst, main = "ROC Curve",col="steelblue")
lines(plot(perfROCTst, main = "ROC Curve",col="red"))


#Finding the AUC Value
aucPerf = performance(rocPredTst, measure = "auc")
aucPerf@y.values


#Plotting Accuracy vs Cutoff curve
accPerf = performance(rocPredTst, measure = "acc")
plot(accPerf, main = "Accuracy Performance")



##----- Misclassification Cost Performance ------------------------------



costPerf = performance(rocPredTst, "cost", cost.fp = 500, cost.fn = 100)
rocPredTst@cutoffs[[1]][which.min(costPerf@y.values[[1]])]
table(true=creditTrn$RESPONSE,pred = predTrn)


##---- Evaluating different threshold values

# Best rpart model : rpart_ptree

#CTHRESH <- cost_mat[2,1]/(cost_mat[2,1]+cost_mat[1,2])

rm(scoreTst); rm(pred1); rm(rocPredTst)
rm(precTst); rm(recTst); rm(f_Tst)
CTHRESH=0.8

pred1=predict(rpart_ptree, creditTst, type='prob')
pred_thresh = ifelse(pred1[, '1'] >= CTHRESH, 1, 0)
cmTst = table( true=creditTst$RESPONSE,pred = pred_thresh)
mean(pred_thresh==creditTst$RESPONSE) #Accuracy of model on Test data

scoreTst = pred1[,'1']
rocPredTst = prediction(scoreTst, creditTst$RESPONSE)
perfROCTst=performance(rocPredTst, "tpr", "fpr")
plot(perfROCTst, main = "ROC Curve",col="steelblue")
lines(plot(perfROCTst, main = "ROC Curve",col="red"))

aucPerf = performance(rocPredTst, measure = "auc")
aucPerf@y.values

tpTst <- cmTst[2,2]
tnTst <- cmTst[1,1]
fpTst <- cmTst[1,2]
fnTst <- cmTst[2,1]

accTst <- (tpTst+tnTst)/(tpTst+tnTst+fpTst+fnTst)
precTst <- tpTst / (tpTst+fpTst);precTst
recTst <- tpTst / (tpTst+fnTst);recTst
f_Tst <- 2*precTst*recTst/(precTst+recTst);f_Tst


#------------ Profit model calculation ----

PROFITVAL=500
COSTVAL=-50

scoreTst=predict(rpart_model,creditTst, type="prob")[,'1'] 
prLifts=data.frame(scoreTst)
CTHRESH=0.5
pred_thresh = ifelse(prLifts[, 1] >= CTHRESH, 1, 0)

prLifts=cbind(prLifts, pred_thresh)

prLifts=prLifts[order(-scoreTst) ,]  #sort by descending score

#add profit and cumulative profits columns
prLifts<-prLifts %>% mutate(profits=ifelse(prLifts$pred_thresh==1, PROFITVAL, COSTVAL), cumProfits=cumsum(profits))

plot(prLifts$cumProfits,pch=16,ylab="Cumulative Profit",main="Profit Lift")



#-----find the score coresponding to the max profit
maxProfit= max(prLifts$cumProfits)
maxProfit_Ind = which.max(prLifts$cumProfits)
maxProfit_score = prLifts$scoreTst[maxProfit_Ind]
print(c(maxProfit = maxProfit, scoreTst = maxProfit_score))
options(scipen=99)

colnames(prLifts) <- c("Probability","Predicted Outcome","Actual Cost/Benefit","Cumulative Net Cost/Benefit")


table(credit$PERSONAL_STATUS)

#Checking and type-casting, if required
str(credit)
credit$`OBS#`<-as.numeric(credit$`OBS#`) #Observation No. to factor
credit$CHK_ACCT<-as.factor(credit$CHK_ACCT) #Checking Account to factor 
credit$HISTORY<-as.factor(credit$HISTORY) #Credit history to factor
credit$NEW_CAR<-as.factor(credit$NEW_CAR) #Purpose of Credit:New Car to factor
credit$USED_CAR<-as.factor(credit$USED_CAR) #Purpose of Credit:Used Car to factor
credit$FURNITURE<-as.factor(credit$FURNITURE) #Purpose of Credit:Furniture to factor
credit$`RADIO/TV`<-as.factor(credit$`RADIO/TV`) #Purpose of credit:Radio/TV to factor
credit$EDUCATION<-as.factor(credit$EDUCATION) #Purpose of credit:Education to factor
credit$RETRAINING<-as.factor(credit$RETRAINING) #Purpose of credit:Retraining to factor

#Lets check if there are any Null/NA values
apply(credit,2,function(x) any(is.na(x))) #Few columns have NA values

#Data Distribution
summary(credit$RESPONSE) #700 Good Credit, 300 Bad Credit
summary(credit$CHK_ACCT) #394 people do not have a checking account
summary(credit$DURATION) #Mean=20.9 months
summary(credit$HISTORY) #293 Critical account
summary(credit$NEW_CAR) #766 NA values
summary(credit$USED_CAR) #897 NA values
summary(credit$FURNITURE) #819 NA values
summary(credit$`RADIO/TV`) #720 NA values
summary(credit$EDUCATION) #950 NA values
summary(credit$RETRAINING) #903 NA values

credit$NEW_CAR<-as.numeric(credit$NEW_CAR)
credit$USED_CAR<-as.numeric(credit$USED_CAR) 
credit$FURNITURE<-as.numeric(credit$FURNITURE) 
credit$`RADIO/TV`<-as.numeric(credit$`RADIO/TV`) 
credit$EDUCATION<-as.numeric(credit$EDUCATION) 
credit$RETRAINING<-as.numeric(credit$RETRAINING)

#Inserting values in NA columns 
credit$NEW_CAR[is.na(credit$NEW_CAR)]<-0
credit$USED_CAR[is.na(credit$USED_CAR)]<-0
credit$FURNITURE[is.na(credit$FURNITURE)]<-0
credit$`RADIO/TV`[is.na(credit$`RADIO/TV`)]<-0
credit$EDUCATION[is.na(credit$EDUCATION)]<-0
credit$RETRAINING[is.na(credit$RETRAINING)]<-0

credit$`OBS#`<-as.numeric(credit$`OBS#`) #Observation No. to factor
credit$CHK_ACCT<-as.factor(credit$CHK_ACCT) #Checking Account to factor 
credit$HISTORY<-as.factor(credit$HISTORY) #Credit history to factor
credit$NEW_CAR<-as.factor(credit$NEW_CAR) #Purpose of Credit:New Car to factor
credit$USED_CAR<-as.factor(credit$USED_CAR) #Purpose of Credit:Used Car to factor
credit$FURNITURE<-as.factor(credit$FURNITURE) #Purpose of Credit:Furniture to factor
credit$`RADIO/TV`<-as.factor(credit$`RADIO/TV`) #Purpose of credit:Radio/TV to factor
credit$EDUCATION<-as.factor(credit$EDUCATION) #Purpose of credit:Education to factor
credit$RETRAINING<-as.factor(credit$RETRAINING) #Purpose of credit:Retraining to factor
credit$SAV_ACCT<-as.factor(credit$SAV_ACCT) #Avg. Balance in saving account to factor
credit$EMPLOYMENT<-as.factor(credit$EMPLOYMENT) #Employment to factor
credit$PERSONAL_STATUS<-as.factor(credit$PERSONAL_STATUS) #Personal Status to factor
credit$`CO-APPLICANT`<-as.factor(credit$`CO-APPLICANT`) #Co-Applicant to factor
credit$GUARANTOR<-as.factor(credit$GUARANTOR) #Guarantor to factor
credit$PRESENT_RESIDENT<-as.factor(credit$PRESENT_RESIDENT) #President-Resident to factor
credit$REAL_ESTATE<-as.factor(credit$REAL_ESTATE) #Real Estate to Factor
credit$PROP_UNKN_NONE<-as.factor(credit$PROP_UNKN_NONE) #No Property to factor
credit$OTHER_INSTALL<-as.factor(credit$OTHER_INSTALL) #Other installment plan to factor
credit$RENT<-as.factor(credit$RENT) #Applicant rents to factor
credit$OWN_RES<-as.factor(credit$OWN_RES) #Owns Residence to factor
credit$NUM_CREDITS<-as.numeric(credit$NUM_CREDITS) #Number of credits at this bank to factor
credit$JOB<-as.factor(credit$JOB) #Nature of Job to factor
credit$TELEPHONE<-as.factor(credit$TELEPHONE) #Telephone to factor
credit$FOREIGN<-as.factor(credit$FOREIGN) #Foreign worker to factor
credit$RESPONSE<-as.factor(credit$RESPONSE) #Good credit rating to factor
credit$NUM_DEPENDENTS<-as.factor(credit$NUM_DEPENDENTS)

summary(credit$NUM_DEPENDENTS) #No NA value
summary(credit$AMOUNT) #No NA value
summary(credit$SAV_ACCT) #No NA value
summary(credit$EMPLOYMENT) #No NA value
summary(credit$INSTALL_RATE) #Rate of Iterest, no NA value
summary(credit$PERSONAL_STATUS) #310 NA values
summary(credit$`CO-APPLICANT`) #No NA value
summary(credit$GUARANTOR) #No NA value
summary(credit$PRESENT_RESIDENT) #No NA value
summary(credit$REAL_ESTATE) #No NA value
summary(credit$PROP_UNKN_NONE) #No NA value
summary(credit$AGE) #9 NA values
summary(credit$OTHER_INSTALL) #No NA value
summary(credit$RENT) #No NA value
summary(credit$OWN_RES) #No NA value
summary(credit$NUM_CREDITS) #No NA value
summary(credit$JOB) #No NA value
summary(credit$NUM_DEPENDENTS) #No NA value
summary(credit$TELEPHONE) #No NA value
summary(credit$FOREIGN) #No NA Value
summary(credit$RESPONSE) #No NA value

credit$PERSONAL_STATUS[is.na(credit$PERSONAL_STATUS)]<-median(as.numeric(credit$PERSONAL_STATUS),na.rm=TRUE)
credit$AGE[is.na(credit$AGE)]<-median(credit$AGE,na.rm=TRUE)

apply(credit, 2, function(x) any(is.na(x))) #no NA values now

#Removing those 55 rows where NEW_CAR, USED_CAR, FURNITURE, RADIO/TV, EDUCATION, RETRAINING are zero(0)

credit_new<-credit
credit_new<-credit[credit$NEW_CAR==0 & credit$USED_CAR==0 & credit$FURNITURE==0 & credit$`RADIO/TV`==0 & credit$EDUCATION==0 & credit$RETRAINING==0,]
credit_final<-credit[-c(credit_new$`OBS#`),]

#Bivariate Analysis
str(credit)
install.packages("tidyr")
library(ggplot)

dev.off()
par(mfrow=c(2,3))
boxplot(AGE~RESPONSE, data = credit, col=c("Red","Green"), main="Relationship between Age and Response", xlab="Response") #Outliers in both the levels
boxplot(DURATION~RESPONSE, data = credit, col=c("Red","Green"), main="Relationship between Duration and Response", xlab="Response") #Outliers in Response 1
boxplot(INSTALL_RATE~RESPONSE, data = credit, col=c("Red","Green"), main="Relationship between Install Rate and Response", xlab="Response") #No outliers
boxplot(AMOUNT~RESPONSE, data = credit, col=c("Red","Green"), main="Relationship between Amount and Response", xlab="Response") #Outliers in both the levels
boxplot(NUM_CREDITS~RESPONSE, data = credit, col=c("Red","Green"), main="Relationship between Number of Credits and Response", xlab="Response") 
table(as.factor(credit$NUM_CREDITS))

library(ggplot2)
library(dplyr)
str(credit)

numcol<-credit_final[,c("DURATION","AMOUNT","INSTALL_RATE","AGE","NUM_CREDITS")]


library(rpart)
library(rpart.plot)
mod1<-rpart(RESPONSE~., data=credit_final, method = "class")
print(mod1)

rpart.plot:prp(mod1, type=2, extra=1)

mod1$variable.importance
summary(mod1) #To check the XError
plotcp(mod1) #Plot CP and X relative Error
ptree<-prune(mod1,cp= mod1$cptable[which.min(mod1$cptable[,"xerror"]),"CP"]) #CP:Complexity Parameter, it is used to prune our tree.
#It is evaluating the value of CP for the minimum value of xerror

plot(ptree,  uniform=TRUE,  main="Decision Tree for German Credit Rating")
text(ptree, use.n=TRUE, all=TRUE, cex=.7)

#bivariate for multiple factors

#ANOVA Test between Response variable and all NUmeric Variables

duration.aov<-aov(DURATION~RESPONSE, data = credit_final)
summary(duration.aov) #Duration is Significant at 95% CI


amount.aov<-aov(AMOUNT~RESPONSE, data = credit_final)
summary(amount.aov) #Response is Significant at 95% CI

installRate.aov<-aov(INSTALL_RATE~RESPONSE, data = credit_final)
summary(installRate.aov) #Install Rate is Significant at 95% CI


install.packages("rpivotTable")
library(rpivotTable)

credit_final %>% rpivotTable()
str(credit_final)

#----------------------------------------RANDOM FOREST--------------------------------------------#



#------Data Preparation for RandomForest------#

install.packages("randomForest")
library(randomForest)

set.seed(655944638)

nr=nrow(credit_final)

creditTrainIndex<-sample(1:nr, size = round(0.7*nr), replace=FALSE)

creditTrain<-credit_final[creditTrainIndex,]
creditTest<-credit_final[-creditTrainIndex,]
str(creditTrain)

colnames(creditTest)[1]<-"Observation"
colnames(creditTrain)<-gsub("-","",colnames(creditTrain))

colnames(creditTrain)[1]<-"Observation"
colnames(creditTrain)<-gsub("/","",colnames(creditTrain))
colnames(creditTest)<-gsub("/","",colnames(creditTest))
colnames(creditTest)<-gsub("-","",colnames(creditTest))


#--------Random Forest Modelling---------#

#Random Forest Model
rm(rfModel)
creditTest<-creditTest[,-c(1)]
rfModel<-randomForest(RESPONSE~., data=creditTrain, ntree=800, importance=TRUE)
rfModel #Check OOB error rate


#Variable Importance
importance(rfModel)
varImpPlot(rfModel)

#Predict on Test Data
rfPredict<-predict(rfModel, creditTest, type="prob")

#drawing the ROC curve for the randomForest model
ROCPredict<-prediction(rfPredict[,2],creditTest$RESPONSE)
ROCPerform<-performance(ROCPredict,"tpr","fpr")
plot(ROCPerform) #ROC Curve

#AUC
scoreTst=rfPredict[,'1']
rocPredTst <- prediction(scoreTst, creditTest$RESPONSE)
aucPerf = performance(rocPredTst, measure="auc")
aucPerf@y.values

#Confusion Matrix
CMatrix<-table(true=creditTest$RESPONSE, predict=predict(rfModel, creditTest, type="class"))

#Confusion Matrix Parameters
TP<-CMatrix[2,2]
TN<-CMatrix[1,1]
FN<-CMatrix[2,1]
FP<-CMatrix[1,2]

Accuracy<-(TP+TN)/(TP+FP+TN+FN)
Accuracy
Precision<-TP/(TP+FP)
Precision
Recall<-TP/(TP+FN)
Recall
FScore<-(2*Precision*Recall)/(Precision+Recall)
FScore

#------------ Profit model calculation ----
dev.off()
PROFITVAL=500
COSTVAL=-50

rm(scoreTest, prLifts)

scoreTest=predict(rfModel,creditTest, type="prob")[,'1']
prLifts=data.frame(scoreTest)
CTHRESH=0.7
pred_thresh = ifelse(prLifts[, 1] >= CTHRESH, 1, 0)

prLifts=cbind(prLifts, pred_thresh)

prLifts=prLifts[order(-scoreTest) ,]  #sort by descending score

#add profit and cumulative profits columns
prLifts<-prLifts %>% mutate(profits=ifelse(prLifts$pred_thresh==1, PROFITVAL, COSTVAL), cumProfits=cumsum(profits))
colnames(prLifts)<-c("Probability", "Predicted Outcome","Actual Cost/Benefit", "Cumulative Net Cost/Benefit")

View(prLifts)
plot(prLifts$cumProfits,pch=16,ylab="Cumulative Profit",main="Profit Lift")

#-----find the score coresponding to the max profit
maxProfit= max(prLifts$cumProfits)
maxProfit_Ind = which.max(prLifts$cumProfits)
maxProfit_score = prLifts$scoreTst[maxProfit_Ind]
print(c(maxProfit = maxProfit, scoreTst = maxProfit_score))
options(scipen=99)

colnames(prLifts) <- c("Probability","Predicted Outcome","Actual Cost/Benefit","Cumulative Net Cost/Benefit")


#-----------------BiVariate Analysis-------------------#

str(credit_final)
dev.off()
par(mfrow=c(3,4))

t<-table(credit_final$RESPONSE,credit_final$CHK_ACCT)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and Checking Account", xlab = "Checking Account")
chisq.test(t)$p.value

t<-table(credit_final$RESPONSE,credit_final$HISTORY)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and History", xlab = "History")
chisq.test(t)$p.value

t<-table(credit_final$RESPONSE,credit_final$NEW_CAR)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and New Car", xlab = "New Car")
chisq.test(t)$p.value

t<-table(credit_final$RESPONSE,credit_final$USED_CAR)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and Used Car", xlab = "Used Car")
chisq.test(t)$p.value

t<-table(credit_final$RESPONSE,credit_final$FURNITURE)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and Furniture", xlab = "Furniture")
chisq.test(t)$p.value

t<-table(credit_final$RESPONSE,credit_final$`RADIO/TV`)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and Radio/TV", xlab = "Radio/TV")
chisq.test(t)$p.value

t<-table(credit_final$RESPONSE,credit_final$EDUCATION)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and Education", xlab = "Education")
chisq.test(t)$p.value

t<-table(credit_final$RESPONSE,credit_final$RETRAINING)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and Retraining", xlab = "Retraining")
chisq.test(t)$p.value

t<-table(credit_final$RESPONSE,credit_final$SAV_ACCT)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and Saving Account", xlab = "No. of Saving Account")
chisq.test(t)$p.value

t<-table(credit_final$RESPONSE,credit_final$EMPLOYMENT)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and Employment", xlab = "Employment")
chisq.test(t)$p.value

t<-table(credit_final$RESPONSE,credit_final$PERSONAL_STATUS)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and Personal Status", xlab = "Personal Status")
chisq.test(t)$p.value

t<-table(credit_final$RESPONSE,credit_final$`CO-APPLICANT`)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and Co-Applicant", xlab = "Co-Applicant")
chisq.test(t)$p.value

dev.off()
par(mfrow=c(3,4))

t<-table(credit_final$RESPONSE,credit_final$GUARANTOR)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and Guarantor", xlab = "Guarantor")
chisq.test(t)$p.value

t<-table(credit_final$RESPONSE,credit_final$PRESENT_RESIDENT)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and Present Resident", xlab = "Present Resident")
chisq.test(t)$p.value

t<-table(credit_final$RESPONSE,credit_final$REAL_ESTATE)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and Real Estate", xlab = "Real Estate")
chisq.test(t)$p.value

t<-table(credit_final$RESPONSE,credit_final$PROP_UNKN_NONE)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and Unknown Property", xlab = "Unknown Property")
chisq.test(t)$p.value

t<-table(credit_final$RESPONSE,credit_final$OTHER_INSTALL)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and Other Installment", xlab = "Other Installment")
chisq.test(t)$p.value

t<-table(credit_final$RESPONSE,credit_final$RENT)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and Rent", xlab = "Rent")
chisq.test(t)$p.value

t<-table(credit_final$RESPONSE,credit_final$OWN_RES)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and Own Resident", xlab = "Own Resident")
chisq.test(t)$p.value

t<-table(credit_final$RESPONSE,credit_final$JOB)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and Job", xlab = "Job")
chisq.test(t)$p.value

t<-table(credit_final$RESPONSE,credit_final$TELEPHONE)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and Telephone", xlab = "Telephone")
chisq.test(t)$p.value

t<-table(credit_final$RESPONSE,credit_final$FOREIGN)
propt<-prop.table(t)
barplot(propt, col=c("Red", "Green"), main="Relationship between Response and Foreign", xlab = "Foreign")
chisq.test(t)$p.value

str(manav_factor)

#------Chi-square MOdel-------------#

manav_factor[,-c("JOB","GUARANTOR","PERSONAL_STATUS","RETRAINING","FURNITURE","EDUCATION","PRESENT_RESIDENT","TELEPHONE")]

dropcol<-c("JOB","GUARANTOR","PERSONAL_STATUS","RETRAINING","FURNITURE","EDUCATION","PRESENT_RESIDENT","TELEPHONE")

SigniVar<-manav_factor %>% select(-dropcol)

str(manav_num)
summary(duration.aov)
summary(installRate.aov) #Not sig
summary(amount.aov)
#age.aov<-aov(AGE~RESPONSE, data=credit_final)
summary(age.aov)
#credit.aov<-aov(NUM_CREDITS~RESPONSE, data=credit_final)
summary(credit.aov)
#dependent.aov<-aov(NUM_DEPENDENTS~RESPONSE, data = credit_final)
summary(dependent.aov)




