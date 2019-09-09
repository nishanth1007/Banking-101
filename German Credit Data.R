library('tidyr')
library('dplyr')
library('imputeTS')
library('readxl')
library('psych')

md=read_excel("N:/UIC MIS/Sem 2/DM - IDS572/Assignments/1/GermanCredit_assgt1_S19.xls")
summary(md)
str(md)
View(md)

md$NEW_CAR[is.na(md$NEW_CAR)] <- 0
md$USED_CAR[is.na(md$USED_CAR)] <- 0
md$FURNITURE[is.na(md$FURNITURE)] <- 0
md$`RADIO/TV`[is.na(md$`RADIO/TV`)] <- 0
md$EDUCATION[is.na(md$EDUCATION)] <- 0
md$RETRAINING[is.na(md$RETRAINING)] <- 0

# Imputing Age since they have missing values
sum(!complete.cases(md$AGE))
var <- median(md$AGE, na.rm= TRUE)
var
md$AGE[is.na(md$AGE)] <- 33

md$PERSONAL_STATUS[is.na(md$PERSONAL_STATUS)]<- "Other"


# Columns to coerce to factor as per the excel sheet
cols<-c("OBS#","CHK_ACCT","HISTORY","NEW_CAR","USED_CAR","FURNITURE","RADIO/TV","EDUCATION","RETRAINING","SAV_ACCT","EMPLOYMENT","PERSONAL_STATUS","CO-APPLICANT","GUARANTOR","PRESENT_RESIDENT","REAL_ESTATE",	"PROP_UNKN_NONE","OTHER_INSTALL","RENT","OWN_RES","JOB","TELEPHONE","FOREIGN","RESPONSE")
md[cols]<-lapply(md[cols], factor)
md$X<-NULL
sapply(md, class)


#checking the datatype of the dataset
str(md)
View(md)

#proportion of bad vs good cases
t1<-table(md$RESPONSE)
t1
barplot(t1)
#there are 700 good cases and 300 bad cases , the same has been depicted in the 
#bar plot. Hence, the proportion of good to bad cases is 7:3

#How  are missing values handled
#We believe the following are the potential ways to handle the missing values: 
#   1) Converting the NAs to 0
#   2) Combining the columns ("NEW_CAR","USED_CAR","FURNITURE","RADIO/TV","EDUCATION","RETRAINING")
#      which contain missing values under a common column name 'Purpose' .
#   3) Imputing the values based on the property of the column.
#   4) Removing the columns ("NEW_CAR","USED_CAR","FURNITURE","RADIO/TV","EDUCATION","RETRAINING") 
#      as these contain the max # of NA values.


###UNIVARIATE Analysis 

## Numerical Variables

# DURATION
describe(md$DURATION)
# The median is smaller than the mean, so it is right skewed. 
# The skewness is 1.09 which shows that it is skewed. 
# The range is 68, between 4 and 72. 
# The kurtosis is 0.9 .
hist(md$DURATION,main="Duration Distribution",xlab ="Duration", col="green", freq=F)
plot(density(md$DURATION),main="Duration",xlab ="Duration", col="black", lwd=3) 
# The plots are right skewed. 
# It is multi modal graph.

# AMOUNT
describe(md$AMOUNT)
# The median is smaller than the mean, so it is right skewed. 
# The skewness is 1.94 which shows that it is heavily skewed. 
# The range is 18174, between 250 and 18424 
# The kurtosis is 4.25 .
hist(md$AMOUNT,main="AMOUNT Distribution",xlab ="Amount", col="green", freq=F)
plot(density(md$AMOUNT),main="AMOUNT",xlab ="Amount", col="black", lwd=3) 
# The plots are heavily right skewed. 

# INSTALL_RATE
describe(md$INSTALL_RATE)
# The mean is smaller than the median.It is right skewed.
# The skewness is -0.53 which shows that it is heavily skewed. 
# The range is 3, between 1 and 4 
# The kurtosis is -1.21 .
hist(md$INSTALL_RATE,main="Installment Rate Distribution",xlab ="Installment Rate", col="green", freq=F)
plot(density(md$INSTALL_RATE),main="Installment Rate",xlab ="Installment Rate", col="black", lwd=3) 
# The plots are heavily right skewed.

# AGE
describe(md$AGE)
# The median is smaller than the mean, so it is right skewed. 
# The skewness is 1.04 which shows that it is skewed. 
# The range is 56, between 19 and 75 
# The kurtosis is 0.65 .
hist(md$AGE,main="AGE Distribution",xlab ="Age", col="green", freq=F)
plot(density(md$AGE, na.rm = TRUE),main="AGE",xlab ="Age", col="black", lwd=3) 
# The plots are right skewed.

# NUM_CREDITS
describe(md$NUM_CREDITS)
# The median is smaller than the mean, so it is right skewed. 
# The skewness is 1.27 which shows that it is heavily skewed. 
# The range is 3, between 1 and 4 
# The kurtosis is 1.58 
hist(md$NUM_CREDITS,main="NUM_CREDITS Distribution",xlab ="NUM_CREDITS", col="green", freq=F)
plot(density(md$NUM_CREDITS, na.rm = TRUE),main="NUM_CREDITS",xlab ="NUM_CREDITS", col="black", lwd=3) 
# The plots are heavily skewed.
# Plot is Multi modal.

# NUM_DEPENDENTS
describe(md$NUM_DEPENDENTS)
# The median is smaller than the mean, so it is right skewed. 
# The skewness is 1.9 which shows that it is heavily skewed. 
# The range is 1, between 1 and 2 
# The kurtosis is 1.63 .
hist(md$NUM_DEPENDENTS,main="NUM_DEPENDENTS Distribution",xlab ="NUM_DEPENDENTS", col="green", freq=F)
plot(density(md$NUM_DEPENDENTS),main="NUM_DEPENDENTS",xlab ="NUM_DEPENDENTS", col="black", lwd=3) 
# The plots are heavily right skewed.
# Plots is Multi modal.



## CATEGORICAL ATTIBUTES

# CHK_ACCT
summary(md$CHK_ACCT)
pchk_acct<-(prop.table(table(md$CHK_ACCT))*100) # Convert to percentages 
pchk_acct
barplot(pchk_acct, main = "Checking Account", xlab = "Checking Account Category", ylab = "Percentage", col="steelblue", ylim=c(0,95))
# From the plot we see that, Checking account status 3 has the maximum percentage, 
# meaning applicants with no Checking account  have the highest percentage whereas, 
# Checking account status 2 has the least percentage, meaning applicants with more than 200 DM hav the lowest percentage

# HISTORY
summary(md$HISTORY)
phistory<-(prop.table(table(md$HISTORY))*100) # Convert to percentages 
barplot(phistory, main = "History", xlab = "History Category", ylab = "Percentage", col="steelblue", ylim=c(0,95))
# From the plot we see that the applicants who have existing credits paid back till now have the highest percentage,whereas, the applicants without any credit history have the lowest percentage.

# SAV_ACCT
summary(md$SAV_ACCT)
psav_acct<-(prop.table(table(md$SAV_ACCT))*100) # Convert to percentages 
barplot(psav_acct, main = "Saving Account", xlab = "Saving Account Category", ylab = "Percentage", col="steelblue", ylim=c(0,95))
# From the plots we see that, an applicant with an average balance of less than 100 DM has the maximum percentage,whereas, those applicants with saving accounts >=1000 DM have the minimum percentage.

# EMPLOYMENT
summary(md$EMPLOYMENT)
pemployment<-(prop.table(table(md$EMPLOYMENT))*100) # Convert to percentages 
barplot(pemployment, main = "Employment", xlab = "Employment Category", ylab = "Percentage", col="steelblue", ylim=c(0,95))
# From the plot we see that the applicants who have existing employment between 1 to 4 years have the highest percentage,whereas, the applicants with no employment have the lowest percentage.

# JOB
summary(md$JOB)
pjob<-(prop.table(table(md$JOB))*100) # Convert to percentages 
barplot(pjob, main = "Job", xlab = "Job Category", ylab = "Percentage", col="steelblue", ylim=c(0,95))
# From the plot we see that the skilled applicants have the highest percentage,whereas, unskilled applicants have the lowest percentage.

# GUARANTOR
summary(md$GUARANTOR)
pguarantor<-(prop.table(table(md$GUARANTOR))*100) # Convert to percentages 
barplot(pguarantor, main = "Guarantor", xlab = "Guarantor Status", ylab = "Percentage", col="steelblue", ylim=c(0,95))
# From the plots, we can see that, the applicants which have no guarantors hold the large percentage as compared to the applicants with a guarantor

#CO-APPLICANT 
summary(md$`CO-APPLICANT`)
pcoapp<-(prop.table(table(md$`CO-APPLICANT`))*100) # Convert to percentages 
barplot(pcoapp, main = "Co-Applicant", xlab = "Co-Applicant Status", ylab = "Percentage", col="steelblue", ylim=c(0,95))
#From the plot it is clearly evident that having no co-applicant has the highest percentage

#FOREIGN
summary(md$FOREIGN)
pforeign<-(prop.table(table(md$FOREIGN))*100) # Convert to percentages 
barplot(pforeign, main = "Foreign", xlab = "Foreign Status", ylab = "Percentage", col="steelblue", ylim=c(0,95))

#PERSONAL_STATUS
summary(md$PERSONAL_STATUS)
ppstat<-(prop.table(table(md$PERSONAL_STATUS))*100) # Convert to percentages 
barplot(ppstat, main = "Personal Status", xlab = "Personal Status", ylab = "Percentage", col="steelblue", ylim=c(0,95))
#The plot has the highest percentage of applicants having status single.

#OTHER_INSTALL
summary(md$OTHER_INSTALL)
poinst<-(prop.table(table(md$OTHER_INSTALL))*100) # Convert to percentages 
barplot(poinst, main = "Other Installment Credit", xlab = "Other Installment Status", ylab = "Percentage", col="steelblue", ylim=c(0,95))

#OWN_RES
summary(md$OWN_RES)
pownres<-(prop.table(table(md$OWN_RES))*100) # Convert to percentages 
barplot(pownres, main = "Owns Residence", xlab = "Owns Residence Status", ylab = "Percentage", col="steelblue", ylim=c(0,95))
# the applicants owning a residence is the highest percentage

#REAL_ESTATE
summary(md$REAL_ESTATE)
prees<-(prop.table(table(md$OWN_RES))*100) # Convert to percentages 
barplot(prees, main = "Owns Real Estate", xlab = "Owns Real Estate Status", ylab = "Percentage", col="steelblue", ylim=c(0,95))

#RENT
summary(md$RENT)
prent<-(prop.table(table(md$RENT))*100) # Convert to percentages 
barplot(prent, main = "Rent", xlab = "Rent Status", ylab = "Percentage", col="steelblue", ylim=c(0,95))

#RESPONSE
summary(md$RESPONSE)
presp<-(prop.table(table(md$RESPONSE))*100) # Convert to percentages 
barplot(presp, main = "Response", xlab = "Response Status", ylab = "Percentage", col="steelblue", ylim=c(0,95))



## BIVARIATE Analysis

# Numerical vs Categorical Relationship

# AGE AND RESPONSE
x<-md[md$RESPONSE=="0",]
y<-md[md$RESPONSE=="1",]
plot(density(x$AGE, na.rm = TRUE), col="red", lwd=2.5, main="Distribution of Age by Response")
lines(density(y$AGE, na.rm = TRUE), col="blue", lwd=2.5) # 
# The blue color plot represents the distribution of good creditors as per age while the red color plot shows distribution of bad creditors according to age.
# The peak of red plot signifies that the number of bad creditors are more as compared to good creditors in the same age range.
# This is something unexpected.

# AMOUNT AND RESPONSE
x<-md[md$RESPONSE=="0",]
y<-md[md$RESPONSE=="1",]
plot(density(x$AMOUNT, na.rm = TRUE), col="red", lwd=2.5, main="Distribution of Amount by Response")
#boxplot(x$AMOUNT, col="red", lwd=2.5, main="Distribution of Amount by Response")
lines(density(y$AMOUNT, na.rm = TRUE), col="blue", lwd=2.5)
# The blue color indicates that there are good creditors with good amount, while the red color plot indicates the bad creditors
# in the same range of amount for bad creditors


# Categorical Vs Categorical Relationships 
library('ggplot2')
# CHK_ACCT and RESPONSE
df <- data.frame(table(md$CHK_ACCT, md$RESPONSE))
names(df) <- c("CHK_ACCT", "RESPONSE", "Count")
ggplot(data=df, aes(x= CHK_ACCT, y=Count, fill = RESPONSE)) + geom_bar(stat = "identity") + geom_text(aes(label=Count))
# Almost 88.3% of applicants having no checking account are good creditors, which is a surprise. The applicants which checking account
# status as >200 DM are mostly good creditors, which is not a surprise.

# HISTORY and RESPONSE
df1 <- data.frame(table(md$HISTORY, md$RESPONSE))
names(df1) <- c("HISTORY", "RESPONSE", "Count")
ggplot(data=df1, aes(x= HISTORY, y=Count, fill = RESPONSE)) + geom_bar(stat = "identity") + geom_text(aes(label=Count))
# Almost 82.9% of applicants having critical accounts are good creditors. This also comes as a surprise.

# SAV_ACCT and RESPONSE
df2 <- data.frame(table(md$SAV_ACCT, md$RESPONSE))
names(df2) <- c("SAV_ACCT", "RESPONSE", "Count")
ggplot(data=df2, aes(x= SAV_ACCT, y=Count, fill = RESPONSE)) + geom_bar(stat = "identity") + geom_text(aes(label=Count))
# Most applicants having average balance in their savings account between 500 - 1000 DM and more than 1000 DM are good creditors.
# Also, 82.5% of appplicants having no savings account are good creditors, and this comes as a surprise.

# JOB and RESPONSE
df3 <- data.frame(table(md$JOB, md$RESPONSE))
names(df3) <- c("JOB", "RESPONSE", "Count")
ggplot(data=df3, aes(x= JOB, y=Count, fill = RESPONSE)) + geom_bar(stat = "identity") + geom_text(aes(label=Count))
# In the skilled/official employee category - 29.5% applicants are bad creditors and 70.5% are good creditors. 
# This maximum number of good creditors doesn't come as a surprise.

# OWN_RES and RESPONSE
df4 <- data.frame(table(md$OWN_RES, md$RESPONSE))
names(df4) <- c("OWN_RES", "RESPONSE", "Count")
ggplot(data=df4, aes(x= OWN_RES, y=Count, fill = RESPONSE)) + geom_bar(stat = "identity") + geom_text(aes(label=Count))
# 73.91% of the applicants who own residence are good creditors and this doesn't come as a surprise.

# GUARANTOR and RESPONSE
df5 <- data.frame(table(md$GUARANTOR, md$RESPONSE))
names(df5) <- c("GUARANTOR", "RESPONSE", "Count")
ggplot(data=df5, aes(x= GUARANTOR, y=Count, fill = RESPONSE)) + geom_bar(stat = "identity") + geom_text(aes(label=Count))
# Though the applicants having a guarantor is very less there more number are good creditors. 69.4% of applicants having no guarantor are good creditors too, which 
# is a surprise.

# FOREIGN and RESPONSE
df6 <- data.frame(table(md$FOREIGN, md$RESPONSE))
names(df6) <- c("FOREIGN", "RESPONSE", "Count")
ggplot(data=df6, aes(x= FOREIGN, y=Count, fill = RESPONSE)) + geom_bar(stat = "identity") + geom_text(aes(label=Count))
# Though the proportion of foreign workers is low, in that proportion good creditors are maximum. 
#Approx 1/3rd of the non-foreign workers are bad creditors while the rest are good.

#TELEPHONE AND RESPONSE
df7<- data.frame(table(md$TELEPHONE,md$RESPONSE))
names(df7) <- c("TELEPHONE", "RESPONSE", "Count")
ggplot(data=df7, aes(x= TELEPHONE, y=Count, fill = RESPONSE)) + geom_bar(stat = "identity") + geom_text(aes(label=Count))

#EMPLOYMENT AND RESPONSE
df8<- data.frame(table(md$EMPLOYMENT,md$RESPONSE))
names(df8) <- c("EMPLOYMENT", "RESPONSE", "Count")
ggplot(data=df8, aes(x= EMPLOYMENT, y=Count, fill = RESPONSE)) + geom_bar(stat = "identity") + geom_text(aes(label=Count))
# Applicants that have less than 4 years of work exp have 76.1% of good creditors as compared to that of 7 years work exp

## Interesting variables and relationships
# Guarantor is interesting because the customers who dont have a Guarantor have a good credit response
# Foreign is interting because of its decent number of 33 customers who are foreigners having good credit response
# Customers who donot have a savings account have a good credit response
# Crticial account holders history  have a good credit response
# People who have an employment of less than 4 yrs have a better credit response than the ones having 7years expereience

## Most Relavant variables
# Age, guarantor, employment, history and savings account 
# 


##---------------------------------------------------------------------------------------------------------###############
#(2)We will first focus on a descriptive model - i.e. assume we are not interested in prediction. 

#2(a) Develop a decision tree on the full data (using the rpart package).
#What decision tree node parameters do you use to get a good model. Explain the parameters you use


install.packages('rpart')
library('rpart')

# The following columns are not influencing while building the decision tree
md<-md[,-c(1,5,6,7,8,9,10)] 
View(md)

set.seed(123)
rmodel=rpart(RESPONSE ~.,data=md,method="class")
print(rmodel)
summary(rmodel) 
View(md)
str(md)
# Plotting the Decision Tree
rpart.plot::prp(rmodel, type = 2, extra = 1)
predTrn=predict(rmodel, md, type='class')
# Confusion table
table(pred = predTrn, true=md$RESPONSE)
#    true
#pred   0   1
#   0 181  98
#   1 119 602
#Accuracy
mean(predTrn==md$RESPONSE)
#78.3%


#DT2
set.seed(128)
rmodel=rpart(RESPONSE ~ ., data=md, method="class",maxdepth=15,minsplit=15,xval=10,cp=.001,parms=list(split='information'))
summary(rmodel) 
# Plotting the graph
rpart.plot::prp(rmodel, type = 2, extra = 1)
predTrn=predict(rmodel, md, type='class')
# Confusion table
table(pred = predTrn, true=md$RESPONSE)
#true
#pred   0   1
#0    216  50
#1    84 650
#Accuracy
mean(predTrn==md$RESPONSE)
#86.6%

#DT3
set.seed(128)
rmodel=rpart(RESPONSE ~ ., data=md, method="class",maxdepth=15,minsplit=40,xval=10,parms=list(split='gini'))
summary(rmodel) 
# Plotting the graph
rpart.plot::prp(rmodel, type = 2, extra = 1)
predTrn=predict(rmodel, md, type='class')
# Confusion table
table(pred = predTrn, true=md$RESPONSE)
#    true
#pred   0   1
#0    170  95
#1  130 605
#Accuracy
mean(predTrn==md$RESPONSE)
#77.5%

#DT4
set.seed(123)
rmodel=rpart(RESPONSE ~ ., data=md, method="class",maxdepth=5,minsplit=50,xval=15,cp=.001,parms=list(split='information'))
summary(rmodel) 
# Plotting the graph
rpart.plot::prp(rmodel, type = 2, extra = 1)
predTrn=predict(rmodel, md, type='class')
# Confusion table
table(pred = predTrn, true=md$RESPONSE)
#true
#pred   0   1
#0    180 117
#1   120 583
#Accuracy
mean(predTrn==md$RESPONSE)
# 76.3%


##2(b)Which variables are important to differentiate "good" from "bad" cases - and how do you determine these? 
#Does this match your expectations (from your response in Question 1)?

#Comparing with the best model obtained from Q2(a)
set.seed(128)
rmodel=rpart(RESPONSE ~ ., data=md, method="class",maxdepth=15,minsplit=15,xval=10,cp=.001,parms=list(split='information'))
summary(rmodel) 
##Variable importance
#CHK_ACCT           AMOUNT         DURATION          HISTORY
#20               15                9                9 

set.seed(128)
rmodel=rpart(RESPONSE ~ ., data=md,method="class", parms=list(split='information'))
summary(rmodel) 
#Variable importance
#CHK_ACCT       HISTORY        DURATION       SAV_ACCT         
#41             13             12             11 

set.seed(128)
rmodel=rpart(RESPONSE ~ ., data=md, method="class",maxdepth=15,minsplit=40,xval=10,parms=list(split='gini'))
summary(rmodel) 
#Variable importance
#CHK_ACCT       DURATION        HISTORY       SAV_ACCT         
#38             13             13             12  

set.seed(128)
rmodel=rpart(RESPONSE ~ ., data=md, method="class",parms=list(split='gini'))
summary(rmodel)
#Variable importance
#CHK_ACCT       HISTORY        DURATION         AMOUNT       
#36             13             13             13


# The CHK_ACCT, DURATION, AMOUNT and HISTORY are the varaibles which influence in differentiaiting the Good 
# and Bad cases. We have determined these by running different models of decision trees using different parameters. 


##2(c)What levels of accuracy/error are obtained? What is the accuracy on the "good" and "bad" cases? 
# Obtain and interpret the lift chart. Do you think this is a reliable (robust?) description, and why.
# Now we can find the model accuracy and accuracy on good cases and bad cases.

#DT1
set.seed(123)
rmodel=rpart(RESPONSE ~ ., data=md, method="class", parms = list(split ='gini'))
predTrn=predict(rmodel, md, type='class')
table(pred = predTrn, true=md$RESPONSE)
#    true
#pred   0   1
#   0 181  98
#   1 119 602
#Accuracy on bad cases : 60.3%
#Accuracy on good cases: 86.0%
#Model Accuracy
mean(predTrn==md$RESPONSE)
#78.3%

#DT2
set.seed(123)
rmodel=rpart(RESPONSE ~ ., data=md, method="class", parms = list(split ='gini'), control = rpart.control(minsplit = 10, cp=0.001)) 
predTrn=predict(rmodel, md, type='class')
table(pred = predTrn, true=md$RESPONSE)
#    true
#pred   0   1
#   0 233  38
#   1  67  662
#Accuracy on bad cases : 76.8%
#Accuracy on good cases: 94.5%
#Model Accuracy
mean(predTrn==md$RESPONSE)
#89.5%

#DT3
set.seed(123)
rmodel=rpart(RESPONSE ~ ., data=md, method="class", parms = list(split ='information'))
predTrn=predict(rmodel, md, type='class')
table(pred = predTrn, true=md$RESPONSE)
#    true
#pred   0   1
#   0 175  97
#   1 125 603
#Accuracy on bad cases : 58.3%
#Accuracy on good cases: 86.1%
#Model Accuracy
mean(predTrn==md$RESPONSE)
#77.8%

#DT4
set.seed(123)
rmodel=rpart(RESPONSE ~ ., data=md, method="class", parms = list(split ='information'), control = rpart.control(minsplit = 10, cp=0.001)) 
predTrn=predict(rmodel, md, type='class')
table(pred = predTrn, true=md$RESPONSE)
#    true
#pred   0   1
#   0 231  30
#   1  69 670
#Accuracy on bad cases : 77%
#Accuracy on good cases: 95.7%
#Model Accuracy
mean(predTrn==md$RESPONSE)
#90.1%


#ROCR
install.packages('ROCR')
library('ROCR')
scoreTst=predict(rmodel,md, type="prob")[,'1']  
#Now apply the prediction function from ROCR to get a prediction object
rocPredTst = prediction(scoreTst, md$RESPONSE, label.ordering = c('0', '1'))  
#Obtain performance using the function from ROCR, then plot
perfROCTst=performance(rocPredTst, "tpr", "fpr")
plot(perfROCTst)
#This method doesn't give a reliable model as we cannot estimate how this model will work on unseen data.
#Lift chart
lift.perf = performance(rocPredTst,measure = "lift")
lift.perf@y.values
plot(lift.perf)



####------------------------------------------------------------------------------#####
##3


# Install the 'rpart' package to develop DT
install.packages('rpart')
library('rpart')

# Make sure that the variabes are set to the correct attribute type -- factor, integer, numeric
str(md) 

#Question 3
# We next consider developing a model for prediction. 
#For this, we should divide the data into Training and Validation sets.
#Consider a partition of the data into 50% for Training and 50% for Test 
#a) a Develop decision trees using the rpart package. What model performance do you obtain? Consider performance based on 
#overall accuracy/error and on the 'good' and 'bad' credit cases - explain which performance measures, 
#like recall, precision, sensitivity, etc. you use and why. 
#Also consider lift, ROC and AUC.Is the model reliable (why or why not)?
#split the data into training and test(validation) sets - 50% for training, rest for validation


set.seed(123)
TRG_PCT=0.5
nr=nrow(md)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE) #get a random 50% sample of row-indices
gcTrn = md[trnIndex,]   #training data with the randomly selected row-indices (50%)
gcTst = md[-trnIndex,]  #test data with the other row-indices (50%)

#Now we can find the model accuracy and accuracy on good cases and bad cases. 

#Model 1 - GINI split
set.seed(123)
rpModel1g = rpart(RESPONSE ~ ., data=gcTrn, method="class", parms = list(split ='gini'))
#Finding prediction on train data:
predTrng = predict(rpModel1g, gcTrn, type='class')
table(pred = predTrng, true=gcTrn$RESPONSE)
#Confusion Matrix
      #true
#pred  0   1
#0  75  19
#1  74 332

#Accuracy on bad cases : 50.3%
#Accuracy on good cases: 94.5%

#Model Accuracy
mean(predTrng==gcTrn$RESPONSE) #81.4%

#Finding prediction on test data:
predTestg = predict(rpModel1g, gcTst, type="class")
table(pred=predTestg, true=gcTst$RESPONSE)
#true
#pred 0   1
#0  49  44
#1  102 305


#Accuracy on bad cases : 32.5%
#Accuracy on good cases: 87.4%
#Model Accuracy
mean(predTestg==gcTst$RESPONSE) #70.8%




#Model 2 - INFORMATION split
set.seed(123)
rpModel1i=rpart(RESPONSE ~ ., data=gcTrn, method="class", parms = list(split ='information'))
#Finding prediction on Train data:
predTrni=predict(rpModel1i, gcTrn, type='class')
table(pred = predTrni, true=gcTrn$RESPONSE)
#true
#pred   0   1
#0  79  21
#1  70 330

#Accuracy on bad cases : 53%
#Accuracy on good cases: 94%

#Model Accuracy
mean(predTrni==gcTrn$RESPONSE) #81.8%

#Finding prediction on test data:
predTesti = predict(rpModel1i, gcTst, type="class")
table(pred=predTesti, true=gcTst$RESPONSE)
#true
#pred   0   1
#0  64  53
#1  87 296

#Accuracy on bad cases :42.4% 
#Accuracy on good cases: 84.8%
#Model Accuracy
mean(predTesti==gcTst$RESPONSE) #72%

#Model build on information split criteria , has a better accuracy on test data (more by 1.2%) 
#as compared to gini criteria,hence,we will only see the models build on information split criteria.

#Performance measures

#Accuracy/Precision/Recall
install.packages("caret")
install.packages("e1071")

library(caret)
library(e1071)

#Using Gini Index
confusionMatrix(predTestg,gcTst$RESPONSE)
confusionMatrix(predTestg,gcTst$RESPONSE,mode = "prec_recall")
#Uisng Information Index
confusionMatrix(predTesti,gcTst$RESPONSE)
confusionMatrix(predTesti,gcTst$RESPONSE,mode = "prec_recall")

#ROC 
library('psych')
library(ROCR)
install.packages('pROC')
library(pROC) # install with install.packages("pROC")
scoreTsti=predict(rpModel1i,gcTst, type="prob")[,'1']  
#now apply the prediction function from ROCR to get a prediction object
rocPredTsti = prediction(scoreTsti, gcTst$RESPONSE, label.ordering = c('0', '1'))  
#obtain performance using the function from ROCR, then plot
perfROCTsti=performance(rocPredTsti, "tpr", "fpr")
plot(perfROCTsti)
cost.perfi = performance(rocPredTsti, "cost")
rocPredTsti@cutoffs[[1]][which.min(cost.perfi@y.values[[1]])]
# 493 
#0.75 

#Lift
lift.perfi = performance(rocPredTsti,measure = "lift")
lift.perfi@y.values
plot(lift.perfi)

#AUC
acc.perfi = performance(rocPredTsti, measure = "acc")
plot(acc.perfi)
auc.perfi = performance(rocPredTsti, measure = "auc")
auc.perfi@y.values

#Value:0.7193116

#From ROC plots and the high AUC value we see that the model is robust and reliable.

# 3(a) In developing the models above, 
#change decision tree options as you
#find reasonable (for example, complexity parameter (cp), 
#the minimum number of cases for split and at a leaf node, the split criteria, etc.) 
#- explain which parameters you experiment with and why. 
#Report on if and how different parameters affect performance.


# Develop a tree on the training data
set.seed(123)
rpModel1=rpart(RESPONSE ~ ., data = gcTrn, method="class")

#### Control parameters 
# cp=0.001 - minsplit = 10| Information Gain
#Training Data
set.seed(123)
rpMode_1=rpart(RESPONSE ~ ., data=gcTrn, method="class", parms = list(split ='information'), control = rpart.control(minsplit = 10, cp=0.001))
predTrn_1 = predict(rpMode_1, gcTrn, type="class")
table(pred=predTrn_1, true=gcTrn$RESPONSE)
#true
#pred   0   1
#0    129  32
#1    20 319
mean(predTrn_1==gcTrn$RESPONSE)
#89.6%
#Testing Data
predTst_1 = predict(rpMode_1, gcTst, type="class")
table(pred=predTst_1, true=gcTst$RESPONSE)
#true
#pred   0   1
#0    71  98
#1    80 251
mean(predTst_1==gcTst$RESPONSE)
#64.4%

# cp = 0.1 - minsplit = 10 | Information Gain
#Training Data
set.seed(123)
rpMode_2=rpart(RESPONSE ~ ., data=gcTrn, method="class", parms = list(split ='information'), control = rpart.control(minsplit = 10, cp=0.1))
predTrn_2 = predict(rpMode_2, gcTrn, type="class")
table(pred=predTrn_2, true=gcTrn$RESPONSE)
#true
#pred   0   1
#0      0   0
#1    149 351
mean(predTrn_2==gcTrn$RESPONSE)
#70.2%
#Testing Data
predTst_2 = predict(rpMode_2, gcTst, type="class")
table(pred=predTst_2, true=gcTst$RESPONSE)
#true
#pred   0   1
#0     0   0
#1    151 349
mean(predTst_2==gcTst$RESPONSE)
#69.8%
#After changing cp drastically to 0.1 from 0.001, we notice that in the confusion matrix,
#the bad creditors are all predicted as good creditors.
#The accuracy for the model predicted on training data decreases by 19.4% 
#and the accuracy for the model predicted on test data increases by 5.4%

# cp = 0.001 - minsplit = 5| Information gain
#training data
set.seed(123)
rpMode_3=rpart(RESPONSE ~ ., data=gcTrn, method="class", parms = list(split ='information'), control = rpart.control(minsplit = 5, cp=0.001))
predTrn_3 = predict(rpMode_3, gcTrn, type="class")
table(pred=predTrn_3, true=gcTrn$RESPONSE)
#true
#pred   0   1
#0    137  16
#1    12 335
mean(predTrn_3==gcTrn$RESPONSE)
#94.4%
#testing data
predTst_3 = predict(rpMode_3, gcTst, type="class")
table(pred=predTst_3, true=gcTst$RESPONSE)
#true
#pred   0   1
#0     67  93
#1    84 256
mean(predTst_3==gcTst$RESPONSE)
#64.6%
#After keeping cp value as 0.001
#and changing minsplit to 5,
#we observe that the accuracy for the model predicted on training data increased to 94.5% from 89.6%, 
#and the accuracy for the model predicted on testing data increased by 0.2% 

# 3(a) Describe the pruning method used here. How do you examine the effect of different values of cp, and how do you select the best pruned tree.
# Which decision tree parameter values do you find to be useful for developing a good model?

# Pruning:
set.seed(123)
#Now we will do post pruning. We will use the base model rpModel1 for our predictions
rpModel1=rpart(RESPONSE ~ ., data=gcTrn, method="class",parms = list(split ='information'))
predTrnl=predict(rpModel1, gcTrn, type='class')
#Confusion table
table(pred = predTrnl, true=gcTrn$RESPONSE)
#true
#pred   0   1
#0    79  21
#1    70 330
#Accuracy
mean(predTrnl==gcTrn$RESPONSE)
#81.8%  
predTrnl=predict(rpModel1, gcTst, type='class')
#Confusion table
table(pred = predTrnl, true=gcTst$RESPONSE)
#Accuracy
mean(predTrnl==gcTst$RESPONSE)
#72%  
printcp(rpModel1)
rpModel1$cptable[which.min(rpModel1$cptable[,"xerror"]),"CP"] #CP value= 0.02908277
plotcp(rpModel1)
#Now pruning on cp values = 0.02908277 and 0.012
rpModelPrune <- prune(rpModel1, cp = 0.02908277)
#Training data
set.seed(123)
predTrnPrune <- predict(rpModelPrune, gcTrn, type = "class",parms = list(split ='information'))
table(pred=predTrnPrune,true=gcTrn$RESPONSE)
mean(predTrnPrune==gcTrn$RESPONSE)
#79.2%
#Testing data
set.seed(123)
predTstPrune <- predict(rpModelPrune, gcTst, type = "class",parms = list(split ='information'))
table(pred=predTstPrune,true=gcTst$RESPONSE)
mean(predTstPrune==gcTst$RESPONSE)
#73.6%  
rpModelPrune <- prune(rpModel1, cp = 0.012)

#Training data
set.seed(123)
predTrnPrune <- predict(rpModelPrune, gcTrn, type = "class",parms = list(split ='information'))
table(pred=predTrnPrune,true=gcTrn$RESPONSE)
mean(predTrnPrune==gcTrn$RESPONSE)
#80.8%
#Testing data
set.seed(123)
predTstPrune <- predict(rpModelPrune, gcTst, type = "class",parms = list(split ='information'))
table(pred=predTstPrune,true=gcTst$RESPONSE)
mean(predTstPrune==gcTst$RESPONSE)
#71.6%

# 1.We first find the base model accuracy for models predicted on training data and test data,
#after which that we find the optimal cp value where the cross validation error (xerror) 
#and where relative error (rel error) have minimum value.
# 2.From that we find two optimal cp values, 0.02908277 and 0.012
# 3.By keeping these optimal values of cp, 
#the accuracy for the training data decreased but for the testing increased.

# 4.We find optimal cp value to be 0.028, with minsplit as 10 and split criteria as information gain.

# For part b, C5 cannot handle null values, therefore reload the data and handle the missing values

##3(b)
# b Consider another type of decision tree - C5.0 - experiment with the parameters till you get a 'good' model. Summarize the parameters and performance you obtain.
# Also develop a set of rules from the decision tree, and compare performance.



set.seed(123)
TRG_PCT=0.5
nr=nrow(md)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE) #get a random 50% sample of row-indices
gcTrn=md[trnIndex,]   #training data with the randomly selected row-indices
gcTst = md[-trnIndex,]

install.packages('C50')
library(C50)
# Model 1
set.seed(123)
c5model_1 <- C5.0(RESPONSE ~ ., data = gcTrn) 
summary(c5model_1)
p_1 = predict(c5model_1, gcTst, type='class')
table(pred = p_1, true = gcTst$RESPONSE)
mean(p_1 == gcTst$RESPONSE) # 71.2%

# Model 2
set.seed(123)
c5model_2 <- C5.0(RESPONSE ~ ., data = gcTrn, rules = FALSE, trials = 1,
                  control = C5.0Control(subset =  FALSE, winnow = FALSE, CF = 0.25, minCases = 2, 
                                        noGlobalPruning = FALSE), costs = NULL) 
summary(c5model_2)
p_2 = predict(c5model_2, gcTst, type='class')
table(pred = p_1, true = gcTst$RESPONSE)
mean(p_2 == gcTst$RESPONSE) # 74.4%

# Model 3
set.seed(123)
c5model_3 <- C5.0(RESPONSE ~ ., data = gcTrn, rules = TRUE, trials = 5,
                  control = C5.0Control(subset =  TRUE, winnow = FALSE, CF = 0.25, minCases = 2, 
                                        noGlobalPruning = FALSE), costs = NULL) 
summary(c5model_3)
p_3 = predict(c5model_3, gcTst, type='class')
table(pred = p_3, true = gcTst$RESPONSE)
mean(p_3 == gcTst$RESPONSE) # 74.6%

# Model 4
set.seed(123)
c5model_4 <- C5.0(RESPONSE ~ ., data = gcTrn, rules = TRUE, trials = 5,
                  control = C5.0Control(subset =  TRUE, winnow = FALSE, CF = 0.25, minCases = 10, 
                                        noGlobalPruning = FALSE), costs = NULL) 
summary(c5model_4)
p_4 = predict(c5model_4, gcTst, type='class')
table(pred = p_4, true = gcTst$RESPONSE)
mean(p_4 == gcTst$RESPONSE) # 75%

# Model 5
set.seed(123)
c5model_5 <- C5.0(RESPONSE ~ ., data = gcTrn, rules = TRUE, trials = 5,
                  control = C5.0Control(subset =  TRUE, winnow = FALSE, CF = 0.25, minCases = 20, 
                                        noGlobalPruning = FALSE), costs = NULL) # 75.2%
summary(c5model_5)
p_5 = predict(c5model_5, gcTst, type='class')
table(pred = p_5, true = gcTst$RESPONSE)
mean(p_5 == gcTst$RESPONSE) # 74%

# Model 6
set.seed(123)
c5model_6 <- C5.0(RESPONSE ~ ., data = gcTrn, rules = TRUE, trials = 5,
                  control = C5.0Control(subset =  TRUE, winnow = FALSE, CF = 0.95, minCases = 10, 
                                        noGlobalPruning = FALSE), costs = NULL) 
summary(c5model_6)
p_6 = predict(c5model_6, gcTst, type='class')
table(pred = p_6, true = gcTst$RESPONSE)
mean(p_6 == gcTst$RESPONSE) # 75%
# Since c5model_3 (CF=0.25, mincases=2, trials=5) gives the best accuracy, this is our best model and the performance measures are:
# Recall, precision and specificity

library(caret)
library(e1071)
confusionMatrix(p_3, gcTst$RESPONSE)  # Sensitivity and specificity
confusionMatrix(p_3, gcTst$RESPONSE,mode = "prec_recall") # precision and recall
summary(c5model_3)
# ROC, AUC, Lift
# ROC 
scoreTst3 = predict(c5model_3,gcTst, type="prob")[,'1']  
#now apply the prediction function from ROCR to get a prediction object
rocPredTst3 = prediction(scoreTst3, gcTst$RESPONSE, label.ordering = c('0', '1'))  
#obtain performance using the function from ROCR, then plot
perfROCTst3=performance(rocPredTst3, "tpr", "fpr")
plot(perfROCTst3)
#Cost ROC
cost.perf3 = performance(rocPredTst3, "cost")
rocPredTst3@cutoffs[[1]][which.min(cost.perf3@y.values[[1]])]#0.591574
#AUC
acc.perf3 = performance(rocPredTst3, measure = "acc")
plot(acc.perf3)
#AUC value
auc.perf3 = performance(rocPredTst3, measure = "auc")
auc.perf3@y.values#0.7549859
#Lift
lift.perf3 = performance(rocPredTst3,measure = "lift")
lift.perf3@y.values
plot(lift.perf3)

## 3(c) Decision tree models are referred to as 'unstable' - in the sense that small differences in
#training data can give very different models. Examine the models and performance for
#different samples of the training/test data (by changing the random seed). Do you find your
#models to be unstable -- explain?

set.seed(123)
TRG_PCT=0.5
nr=nrow(md)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE) #get a random 50%sample of row-indices
gcTrn_123 = md[trnIndex,]   #training data with the randomly selected row-indices
gcTst_123 = md[-trnIndex,]  #test data with the other row-indices

#seed value = 123
set.seed(123)
rpModel_123=rpart(RESPONSE ~ ., data=gcTrn_123, method="class")

predTrn_123=predict(rpModel_123, gcTrn_123, type='class')
mean(predTrn_123==gcTrn_123$RESPONSE)
#81.4%
predTest_123 = predict(rpModel_123, gcTst_123, type="class")
mean(predTest_123==gcTst_123$RESPONSE)
#70.8%

#seed value = 5
set.seed(5)
TRG_PCT=0.5
nr=nrow(md)
trnIndex1 = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE) #get a random 50%sample of row-indices
gcTrn_5 = md[trnIndex1,]   #training data with the randomly selected row-indices
gcTst_5 = md[-trnIndex1,]  #test data with the other row-indices

#develop a tree on the training data
set.seed(5)
rpModel_5=rpart(RESPONSE ~ ., data=gcTrn_5, method="class")

#Obtain the model's predictions on the training data
predTrn_5=predict(rpModel_5, gcTrn_5, type='class')
mean(predTrn_5==gcTrn_5$RESPONSE)
#84.4%

#Obtain the model's predictions on the test data
predTest_5 = predict(rpModel_5, gcTst_5, type="class")
mean(predTest_5==gcTst_5$RESPONSE)
#72%

#seed value = 999
set.seed(999)
TRG_PCT=0.5
nr=nrow(md)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE)
gcTrn_999 = md[trnIndex,]   #training data with the randomly selected row-indices
gcTst_999 = md[-trnIndex,]  #test data with the other row-indices

#develop a tree on the training data
set.seed(999)
rpModel_999 = rpart(RESPONSE ~ ., data=gcTrn_999, method="class")

# prediction on training
predTrn_999 = predict(rpModel_999, gcTrn_999, type='class')
mean(predTrn_999 == gcTrn_999$RESPONSE) 
#83.2%

# prediction on test
predTst_999 = predict(rpModel_999, gcTst_999, type='class')
mean(predTst_999 == gcTst_999$RESPONSE) 
# 75.6%

#Seed value = 123 
#Accuracy on training model = 81.4%
#Accuracy on test model = 70.8%
#Seed value = 5
#Accuracy on training model = 84.4%
#Accuracy on test model = 72%
#Seed value = 999
#Accuracy on training model = 83.2%
#Accuracy on test model = 75.6%

#We notice that on different seed values, the accuracy on Training data differ as much as 3% change, as compared to the accuracy on Test data with a change of 4.8%

## 3(d) Which variables are important for separating 'Good' from 'Bad' credit? Determine variable importance from the different 'best' trees. Are there similarities, differences?

set.seed(123)
rpModel1=rpart(RESPONSE ~ ., data=gcTrn, method="class", parms = list(split ='information'), control = rpart.control(minsplit = 10, cp=0.001)) 
summary(rpModel1)
# Variable importance
# AMOUNT         CHK_ACCT        DURATION          AGE         PERSONAL_STATUS         PRESENT_RESIDENT
# 22             12              8                 8                7                   6
set.seed(123)
rpModel1=rpart(RESPONSE ~ ., data=gcTrn, method="class", parms = list(split ='information'), control = rpart.control(cp=0.02908277))
summary(rpModel1) 
# Variable importance
# CHK_ACCT       AMOUNT         DURATION         HISTORY           SAV_ACCT
# 33             11              10                8                  8 
set.seed(123)
c5model_3 <- C5.0(RESPONSE ~ ., data = gcTrn, rules = TRUE, trials = 5,
                  control = C5.0Control(subset =  TRUE, winnow = FALSE, CF = 0.25, minCases = 2, 
                                        noGlobalPruning = FALSE), costs = NULL)
summary(c5model_3)
# Attribute usage and decreasing variable importance: 
#100.00%	CHK_ACCT
#100.00%	GUARANTOR
#99.60%	NUM_CREDITS
#98.60%	PROP_UNKN_NONE
#93.20%	NUM_DEPENDENTS
#77.40%	TELEPHONE

#We see that with different 'Best' models, the important variables coming on the top part of the tree are nearly the same. With each model the percentage of observations change. Seeing one model we can match our expectaions with other models. The top 3 most important variables are CHK_ACCT, GUARANTOR AND NUM_CREDITS

# 3(e) Consider partitions of the data into 70% for Training and 30% for Test, and 80% for Training and 20% for Test and report on model and performance comparisons (for the decision tree learners considered above).
#In the earlier question, you had determined a set of decision tree parameters to work well. Do the same parameters give 'best' models across the 50-50, 70-30, 80-20 training-test splits?
#Are there similarities among the different models ..in, say, the upper part of the tree - and what does this indicate?
#Is there any specific model you would prefer for implementation?


## Developing the Decision Tree again on 50-50 % 
set.seed(123)
TRG_PCT=0.5
nr=nrow(md)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE) #get a random 50%sample of row-indices
gcTrn = md[trnIndex,]   #training data with the randomly selected row-indices
gcTst = md[-trnIndex,]  #test data with the other row-indices

set.seed(123)
#develop a tree on the training data
rpModel1=rpart(RESPONSE ~ ., data=gcTrn, method="class", parms = list(split ='information'))
predTrn=predict(rpModel1, gcTrn, type='class')
mean(predTrn==gcTrn$RESPONSE) #81.8%
predTest = predict(rpModel1, gcTst, type="class")
mean(predTest==gcTst$RESPONSE)  #72%
summary(rpModel1)
# Variable importance
# CHK_ACCT          AMOUNT          EMPLOYMENT        SAV_ACCT         DURATION 
# 24                12               11                 8                7 

set.seed(123)
rpModel1=rpart(RESPONSE ~ ., data=gcTrn, method="class", parms = list(split ='information'), control = rpart.control(minsplit = 10, cp=0.02908277))
predTrn=predict(rpModel1, gcTrn, type='class')
mean(predTrn==gcTrn$RESPONSE) #73.6
predTest = predict(rpModel1, gcTst, type="class")
mean(predTest==gcTst$RESPONSE)  #72.6%
summary(rpModel1)
# Variable importance
# CHK_ACCT        DURATION        SAV_ACCT        AMOUNT        HISTORY          OWN_RES 
# 54              11             11               8               6               5 

#Partition of Data based on 70-30 % of Training and test Data
set.seed(123)
TRG_PCT=0.7
nr=nrow(md)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE) #get a random 70%sample of row-indices
mdTrn = md[trnIndex,]   #training data with the randomly selected row-indices
mdTst = md[-trnIndex,]  #test data with the other row-indices

set.seed(123)
#develop a tree on the training data
rpModel1=rpart(RESPONSE ~ ., data=gcTrn, method="class", parms = list(split ='information'))
predTrn=predict(rpModel1, mdTrn, type='class')
mean(predTrn==mdTrn$RESPONSE) #78.4%
predTest = predict(rpModel1, mdTst, type="class")
mean(predTest==mdTst$RESPONSE)  #73.33%
summary(rpModel1)
# Variable importance
# CHK_ACCT         AMOUNT          EMPLOYMENT         SAV_ACCT         DURATION       HISTORY
# 24               12              11                 8                7              6

set.seed(123)
rpModel1=rpart(RESPONSE ~ ., data=mdTrn, method="class", parms = list(split ='information'), control = rpart.control(minsplit = 10, cp=0.02908277))
predTrn=predict(rpModel1, mdTrn, type='class')
mean(predTrn==mdTrn$RESPONSE) #78%
predTest = predict(rpModel1, mdTst, type="class")
mean(predTest==mdTst$RESPONSE)  #73.3%
summary(rpModel1)
# Variable importance
# CHK_ACCT         HISTORY          AMOUNT         DURATION       EMPLOYMENT 
# 40               13               13             9              8

#Partition of Data based on 80-20 % of Training and test Data
set.seed(123)
TRG_PCT=0.8
nr=nrow(md)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE) #get a random 80%sample of row-indices
mdTrn = md[trnIndex,]   #training data with the randomly selected row-indices
mdTst = md[-trnIndex,]  #test data with the other row-indices

set.seed(123)
#develop a tree on the training data
rpModel1=rpart(RESPONSE ~ ., data=mdTrn, method="class", parms = list(split ='information'))
predTrn=predict(rpModel1, mdTrn, type='class')
mean(predTrn==mdTrn$RESPONSE) #81.37%
predTest = predict(rpModel1, mdTst, type="class")
mean(predTest==mdTst$RESPONSE)  #73.5%
summary(rpModel1)
# Variable importance
# CHK_ACCT         AMOUNT         DURATION         HISTORY          SAV_ACCT 
# 31               13              12               12                9 

set.seed(123)
rpModel1=rpart(RESPONSE ~ ., data=mdTrn, method="class", parms = list(split ='information'), control = rpart.control(minsplit = 10, cp=0.02908277))
predTrn=predict(rpModel1, mdTrn, type='class')
mean(predTrn==mdTrn$RESPONSE) #76.1%
predTest = predict(rpModel1, mdTst, type="class")
mean(predTest==mdTst$RESPONSE)  #75%
summary(rpModel1)
# Variable importance
# CHK_ACCT         HISTORY         SAV_ACCT         DURATION          PRESENT_RESIDENT 
# 51               15              14                9                3


# As we are increase the partitioning parameter from 0.5 to 0.7 and finally 0.8 we see that some variables have gained in observation percentage. The variables seen in all the models are CHK_ACCT, DURATION, SAV_ACCT and HISTORY. We also observe that, this increment in training data observations leads to an increase in the number of observations percentage in the variable on 1st position of the model.

######------------------------------------------------------------------------------------------------######
###4
# Consider the net profit (on average) of credit decisions as:
# Accept applicant decision for an Actual "Good" case: 100DM, and
# Accept applicant decision for an Actual "Bad" case: -500DM

# a Use the misclassification costs to assess performance of a chosen model from Q 3 above. Compare model #performance. 

install.packages("NHEMOtree")
library('NHEMOtree')
install.packages("bmrm")
library('bmrm')
library('rpart')
install.packages("rpart.plot")
library('rpart.plot')
library('dplyr')

costMatrix <- matrix(c(0,100,500, 0), byrow=TRUE, nrow=2)
colnames(costMatrix) <- c('Predict Good','Predict Bad')
rownames(costMatrix) <- c('Actual Good','Actual Bad')
costMatrix


rpModel_cost = rpart(RESPONSE ~ ., data=gcTrn, method="class", parms = list(prior = c(.7, .3),loss = costMatrix, split ='information'), control =  rpart.control(minsplit = 10, cp=0.02908277))

# prediction on training data
predTrn_rp = predict(rpModel_cost, gcTrn, type='class')
#Confusion table
table(pred = predTrn_rp, true = gcTrn$RESPONSE)
#true
#pred   0   1
#0     64  35
#1     85 316
#Accuracy
mean(predTrn_rp == gcTrn$RESPONSE) # 77.8%

# prediction on test data
predTst_rp = predict(rpModel_cost, gcTst, type='class')
#Confusion table
table(pred = predTst_rp, true = gcTst$RESPONSE)
#true
#pred   0   1
#0    46  59
#1    105 290
#Accuracy
mean(predTst_rp == gcTst$RESPONSE) # 72.6%

#Examine how different cutoff values for classification threshold make a difference. Use the #ROC curve to choose a classification threshold which you think will be better than the default 0.5. What #is the best performance you find?
CTHRESH=0.7
set.seed(123)
# CHOSEN Model
rpModel1=rpart(RESPONSE ~ ., data=gcTrn, method="class",parms = list(prior = c(.7, .3),loss = costMatrix, split ='information'), control =  rpart.control(minsplit = 10, cp=0.02908277))

predProbTrn=predict(rpModel1, gcTrn, type='prob')
#Confusion table
predTrn = ifelse(predProbTrn[, '1'] >= CTHRESH, '1', '0')
ct = table( pred = predTrn, true=gcTrn$RESPONSE)
#Accuracy
mean(predTrn==gcTrn$RESPONSE)
#0.1 = 76.8%
#CTRESH values and accuracy Training Data
#0.2 = 77.8%
#0.3 = 67.4%    77.8%
####0.4 = 66.8%
#0.5 = 64.8%    66.8
####0.6 = 31.8%
#0.7 = 29.8%    31.8

set.seed(123)
CTHRESH=0.7
# CHOSEN Model
rpModel1=rpart(RESPONSE ~ ., data=gcTrn, method="class",parms = list(prior = c(.7, .3),loss = costMatrix, split ='information'), control =  rpart.control(minsplit = 10, cp=0.02908277))

predProbTst=predict(rpModel1, gcTst, type='prob')
#Confusion table
predTst = ifelse(predProbTst[, '1'] >= CTHRESH, '1', '0')
ct = table( pred = predTst, true=gcTst$RESPONSE)
#Accuracy
mean(predTst==gcTst$RESPONSE)
#CTRESH values and accuracy Test Data
#0.1 = 73.2%
#0.2 = 74.6%    72.6%
#0.3 = 64.4%    72.6%
####0.4 = 63.4%
#0.5 = 62.6%    63.4%
####0.6 = 31%
#0.7 = 31%
# We see that accuracy is maximum when the cthresh is set between 0.2,
#as the cthresh value increases from 0.2 to 0.5 the accuracy drops slightly from 77.8% to 66.8%  and
#as the cthresh value increases from 0.5 to 0.7, we see a sharp drop in accuracy from 66.8% to 31.8% in Training data

## We see that accuracy is maximum when the cthresh is set between 0.2,
#as the cthresh value increases from 0.2 to 0.5 the accuracy drops slightly from 72.6% to 63.4%  and 
#as the cthresh value increases from 0.5 to 0.7, we see a sharp drop in accuracy from 63.4% to 31% in Testing data)

# ROC 
rocpredTrn1=predict(rpModel1, gcTrn, type='prob')
#Confusion table
predTrn1234 = ifelse(rocpredTrn1[,'1'] >= CTHRESH, '1', '0')
ct1 = table( pred = predTrn1234, true=gcTrn$RESPONSE)
#Accuracy
mean(predTrn1234==gcTrn$RESPONSE)
#71.4%

library(ROCR)
#score test data set
gcTst$score<-predict(rpModel1,type='prob',gcTst)
pred1<-prediction(gcTst$score[,2],gcTst$RESPONSE)
perf12 <- performance(pred1,"tpr","fpr")
plot(perf12)


## 4(b) Calculate and apply the 'theoretical' threshold and assess performance - what do you notice, and how does this relate to the answer from (a) above.

th = costMatrix[1,2]/(costMatrix[2,1] + costMatrix[1,2])
th 
#0.1666667

##4(c) Use misclassification costs to develop the tree models (rpart and C5.0) - are the trees here different than ones obtained earlier? Compare performance of these two new models with those obtained earlier (in part 3a, b above).

# C50 model:
c50_cost <- C5.0(RESPONSE ~ ., data = gcTrn, rules = TRUE, trials = 5,
                 control = C5.0Control(subset =  TRUE, winnow = FALSE, CF = 0.25, minCases = 2, 
                                       noGlobalPruning = FALSE), costs = matrix(c(0,100,500,0),nrow = 2, byrow = TRUE)) # 66.8%
summary(c50_cost)
# 55.60 cost
# Prediction on test data:
prd = predict(c50_cost, gcTst, type='class')
table(pred = prd, true = gcTst$RESPONSE)
mean(prd == gcTst$RESPONSE) 
#Accuracy - 67%

# Rpart:
CTHRESH=0.2
set.seed(123)
# CHOSEN Model
rpModel1=rpart(RESPONSE ~ ., data=gcTrn, method="class",parms = list(prior = c(.7, .3),loss = costMatrix, split ='information'), control =  rpart.control(minsplit = 10, cp=0.02908277))

predProbTrn=predict(rpModel1, gcTrn, type='prob')
#Confusion table
predTrn = ifelse(predProbTrn[, '1'] >= CTHRESH, '1', '0')
ct = table( pred = predTrn, true=gcTrn$RESPONSE)
#Accuracy
mean(predTrn==gcTrn$RESPONSE) # 77.6%

predProbTst=predict(rpModel1, gcTst, type='prob')
#Confusion table
predTst = ifelse(predProbTst[, '1'] >= CTHRESH, '1', '0')
ct = table( pred = predTst, true=gcTst$RESPONSE)
#Accuracy
mean(predTst==gcTst$RESPONSE) # 74.6%



#######--------------------------------------------------------------------------------------------
##5
# Best decision tree =
set.seed(123)
rpModel=rpart(RESPONSE ~ ., data=gcTrn, method="class", maxdepth=15, parms = list(split ='information'), control = rpart.control(minsplit = 15, cp=0.02908277))
summary(rpModel)
print(rpModel)

library(rattle)
fancyRpartPlot(rpModel)
# Depth = 6
# No. of nodes= 17
# Variable importance
# CHK_ACCT         DURATION        SAV_ACCT        AMOUNT         HISTORY           
# 54              11                11            8               6

#Two relatively pure nodes:
#Node 3: Predicted as 1. Probability ratio of good vs bad cases = 0.86:0.14
#Node 45: Predicted as 0. Probability ratio of good vs bad cases = 0.28:0.72

modelLaplace <- naiveBayes(RESPONSE ~ ., data = gcTrn, laplace = 3)
predL <- predict(modelLaplace, gcTrn[,-1])
table(predL, gcTrn$RESPONSE)
# Predicted values of Laplace
#predL   0   1
#0  59  49
#1  90 302

#####------------------------------------------------------------------------------------------
### 6


# Develop a tree on the training data
rpModel=rpart(RESPONSE ~ ., data=mdTrn, method="class", parms = list(split ='information'), control = rpart.control(minsplit = 10, cp=0.02908277))

# Plot the tree
library(rpart.plot)
rpart.plot::prp(rpModel, type=2, extra=1)


#Examine model performance
# Obtain the model's predictions on the training data
set.seed(123)
predTrn=predict(rpModel, mdTrn, type='class')
# Confusion table
table(pred = predTrn, true=mdTrn$RESPONSE)
# Accuracy
mean(predTrn==mdTrn$RESPONSE)#76.125%

# Obtain the model's predictions on the test data
predTst=predict(rpModel, mdTst, type='class')
# Confusion table
table(pred = predTst, true=mdTst$RESPONSE)
# Accuracy
mean(predTst==mdTst$RESPONSE)#75%

#For this, first sort the validation data on predicted probability.
predTstProb=predict(rpModel, mdTst, type='prob')
head(predTstProb)
# Next we sort the data based on these values
# We need the score and actual class (RESPONSE) values
tstSc <- subset(mdTst, select=c("RESPONSE"))  # selects the RESPONSE column into tstSc
tstSc["score"]<-predTstProb[, 2]  #add a column named 'Score' with prob(1) values in the first column of predTstProb
# Sort by score
tstSc<-tstSc[order(tstSc$score, decreasing=TRUE),]
tstSc

# Then, for each validation case, calculate the actual cost/benefit of extending credit by adding a separate column for the cumulative net cost/benefit

tstSc$RESPONSE<-as.numeric(as.character(tstSc$RESPONSE))
str(tstSc)
# Obtain the cumulative sum of default cases captured .
tstSc$cumDefault<-cumsum(tstSc$RESPONSE)
head(tstSc)
# Plot the cumDefault values (y-axis) by numCases (x-axis)
plot(seq(nrow(tstSc)), tstSc$cumDefault,type = "l", xlab='#cases', ylab='#default')

# How far into the validation data would you go to get maximum net benefit? 
# In using this model to score future credit # applicants, what cutoff value for predicted probability would you recommend?

# ROCR curve

library('ROCR')
# Obtain the scores from the model for the class of interest, here, the prob('1')
scoreTst=predict(rpModel,mdTst, type="prob")[,'1']  
scoreTst
# Now apply the prediction function from ROCR to get a prediction object
rocPredTst = prediction(scoreTst, mdTst$RESPONSE, label.ordering = c('0', '1'))  
rocPredTst
# Obtain performance using the function from ROCR, then plot
perfROCTst=performance(rocPredTst, "tpr", "fpr")
plot(perfROCTst)
# Optimal cutoff
cost.perf = performance(rocPredTst, "cost")
rocPredTst@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
# 198 
# 0.6811594 
# Optimal cost with different costs for fp and fn
cost.perf = performance(rocPredTst, "cost", cost.fp = 3, cost.fn = 1)
rocPredTst@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
##199 
##0.8780488 

# Provide appropriate performance values to back up your recommendation
# Other performance measures with the performance function
acc.perf = performance(rocPredTst, measure = "acc")
plot(acc.perf)

# AUC vaue
auc.perf = performance(rocPredTst, measure = "auc")
auc.perf@y.values
#0.7024895

#Calculate the 'profit' lift for a model - for this we need the scores given by a model, and the actual class values.
#Assume a 'profit' value for correctly predicting a '1' case, and a 'cost' for mistakes.
#First, sort by descending score values then calculate the profits, and then the cumulative profits

library(dplyr)

PROFITVAL=100
COSTVAL=-500

scoreTst=predict(rpModel,mdTst, type="prob")[,2] 
prLifts=data.frame(scoreTst)
prLifts=cbind(prLifts, Resp=mdTst$RESPONSE)
head(prLifts)

prLifts=prLifts[order(-scoreTst) ,]  #sort by descending score
prLifts
# Add profit and cumulative profits columns
prLifts<-prLifts %>% mutate(profits=ifelse(prLifts$Resp=='1',PROFITVAL,COSTVAL),cumProfits=cumsum(profits))

plot(prLifts$cumProfits)
# Find the score coresponding to the max profit
maxProfit= max(prLifts$cumProfits)
maxProfit_Ind = which.max(prLifts$cumProfits)
maxProfit_score = prLifts$scoreTst[maxProfit_Ind]
print(c(maxProfit = maxProfit, scoreTst = maxProfit_score))
#maxProfit    scoreTst 
#3900.00    0.867 

#######---------------------------------------------------------------------------------------------
###(7)
#Develop a random forest model (using a 70:30 training: test data split). What random
#forest parameters do you try out, and what performance do you obtain?
#  Compare the performance of the best random forest and best decision tree models - show a
#ROC plot to help compare models, and also the maximum net benefit (as in question 6).


set.seed(123)

View(md)

TRG_PCT=0.7
nr=nrow(md)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE) #get a random 70%sample of row-indices
mdTrn = md[trnIndex,]   #training data with the randomly selected row-indices
mdTst = md[-trnIndex,]  #test data with the other row-indices

mdTrn$COAPPLICANT <- mdTrn$`CO-APPLICANT`

mdTst$COAPPLICANT <- mdTst$`CO-APPLICANT`

mdTrn<-mdTrn[,-c(9)] 

mdTst<-mdTst[,-c(9)] 

View(mdTrn)


rfModelRFTrn = randomForest(RESPONSE ~ .,data=mdTrn,ntree=200,importance=TRUE)
rfModelRFTrn

## Running the Random Forest model ###
library('randomForest')
#for reproducible results, set a specific value for the random number seed
set.seed(123)

#develop a model with 200 trees, and obtain variable importance
rfModel = randomForest(RESPONSE ~ ., data=mdTrn, ntree=200, importance=TRUE )
#check the model -- see what OOB error rate it gives

#Variable importance
importance(rfModelRFTrn)
varImpPlot(rfModelRFTrn)
#Draw the ROC curve for the randomForest model
perf_rfTst=performance(prediction(predict(rfModelRFTrn,mdTst, type="prob")[,2], mdTst$RESPONSE), "tpr", "fpr")
plot(perf_rfTst)


##Running on Decision Tree ####
#Using the best DT Model obtained
rpModelDt=rpart(RESPONSE ~ ., data=mdTrn, method="class", parms = list(split ='information'))
predDtTrn=predict(rpModelDt, mdTrn, type='class')
mean(predDtTrn==mdTrn$RESPONSE) #78.4%
predDtTest = predict(rpModelDt, mdTst, type="class")
mean(predDtTest==mdTst$RESPONSE)  #73.33%
summary(rpModelDt)
library('rpart')
#for reproducible results, set a specific value for the random number seed
set.seed(123)


#check the model -- see what OOB error rate it gives

#Draw the ROC curve for the Decision tree
perf_DtTst=performance(prediction(predict(rpModelDt,mdTst, type="prob")[,2], mdTst$RESPONSE), "tpr", "fpr")
plot(perf_DtTst)


dev.off()

plot(perf_DtTst, col='red',add=TRUE)
#plot(perfRoc_dt2Tst, col='blue', add=TRUE)
plot(perf_rfTst, col='green', add=TRUE)
legend('bottomright', c('DecisionTree', 'RandomForest'), lty=1, col=c('red', 'green'))
