# installing important libraries
library(DataExplorer)
library(pastecs)
library(readxl)
library(ggplot2)
library(dplyr)
library(party)
library(rpart)
library(rpart.plot)
library(datasets)
library(caTools)
library(party)

# loading the dataset
Employee_Ret<-read.csv("C:/Users/User/Desktop/MBA/HR-Employee-Attrition.csv",stringsAsFactors = TRUE)
Employee_Ret
# DATA CLEANING
# checking if any NA values are there
anyNA(Employee_Ret)
# checking structure of dataset
str(Employee_Ret)
# removing columns EmployeeCount,Over18,StandardHours as they won't be helpful in our analysis
Employee_Ret<-Employee_Ret[,-c(9,22,27)]
glimpse(Employee_Ret)
dim(Employee_Ret)
# converting Attrition column into numeric values of 0 and 1 where 1 means Yes , 0 means No
Employee_Ret$Attrition = ifelse( Employee_Ret$Attrition == "Yes", 1, 0)
#Employee_Ret$BusinessTravel = ifelse( Employee_Ret$BusinessTravel == "Travel_Frequently", 2,1, 0)
Employee_Ret$TravelFrequently =
  as.factor(ifelse(Employee_Ret$BusinessTravel == "Travel_Frequently", "Yes", "No"))
#checking class of Attrition and Business travel
class(Employee_Ret$Attrition)
class(Employee_Ret$BusinessTravel)
# structure of dataset
str(Employee_Ret)
# dimension of dataset
dim(Employee_Ret)
# summary of dataset
summary(Employee_Ret)
# contigency table for better insights
table(Employee_Ret$Attrition)
table(Employee_Ret$BusinessTravel)
table(Employee_Ret$JobSatisfaction)
table(Employee_Ret$MaritalStatus)
table(Employee_Ret$OverTime)
table(Employee_Ret$JobLevel)

table_PR_Att<- xtabs(~PerformanceRating+Attrition,data=Employee_Ret)
round(prop.table(table_PR_Att,1)*100,2)

table_OT_Att<- xtabs(~OverTime+Attrition,data=Employee_Ret)
round(prop.table(table_OT_Att,1)*100,2)

table_WB_Att<- xtabs(~WorkLifeBalance+Attrition,data=Employee_Ret) 
round(prop.table(table_WB_Att,1)*100,2)

table_JR_Att<- xtabs(~JobRole+Attrition,data=Employee_Ret) 
round(prop.table(table_JR_Att,1)*100,2)

table_Comp_Att<- xtabs(~NumCompaniesWorked+Attrition,data=Employee_Ret)
round(prop.table(table_Comp_Att,1)*100,2)

table_MR_Att<- xtabs(~MaritalStatus+Attrition,data=Employee_Ret) 
round(prop.table(table_MR_Att,1)*100,2) 

table_gen_Att<- xtabs(~Gender+Attrition,data=Employee_Ret) 
round(prop.table(table_gen_Att,1)*100,2)

table_JS_Att<- xtabs(~JobSatisfaction+Attrition,data=Employee_Ret)
round(prop.table(table_JS_Att,1)*100,2)

table_ES_Att<- xtabs(~EnvironmentSatisfaction+Attrition,data=Employee_Ret) 
round(prop.table(table_ES_Att,1)*100,2)

table_Edu_Att<- xtabs(~Education+Attrition,data=Employee_Ret) 
round(prop.table(table_Edu_Att,1)*100,2)

table_BT_Att<- xtabs(~BusinessTravel+Attrition,data=Employee_Ret) 
round(prop.table(table_BT_Att,1)*100,2)

table_EduFie_Att<- xtabs(~EducationField+Attrition,data=Employee_Ret) 
round(prop.table(table_EduFie_Att,1)*100,2)

# VISUALIZATION OF DATASET
# boxplot b/w MonthlyRate and Attrition
boxplot(Employee_Ret$MonthlyRate~Employee_Ret$Attrition,main="Boxplot",xlab="MonthlyRate",
        ylab = "Attrition",horizontal=TRUE,col=c("lightgreen","lightyellow"))
# boxplot b/w DistanceFromHome and Attrition
boxplot(Employee_Ret$DistanceFromHome~Employee_Ret$Attrition,main="Boxplot",xlab="DistanceFromHome",
        ylab = "Attrition",horizontal=TRUE,col=c("lightgreen","lightyellow"))
# boxplot b/w HourlyRate and Attrition
boxplot(Employee_Ret$HourlyRate~Employee_Ret$Attrition,main="Boxplot",xlab="HourlyRate",
        ylab = "Attrition",horizontal=TRUE,col=c("lightgreen","lightyellow"))
# boxplot b/w DailyRate and Attrition
boxplot(Employee_Ret$DailyRate~Employee_Ret$Attrition,main="Boxplot",xlab="DailyRate",
        ylab = "Attrition",horizontal=TRUE,col=c("lightgreen","lightyellow"))
# boxplot b/w ï..Age and Attrition
boxplot(Employee_Ret$ï..Age~Employee_Ret$Attrition,main="Boxplot",xlab="Age",ylab = "Attrition",
        horizontal=TRUE,col=c("lightgreen","lightyellow"))
# boxplot b/w MonthlyIncome and Attrition
boxplot(Employee_Ret$MonthlyIncome~Employee_Ret$Attrition,main="Boxplot",xlab="MonthlyIncome",
        ylab = "Attrition",horizontal=TRUE,col=c("lightgreen","lightyellow"))

# histogram of YearsAtCompany 
hist(Employee_Ret$YearsAtCompany,xlab="YearsAtCompany",
     ylab="count",breaks=20,main="YearsAtcompany",col="lightpink",ylim=c(0,350))
# histogram of PercentSalaryHike 
hist(Employee_Ret$PercentSalaryHike,xlab="PercentSalaryHike",
     ylab="count",breaks=20,main="PercentSalaryHike",col="lightyellow")
# 

library(psych)
describe(Employee_Ret)
#group on the basis of categorical variable in this case gender
describeBy(Employee_Ret,group=Employee_Ret$Gender)

ggplot(Employee_Ret,aes(Attrition,fill=Gender))+geom_point()
ggplot(Employee_Ret,aes(Attrition,fill=Gender))+geom_smooth()
#from this we can say that when number of projects assigned to employees are less employee is likely to be retained

#CHI SQUARE TEST ON CATEGORICAL VARIABLES

tab1<-table(Employee_Ret$Attrition,Employee_Ret$WorkLifeBalance)
tab1
chisq.test(tab1)
#X-squared = 16.325, df = 3, p-value = 0.0009726
# since p value is less than 0.05 we reject null hypothesis and conlude that two variables are associated.
tab2<-table(Employee_Ret$Attrition,Employee_Ret$JobSatisfaction)
tab2
chisq.test(tab2)
#X-squared = 17.505, df = 3, p-value = 0.0005563
# since p value is less than 0.05 we reject null hypothesis and conlude that two variables are associated.
tab3<-table(Employee_Ret$Attrition,Employee_Ret$BusinessTravel)
tab3
chisq.test(tab3)
#X-squared = 24.182, df = 3, p-value = 5.609e-06
# since p value is less than 0.05 we reject null hypothesis and conlude that two variables are associated.
tab4<-table(Employee_Ret$Attrition,Employee_Ret$OverTime)
tab4
chisq.test(tab4)
#X-squared = 87.564, df = 1, p-value = 2.2e-16
# since p value is less than 0.05 we reject null hypothesis and conlude that two variables are associated.
tab5<-table(Employee_Ret$Attrition,Employee_Ret$MaritalStatus)
tab5
chisq.test(tab5)
#X-squared = 46.164, df = 2, p-value = 9.456e-11
# since p value is less than 0.05 we reject null hypothesis and conclude that two variables are associated.
tab6<-table(Employee_Ret$Attrition,Employee_Ret$Department)
tab6
chisq.test(tab6)
#X-squared = 10.796, df = 2, p-value =0.004526
# since p value is less than 0.05 we reject null hypothesis and conclude that two variables are associated.
tab7<-table(Employee_Ret$Attrition,Employee_Ret$JobLevel)
tab7
chisq.test(tab7)
#X-squared = 72.529, df = 4, p-value =0.004526
# since p value is less than 0.05 we reject null hypothesis and conclude that two variables are associated.
tab8<-table(Employee_Ret$Attrition,Employee_Ret$JobRole)
tab8
chisq.test(tab8)
#X-squared = 86.19, df = 8, p-value =2.752e-15  
# since p value is less than 0.05 we reject null hypothesis and conclude that two variables are associated.
tab9<-table(Employee_Ret$Attrition,Employee_Ret$EnvironmentSatisfaction)
tab9
chisq.test(tab9)
#X-squared = 22.504, df = 3, p-value =5.123e-05
# since p value is less than 0.05 we reject null hypothesis and conclude that two variables are associated.
tab10<-table(Employee_Ret$Attrition,Employee_Ret$EducationField)
tab10
chisq.test(tab10)
#X-squared = 16.025, df = 5, p-value =0.006774
# since p value is less than 0.05 we reject null hypothesis and conclude that two variables are associated.


# CORRELATION TEST FOR 2 NUMERICAL COLUMNS
cor.test(Employee_Ret$ï..Age,Employee_Ret$MonthlyIncome)
# r=0.497  , p=2.2e-16,
#null hypothesis r=0 , as per p value , reject H0 , accept cor is there
#here it is positive core 0.4583951 0.5353551

cor.test(Employee_Ret$ï..Age,Employee_Ret$TotalWorkingYears)
# r=0.6803805   , p=2.2e-16,
#null hypothesis r=0 , as per p value , reject H0 , accept cor is there
#here it is positive core  0.6519312 0.7069171

t.test(Employee_Ret$YearsSinceLastPromotion~Employee_Ret$Attrition)
#
t.test((Employee_Ret$MonthlyIncome~Employee_Ret$Attrition))
#
t.test(Employee_Ret$MonthlyRate~Employee_Ret$Attrition)
#
t.test(Employee_Ret$DailyRate~Employee_Ret$Attrition)
#------------------------------------------------------------------------------------------
# spliting dataset into training and testing data
set.seed(1)
Emp_Attrition<-sample(1:nrow(Employee_Ret),size=round(0.7*nrow(Employee_Ret)),replace = FALSE)
Employee_Ret_train<-Employee_Ret[Emp_Attrition,]
Employee_Ret_test<-Employee_Ret[-Emp_Attrition,]
# dimension of training data
dim(Employee_Ret_train)
# dimension of testing data
dim(Employee_Ret_test)
class(Employee_Ret_train$Attrition)
#creating logistic regression model
modelem<-glm(Employee_Ret_train$Attrition~
            Employee_Ret_train$ï..Age
          + Employee_Ret_train$BusinessTravel
          + Employee_Ret_train$JobInvolvement
          + Employee_Ret_train$OverTime
          + Employee_Ret_train$YearsInCurrentRole
          + Employee_Ret_train$EnvironmentSatisfaction
          +Employee_Ret_train$PercentSalaryHike
          + Employee_Ret_train$JobSatisfaction
          ,data=Employee_Ret_train,family="binomial")
summary(modelem)
# prediction using logistic regression model
res<-predict(modelem,Employee_Ret_test,type="response")
res
res<-predict(modelem,Employee_Ret_train,type="response")
res
# Validate the model using confusion model
conmatrix<-table(Actual_Value=Employee_Ret_train$Attrition,
                 Predicted_val=res>0.5)
conmatrix
#------------------------------------

#RANDOM FOREST

library(ggplot2)
library(randomForest)
# spliting dataset into training and testing data
Employee_Ret$Attrition=as.factor(Employee_Ret$Attrition)
Emp_Attrition<-sample(1:nrow(Employee_Ret),size=round(0.70*nrow(Employee_Ret)),replace = FALSE)
Employee_Ret_train<-Employee_Ret[Emp_Attrition,]
Employee_Ret_test<-Employee_Ret[-Emp_Attrition,]
# dimension of training data
dim(Employee_Ret_train)
# dimension of testing data
dim(Employee_Ret_test)
# creating Random Forest Model 
rf<-randomForest(Employee_Ret_train$Attrition~.,data=Employee_Ret_train,mtry=4,
                 ntree=2001,importance=TRUE)
rf
# confusin matrix using random forest
rf$confusion

plot(rf)
# prediction using random forest model
prediction<-predict(rf,newdata=Employee_Ret_test)
table(prediction,Employee_Ret_test$Attrition)
prediction

results<-cbind(prediction,Employee_Ret_test$Attrition)
results
colnames(results)<-c("pred","real")
results<as.data.frame(results)
View(results)
sum(prediction==Employee_Ret_test$Attrition)/nrow(Employee_Ret_test)







