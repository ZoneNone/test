######################################################################################
#A Logistic Regression model to predict customer probability to buy a new plan
#Here we train the model with 70% of train data and 30% of test data
#then predict the new set of data
######################################################################################
#Include the library
library(ROCR)
data1 = read.csv("H:/Analytics/Analytics_Workouts/R/Data/test_data_hack.csv", header=T, sep=",", stringsAsFactor=FALSE)
#If you have a sql table use RODBC package with sql connection and query for the table to a df object

#Let's take a look at the data
head(data1) #displays 6 records by default
str(data1) #look at the structure of the data

data1$gender = factor(data1$gender) #convert the char to factor type
data1$status = factor(data1$status) #convert the char to factor type

set.seed(2) #we don't want to get different result everytime we run
data1$ind<-sample(2,nrow(data1),replace=TRUE,prob=c(0.7,0.3)) #sampling training and test data as 70-30 ratio

#Let's take a look at the data
head(data1)

#Save the data based on indicator to train and test data frame object
trainData<-data1[(data1$ind==1),]
testData<-data1[(data1$ind==2),]

#Check the count
nrow(trainData)
nrow(testData)

#Run the Logistic Regression glm method "logit" algorithm
mylogit <- glm(planproposed ~ age + gender + plandata + planamount, data = trainData, family = "binomial"(link=logit))

#Evaluate the prediction on 30% test data
testData$pred <- predict(mylogit,testData,type="response")
#pred<-prediction(testData$pred, testData$planproposed)
#perf <- performance(pred,"tpr","fpr")
#plot(perf, lty=1)

#####confusion matrix######
#pred_df <- data.frame(dep_var = testData$planproposed, pred = testData$pred)
#confusion_matrix(pred_df, cutoff = 0.6)

#Predict the new data
newdata <- read.csv("H:/Analytics/Analytics_Workouts/R/Data/predictHackTest2.csv", header=T, sep=",", stringsAsFactor=FALSE)
#If you have a sql table use RODBC package with sql connection and query for the table to a df object

head(newdata)
str(newdata)

newdata$gender = factor(newdata$gender)

newdata$predP <- predict(mylogit,newdata,type="response")
newdata$predB <- round(predict(mylogit,newdata,type="response"))

write.csv(newdata,"H:/Analytics/Analytics_Workouts/R/Data/Prediction.csv")
