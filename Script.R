#Machine Learning Assignment
#Author: RM
#Date: 17.02.2017
# Install packages
install.packages("gmodels")
library(gmodels)

install.packages("randomForest")
library(randomForest)

#Load Data - Fill in Directory
data <- read.csv("Fill in Directory")
str(data)

#Check for missing values
colSums(sapply(data,is.na))

#Replace missing values by mode
summary(data$Country)
data$Country[is.na(data$Country)] <- "stnzd"

summary(data$DeviceType)
data$DeviceType[is.na(data$DeviceType)] <- "desktop"

summary(data$ResponsePromise)
data$ResponsePromise[is.na(data$ResponsePromise)] <- "1 minute"


#Define Classes

data$DisplayTime <- strptime(data$DisplayTime, format="%Y-%m-%d %H:%M:%S", tz= "CET")
data$PreviousDisplayTime <- strptime(data$PreviousDisplayTime, format="%Y-%m-%d %H:%M:%S", tz= "CET")
data$CurrentSessionStartTime <- strptime(data$CurrentSessionStartTime, format="%Y-%m-%d %H:%M:%S", tz= "CET")
data$FirstSessionStartTime <- strptime(data$FirstSessionStartTime, format="%Y-%m-%d %H:%M:%S", tz= "CET")
data$PreviousSessionStartTime <- strptime(data$PreviousSessionStartTime, format="%Y-%m-%d %H:%M:%S", tz= "CET")

#Create difference for the session 
data$differencesession <- difftime(data$CurrentSessionStartTime, data$PreviousSessionStartTime, units=c("mins"))
data$differencesession[is.na(data$differencesession)] <- 0
data$differencesession <- as.numeric(data$differencesession, units= "days")

#Create difference for the offer 
data$differenceoffer <- difftime(data$DisplayTime, data$PreviousDisplayTime, units=c("mins"))
data$differenceoffer[is.na(data$differenceoffer)] <- 0
data$differenceoffer <- as.numeric(data$differenceoffer, units= "days")

#Define right class
data$TrafficSource <- as.factor(data$TrafficSource)
class(data$TrafficSource)

data$TotalVisits <- as.numeric(data$TotalVisits)
data$ClicksBeforeThisDisplay <- as.numeric(data$ClicksBeforeThisDisplay)
data$PreviousDisplaysThisSession <- as.numeric(data$PreviousDisplaysThisSession)
data$Country <-  as.factor(data$Country)
data$MessageType <- as.factor(data$MessageType)
data$ResponsePromise <- as.factor(data$ResponsePromise)
data$DisplayReason <- as.factor(data$DisplayReason)
data$CallCenterStatus <- as.factor(data$CallCenterStatus)
data$DeviceType <- as.factor(data$DeviceType)
data$Contact <- as.factor(data$Contact)
data$differenceoffer <- as.numeric(data$differenceoffer)
data$differencesession <- as.numeric(data$differencesession)
#Subset Data set
df <- subset(data, select=-c(RowId, UID, DisplayTime, LandingPage, FirstSessionStartTime, PreviousSessionStartTime, CurrentSessionStartTime, PreviousDisplayTime, CurrentURL, Country ))
positive <- df[df$Contact==1,]
negative <- df[df$Contact==0,]
sample <- df[sample(nrow(negative), nrow(positive), replace=FALSE),]
train <- rbind(positive,sample)
#Impute missing values in Traffic Source to apply on train sample
train <- rfImpute(Contact~., iter=5, ntree=10, data=train)

# logistic regression model
logistic <- glm(Contact~ TotalVisits + ClicksBeforeThisDisplay + DeviceType +ResponsePromise + DisplayReason, family="binomial", data= train)
summary(logistic)
logisticprediction <- predict(logistic, train, type = "response")
logisticprediction <- ifelse(logisticprediction > 0.5,1,0)
misclassificationError_logistic <- mean(logisticprediction != train$Contact)


#Decision Tree Model
tree <- rpart(Contact~., data= train, method = "class")
tree_prediction <- predict(tree, train, type ="class")
misclassificationError_tree <- mean(tree_prediction != train$Contact)
print(paste('Accuracy Decision Tree',1-misclassificationError_tree))
rpart.plot(tree)



#Random Forest
forest <- randomForest(Contact~., data=train, ntree=100)
forest_prediction <- predict(forest, train, type ="class")
misclassificationError_forest <- mean(forest_prediction != train$Contact)
print(paste('Accuracy Random Forest',1-misclassificationError_forest))


dfprediction <- predict(forest,df, type = "class")
misclassificationError_df <- mean(dfprediction != df$Contact)
print(paste('Accuracy Random Forest on df',1-misclassificationError_df))




#Exploring Correlations
barplot(prop.table(table(df$Contact)))
joint= CrossTable(data$TrafficSource,data$TotalVisits, prop.chisaq = FALSE)

plot(data$TrafficSource, data$TotalVisits)
plot(data$TrafficSource, data$differenceoffer)


#simple model
model <- glm(Contact~TotalVisits, family="binomial", data= df)
summary(model)
predictions <- predict(model,df,type = "response")
predictions <- ifelse(predictions > 0.5,1,0)
misclassificationError <- mean(predictions != df$Contact)
print(paste('Accuracy',1-misclassificationError))

#decision tree model
model <- rpart(Contact~TotalVisits + ResponsePromise + ClicksBeforeThisDisplay + DeviceType + CallCenterStatus, data= df, method = "class", control= rpart.control(minsplit=10, cp=0))
