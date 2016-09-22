library(C50)
data(churn)
str(churnTrain)
#column names used as predictors
predictors=names(churnTrain)[names(churnTrain)!="churn"]

library(caret)
###createDataPartition()-to split data into train() and test
## preprocess()-to get mean,sd, valeus,transform te data(scale)


#Boosting algorithms - original-ADA boost algorithm
#boost prediction of weak learners =single tree. boost by mofifying
#gbm-gradirnt boosting



#modelling ensembles=stacking models on top of one another
#invrease accuracy and stabilty
# use caret library
 library(caret)
names(getModelInfo())=# all the models in caret
  library(Rcurl)
library(RCurl)
urlfile <-'https://raw.githubusercontent.com/hadley/fueleconomy/master/data-raw/vehicles.csv'
x <- getURL(urlfile, ssl.verifypeer = FALSE)
vehicles <- read.csv(textConnection(x))
str(vehicles)






#We clean the outcome variable cyclinders by assigning it 1 for 6 cyclinders and 0 for everything else:
  
vehicles <- vehicles[names(vehicles)[1:24]]
vehicles <- data.frame(lapply(vehicles, as.character), stringsAsFactors=FALSE)
vehicles <- data.frame(lapply(vehicles, as.numeric))
vehicles[is.na(vehicles)] <- 0
vehicles$cylinders <- ifelse(vehicles$cylinders == 6, 1,0)

prop.table(table(vehicles$cylinders))


#Here is the one complicated part, instead of splitting the data into 2 parts of train and test, we split the data into 3 parts: ensembleData, blenderData, and testingData:
set.seed(1234)
vehicles <- vehicles[sample(nrow(vehicles)),]
split <- floor(nrow(vehicles)/3)
ensembleData <- vehicles[0:split,]
blenderData <- vehicles[(split+1):(split*2),]
testingData <- vehicles[(split*2+1):nrow(vehicles),]


#We assign the outcome name to labelName and the predictor variables to predictors:
  
labelName <- 'cylinders'
predictors <- names(ensembleData)[names(ensembleData) != labelName]

#We create a caret trainControl object to control the number of cross-validations performed (the more the better but for breivity we only require 3):
#by default train control uses bootstrap instead we want it to do CV 3 times
myControl <- trainControl(method='cv', number=3, returnResamp='none')

#Benchmark Model

#We run the data on a gbm model without any enembling to use as a comparative benchmark:
  
  test_model <- train(blenderData[,predictors], blenderData[,labelName], method='gbm', trControl=myControl)


  #You'll see a series of the lines (as shown above) as it trains thegbm model.
  
  #We then use the model to predict 6-cylinder vehicles using the testingData data set and pROC's auc function to get the Area Under the Curve (AUC):
    
    preds <- predict(object=test_model, testingData[,predictors])
  library(pROC)
  auc <- roc(testingData[,labelName], preds)
  print(auc$auc) 
  
  
  
 # Ensembles
  
  #But we're going to try. We now use 3 models - gbm, rpart, and treebag as part of our ensembles of models and train them with the ensembleData data set:
    
   model_gbm <- train(ensembleData[,predictors], ensembleData[,labelName], method='gbm', trControl=myControl)
  
  model_rpart <- train(ensembleData[,predictors], ensembleData[,labelName], method='rpart', trControl=myControl)
  
  model_treebag <- train(ensembleData[,predictors], ensembleData[,labelName], method='treebag', trControl=myControl)
  
  blenderData$gbm_PROB <- predict(object=model_gbm, blenderData[,predictors])
  blenderData$rf_PROB <- predict(object=model_rpart, blenderData[,predictors])
  blenderData$treebag_PROB <- predict(object=model_treebag, blenderData[,predictors])
  
  testingData$gbm_PROB <- predict(object=model_gbm, testingData[,predictors])
  testingData$rf_PROB <- predict(object=model_rpart, testingData[,predictors])
  testingData$treebag_PROB <- predict(object=model_treebag, testingData[,predictors])
  
  
 # Please note how easy it is to add those values back to the original data set (follow where we assign the resulting predictions above).
  
#  Now we train a final blending model on the old data and the new predictions (we use gbm but that is completely arbitrary):
    
    predictors <- names(blenderData)[names(blenderData) != labelName]
  final_blender_model <- train(blenderData[,predictors], blenderData[,labelName], method='gbm', trControl=myControl)
  
  preds <- predict(object=final_blender_model, testingData[,predictors])
  auc <- roc(testingData[,labelName], preds)
  print(auc$auc)
  
  
  
  #caret...................
  1) preProcess()
2) inbuilt data splitting
createDataPartition()
createResample()
createTimeSlices()
3) training testing functionCallString
train()
prredict()
4) Model comparison:
confusionMatrix()
  

#Machine learning algorithms
LDA
regression
SVM
Naive BAyer
Rf

unfying framework for predict()


####################################JHU caret
library(caret)
library(kernlab)
data(spam)
inTrain=createDataPartition(y=spam$type,p=0.75,list=FALSE)
training=spam[inTrain,]
testing=spam[-inTrain,]
dim(training)


modelFit=train(type~.,data=training,method="glm")
modelFit
modelFit$finalModel
predictions=predict(modelFit,newdata=testing)
confusionMatrix(predictions,testing$type)
