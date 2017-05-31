dataset<-read.csv("D:/R/Airline Data/program/2007.csv")
dataset$speedofplane<-(dataset$Distance)/(dataset$ActualElapsedTime/60)
head(dataset$speedofplane)
table(dataset$Month)
#a<-dataset[which(dataset$Origin=="ATL" & dataset$Dest=="DFW"),]
dataset1<-dataset[complete.cases(dataset),]
dataset2<-dataset1[which(dataset1$Cancelled==0 & dataset1$Diverted==0),]


sum(dataset2$CarrierDelay)
sum(dataset2$WeatherDelay)
sum(dataset2$NASDelay)
sum(dataset2$SecurityDelay)
sum(dataset2$LateAircraftDelay)

dim(dataset2[which(dataset2$ArrDelay>0 & dataset2$DepDelay>0 & dataset2$CarrierDelay==0 & dataset2$NASDelay==0 & dataset2$WeatherDelay==0 & dataset2$SecurityDelay==0 & dataset2$LateAircraftDelay==0),])

dim(dataset2[which(dataset2$CarrierDelay!=0 | dataset2$WeatherDelay!=0 |dataset2$NASDelay!=0 |dataset2$SecurityDelay!=0 |dataset2$LateAircraftDelay!=0),])

dim(dataset2[which(dataset2$WeatherDelay!=0),])
table(dataset2$WeatherDelay)

str(dataset2)
names(dataset2)
dataset3<-dataset2[,c(2,3,9:21,26)]

dataset3$Month<-as.factor(dataset3$Month)
dataset3$DayofMonth<-as.factor(dataset3$DayofMonth)

c<-dataset3$WeatherDelay
c<-as.data.frame(c)
c[which(c$c>0 & c$c<=15),]<-1
c[which(c$c>15 & c$c<=30),]<-2
c[which(c$c>30 & c$c<=50),]<-3
c[which(c$c>50 & c$c<=100),]<-4
c[which(c$c>100 & c$c<=350),]<-5
c[which(c$c>350),]<-6
d<-cbind(b,c)
d<-d[,-16]
names(d)
colnames(d)[16]<-"WeatherDelay"

############################################


library(caret)
library(doParallel)
library(foreach)
### Register parallel backend
avail_cores <- detectCores() # available cores
p_cluster <- makeCluster(avail_cores-2)
registerDoParallel(p_cluster)
sprintf("Cores registered: %d",getDoParWorkers())

exploreData <- d
finalData <- data.frame(exploreData)

varinfo <- nearZeroVar(exploreData, saveMetrics = TRUE)
sortedVarinfo <- varinfo[order(varinfo["percentUnique"]),]
sprintf("Total number of near-zero var preds: %d", sum(sortedVarinfo$nzv))
head(sortedVarinfo[sortedVarinfo$nzv==TRUE,])

varinfo <- nearZeroVar(exploreData, saveMetrics = TRUE)
sortedVarinfo <- varinfo[order(varinfo["percentUnique"]),]
factor_col_names <- setdiff(names(Filter(is.factor, exploreData)),c("WeatherDelay"))
print("Near zero variance:")
head(sortedVarinfo)
print("Factors variables:")
print(factor_col_names)
summary(exploreData[factor_col_names])
exploreData <- exploreData[setdiff(colnames(exploreData),factor_col_names)]
dropped_columns <- c(factor_col_names)

correlationMatrix <- cor(exploreData[,setdiff(colnames(exploreData),c("WeatherDelay"))])
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
print(colnames(exploreData)[highlyCorrelated])

inData1 <- createDataPartition(exploreData$WeatherDelay,p = 0.1, list = FALSE)
rfData1 <- exploreData[inData1,]
rfInData2 <- exploreData[-inData1,]
inData2 <- createDataPartition(rfInData2$WeatherDelay,p = 0.1, list = FALSE)
rfData2 <- rfInData2[inData2,]

x1 <- rfData1[,setdiff(colnames(rfData1),c("WeatherDelay"))]
y1 <- rfData1$WeatherDelay

x2 <- rfData2[,setdiff(colnames(rfData2),c("WeatherDelay"))]
y2 <- rfData2$WeatherDelay

# rfeCtrl <- rfeControl(functions=rfFuncs, 
#                       method="cv", 
#                       number=10,
#                       repeats = 3)
# results1 <- rfe(x1, y1, sizes=c(5,10,15,25,40), rfeControl=rfeCtrl)
# results2 <- rfe(x2, y2, sizes=c(5,10,15,25,40), rfeControl=rfeCtrl)
# plot(results1, type=c("g", "o"))
# plot(results2, type=c("g", "o"))
# final_predictors <- unique(c(predictors(results1),predictors(results2)))
# print("Final predictors :")
# print(final_predictors)
# 
# useAutomaticPredictors = TRUE
# usePCA = FALSE
# 
# if (useAutomaticPredictors){
#   good_columns <- c(final_predictors,c("WeatherDelay"))
#   pred_columns <- final_predictors 
# }else{
#   good_columns <- setdiff(colnames(finalData))
#   pred_columns <- setdiff(good_columns, c("WeatherDelay"))
#   
# }


finalData <- d
inTraining <- createDataPartition(finalData$WeatherDelay, p=0.6, list=FALSE)
trainingStd <- finalData[inTraining,]
testdataStd <- finalData[-inTraining,]
inVal <- createDataPartition(testdataStd$WeatherDelay, p=0.5, list=FALSE)
crossvalStd <- testdataStd[inVal,]
testingStd <- testdataStd[-inVal,]

if (usePCA)
{
  PCA.model <- preProcess(trainingStd[pred_columns],method="pca", thresh=0.95)
  training <- predict(PCA.model, trainingStd)
  crossvalidation <- predict(PCA.model,crossvalStd )
  testing <- predict(PCA.model, testingStd)  
} else
{
  training <- trainingStd
  crossvalidation <- crossvalStd
  testing <- testingStd
}




All.Methods <- c("lda","rpart","knn","lvq","xgbTree")#,"lssvmRadial")
nr_models <- length(All.Methods)
Cross.Accuracy <- c()
Training.Time <- c()
bestAccuracy <- 0 
#set.seed(12345)


curr.model <- train(WeatherDelay ~ .,data = training,method = "xgbTree")
preds<- predict(curr.model,crossvalidation)

cfm <- confusionMatrix(preds,crossvalStd$WeatherDelay)
Cross.Accuracy[c_model] <- cfm$overall['Accuracy']

table(is.na(b$WeatherDelay))


for (c_model in 1:nr_models){
  
  
  methodName <-  All.Methods[c_model]
  print(paste0("Training ",methodName,"..."))
  tmr_start <- proc.time()
  curr.model <- train(WeatherDelay ~ .,
                      data = training,
                      method = methodName)
  tmr_end <- proc.time()
  print(paste0("Done training ",methodName,"."))  
  Training.Time[c_model] = (tmr_end-tmr_start)[3]
  
  preds<- predict(curr.model,crossvalidation)
  
  cfm <- confusionMatrix(preds,crossvalStd$WeatherDelay)
  Cross.Accuracy[c_model] <- cfm$overall['Accuracy']
  
  if(bestAccuracy < Cross.Accuracy[c_model]){
    best.model <- curr.model
    bestAccuracy <- Cross.Accuracy[c_model]
  }
  
}


summary_info <- data.frame(All.Methods,Cross.Accuracy,Training.Time)
summary_info <- summary_info[order(summary_info$Cross.Accuracy),]
print(summary_info)



save(best.model, file = "D:/My Stuff/Application Calling/best_model1.rda")


print(paste("Predicting with:",best.model$method))
testpred <- predict(best.model,testing)
confusionMatrix(testpred,testingStd$WeatherDelay)

##
## now finally apply best model on unseen observation
##

unseen.data <- read.csv(paste0(mypath,"/pml-testing.csv"))
unseen.data <- unseen.data[pred_columns]

if (usePCA)
{
  unseen.data <- predict(PCA.model,unseen.data)
}

print(paste0("Now predicting unseen observations with ",best.model$method,":"))                        
finalPredictions <- predict(best.model,unseen.data)
print(finalPredictions)












