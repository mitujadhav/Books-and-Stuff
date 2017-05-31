Apr_data<-read.csv("D:/R/Airline Data/PushkarsirAirlinedata/2015/Apr2015.csv")
#Feb_data<-read.csv("D:/R/Airline Data/Pushkar sir Airline data/2015/Feb2015.csv")
#Jan_data<-read.csv("D:/R/Airline Data/Pushkar sir Airline data/2015/Jan2015.csv")
#jf_data<-rbind(Jan_data,Feb_data)

Apr_data<-Apr_data[which(Apr_data$CARRIER_DELAY!=""),]
library(dplyr)
Apr_data<-Apr_data[,c(4,6,7,8,13,21,26:35,39:47)]

ex<-Apr_data %>% group_by(DAY_OF_MONTH,TAIL_NUM) %>% arrange(CRS_DEP_TIME)%>% arrange(DAY_OF_MONTH,TAIL_NUM)
ex<-as.data.frame(ex)

names(ex)
table(ex$LATE_AIRCRAFT_DELAY)
table(ex$CARRIER_DELAY)
table(ex$WEATHER_DELAY)
table(ex$NAS_DELAY)
table(ex$SECURITY_DELAY)

###############################
## Group of 1

dim(ex[which(ex$SECURITY_DELAY!=0),])
dim(ex[which(ex$LATE_AIRCRAFT_DELAY!=0),])
dim(ex[which(ex$WEATHER_DELAY!=0),])
dim(ex[which(ex$NAS_DELAY!=0),])
dim(ex[which(ex$CARRIER_DELAY!=0),])

## Group of 2

dim(ex[which(ex$CARRIER_DELAY!=0 & ex$LATE_AIRCRAFT_DELAY!=0),])
dim(ex[which(ex$CARRIER_DELAY!=0 & ex$NAS_DELAY!=0),])
dim(ex[which(ex$CARRIER_DELAY!=0 & ex$WEATHER_DELAY!=0),])
dim(ex[which(ex$CARRIER_DELAY!=0 & ex$SECURITY_DELAY!=0),])

dim(ex[which(ex$LATE_AIRCRAFT_DELAY!=0 & ex$WEATHER_DELAY!=0 ),])
dim(ex[which(ex$LATE_AIRCRAFT_DELAY!=0 & ex$NAS_DELAY!=0 ),])
dim(ex[which(ex$LATE_AIRCRAFT_DELAY!=0 & ex$SECURITY_DELAY!=0 ),])

dim(ex[which(ex$NAS_DELAY!=0 & ex$WEATHER_DELAY!=0),])
dim(ex[which(ex$NAS_DELAY!=0 & ex$SECURITY_DELAY!=0),])

dim(ex[which(ex$SECURITY_DELAY!=0 & ex$WEATHER_DELAY!=0),])

## Group of 3

dim(ex[which(ex$CARRIER_DELAY!=0 & ex$LATE_AIRCRAFT_DELAY!=0 & ex$NAS_DELAY!=0),])
dim(ex[which(ex$CARRIER_DELAY!=0 & ex$LATE_AIRCRAFT_DELAY!=0 & ex$WEATHER_DELAY!=0),])
dim(ex[which(ex$CARRIER_DELAY!=0 & ex$LATE_AIRCRAFT_DELAY!=0 & ex$SECURITY_DELAY!=0),])


dim(ex[which( ex$LATE_AIRCRAFT_DELAY!=0 & ex$NAS_DELAY!=0 & ex$SECURITY_DELAY!=0),])
dim(ex[which( ex$LATE_AIRCRAFT_DELAY!=0 & ex$NAS_DELAY!=0 & ex$WEATHER_DELAY!=0),])

dim(ex[which( ex$SECURITY_DELAY!=0 & ex$NAS_DELAY!=0 & ex$WEATHER_DELAY!=0),])


## Group of 4
dim(ex[which(ex$CARRIER_DELAY!=0 & ex$LATE_AIRCRAFT_DELAY!=0 & ex$NAS_DELAY!=0 & ex$WEATHER_DELAY!=0),])
dim(ex[which(ex$CARRIER_DELAY!=0 & ex$LATE_AIRCRAFT_DELAY!=0 & ex$NAS_DELAY!=0 & ex$SECURITY_DELAY!=0),])

## Group of 5
dim(ex[which(ex$CARRIER_DELAY!=0 & ex$LATE_AIRCRAFT_DELAY!=0 & ex$NAS_DELAY!=0 & ex$SECURITY_DELAY!=0 &ex$WEATHER_DELAY!=0),])
#####################

ex$CRS_DEP_TIME <- substr(as.POSIXct(sprintf("%04.0f", ex$CRS_DEP_TIME), format='%H%M'), 12, 16)
ex$ARR_TIME <- substr(as.POSIXct(sprintf("%04.0f", ex$ARR_TIME), format='%H%M'), 12, 16)
ex$CRS_ARR_TIME <- substr(as.POSIXct(sprintf("%04.0f", ex$CRS_ARR_TIME), format='%H%M'), 12, 16)

ex$a<-as.POSIXlt(paste0(ex$FL_DATE," ",ex$CRS_DEP_TIME,":00.000"))
ex$b<-as.POSIXlt(paste0(ex$FL_DATE," ",ex$ARR_TIME,":00.000"))
ex$d<-as.POSIXlt(paste0(ex$FL_DATE," ",ex$CRS_ARR_TIME,":00.000"))

head(as.data.frame(difftime(ex$a,ex$d,units="mins")))
ex$difference<-difftime(ex$d,ex$a,units="mins")
ex$difference<-as.numeric(ex$difference)
head(ex[,c(26,28,17,29)])

ex$c[1]<-0

for(i in seq(1:(length(ex$a)-1)))
{
  subtraction<-difftime(ex$a[i+1],ex$b[i],units="mins")
  print(difftime(ex$a[i+1],ex$b[i],units="mins"))
  ex$c[i+1]<-subtraction
}

k=0
l=0
ex$flag[1]<-"Null"
for(i in seq(1:(length(ex$c)-1)))
{
  if(ex$DAY_OF_MONTH[i]==ex$DAY_OF_MONTH[i+1] & ex$TAIL_NUM[i]==ex$TAIL_NUM[i+1])
  {
    k<-k+1
    ex$flag[i+1]<-"Yes"
  }
  else
  {
    l<-l+1
    ex$flag[i+1]<-"No"
  }
}

print(k)
print(l)

head(ex)
head(ex[,c(1,4,15,7,28,29,25,21:23)],20)

head(ex[,c(1,4,29,28,16,25)],20)
head(ex[which(ex$c>-50 & ex<50),])

dim(ex[which(ex$flag=="No" & ex$LATE_AIRCRAFT_DELAY!=0),])
(9155/41781)*100

z<-ex[which(ex$c>-50 & ex<50),]
head(z[,c(1,4,15,7,28,29,25,21:23)],20)


names(ex)
train<-ex[,c(9,16,21:25,29)]
train[which(train$LATE_AIRCRAFT_DELAY!=0),7]<-1
train$LATE_AIRCRAFT_DELAY<-as.factor(train$LATE_AIRCRAFT_DELAY)
train$flag<-as.factor(train$flag)
names(train)
train<-train[,c(1,2,7,8)]
table(train$LATE_AIRCRAFT_DELAY)

library(caret)
index<-createDataPartition(train$LATE_AIRCRAFT_DELAY,p=0.75,list=FALSE)
training<-train[index,]
testing<-train[-index,]

library(devtools)
#install_github("tomasgreif/woe")
library(woe)

row.names(training) <- 1:nrow(training) 
IV<-iv.mult(training,y="LATE_AIRCRAFT_DELAY",TRUE)
var<-IV[which(IV$InformationValue>0.1),]
var1<-var[which(var$InformationValue<0.5),]
final_var<-var1$Variable
x_train<-training[final_var]
iv.plot.summary(IV)


# METHOD 1: Logistic Regression
#log_reg_mod <- train(training$LATE_AIRCRAFT_DELAY ~ ., data = training, method = "glm", family = "binomial",trControl=trainControl(method = "cv", number = 5, repeats = 5))

fit <- glm(training$LATE_AIRCRAFT_DELAY~.,data=training,family=binomial())
summary(fit)

# Predict
predicted <- predict(fit, testing)

# Confusion matrix 
confusion_matrix_reg <- confusionMatrix(predicted, training[,3])
confusion_matrix_reg

library(randomForest) 
random_forest <- randomForest(training_data[-1], training_data$ARR_DEL15, proximity = TRUE, importance = TRUE)
random_forest
random_forest_validation <- predict(random_forest, testing_data)

# Confusion matrix
confusion_matrix_rf <- confusionMatrix(random_forest_validatio, testing_data[,"ARR_DEL15"])
confusion_matrix_rf

training<-training[-1,]


23+ 16+20 + 33 +72 +81+201 + 275 + 429 + 539  +792+ 1110 +1204 +1316+ 1325 +1404

negdiff<-ex[which((ex$DEP_DELAY-ex$LATE_AIRCRAFT_DELAY)<0),]
write.csv(negdiff,file="D:/R/Airline Data/output/negdiff.csv")


ab<-train
ab<-ab[,-3]
ab$delay<-0

for(i in seq(1:length(ab$DEP_DELAY)))
{
  if(ab$flag[i]=="Yes" & ab$ARR_DELAY[i]>0 & ab$DEP_DELAY[i]>0)
  {
    ab$delay[i]<-1
  }
  else
  {
    ab$delay[i]<-0
  }
}
output<-cbind(train,ab$delay)
write.csv(output,file="D:/R/Airline Data/output/output.csv")

confusion_matrix_reg <- confusionMatrix(output$LATE_AIRCRAFT_DELAY,output$`ab$delay` )
confusion_matrix_reg

((35921+32626)*100)/(32626+35921+9155+4545)


