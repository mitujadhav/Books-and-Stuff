Apr_data<-read.csv("D:/R/Airline Data/PushkarsirAirlinedata/2015/Apr2015.csv")

Apr_data<-Apr_data[which(Apr_data$CARRIER_DELAY!=""),]
library(dplyr)
Apr_data<-Apr_data[,c(4,6,7,8,13,21,26:35,39:47)]

ex<-Apr_data %>% group_by(DAY_OF_MONTH,TAIL_NUM) %>% arrange(CRS_DEP_TIME)%>% arrange(DAY_OF_MONTH,TAIL_NUM)
ex<-as.data.frame(ex)

nas<-ex[,c(16:18,23)]


for(i in seq(1:length(nas$ARR_DELAY)))
{
  if(nas$ACTUAL_ELAPSED_TIME[i]>nas$CRS_ELAPSED_TIME[i])
  {
    nas$elapsed_time_diff[i]<-nas$ACTUAL_ELAPSED_TIME[i]-nas$CRS_ELAPSED_TIME[i]
  }
  else
  {
    nas$elapsed_time_diff[i]<-0
  }
}

for(i in seq(1:length(nas$ARR_DELAY)))
{
  if(nas$elapsed_time_diff[i]==0)
  {
    nas$nas_delay_calculated[i]<-0
  }
  else if(nas$ARR_DELAY[i]>nas$elapsed_time_diff[i])
  {
    nas$nas_delay_calculated[i]<-nas$elapsed_time_diff[i]
  }
  else if(nas$elapsed_time_diff[i]>nas$ARR_DELAY[i])
  {
    nas$nas_delay_calculated[i]<-nas$ARR_DELAY[i]
  }
}

j=0
k=0
for(i in seq(1:length(nas$ARR_DELAY)))
{
  if(nas$NAS_DELAY[i]==nas$nas_delay_calculated[i])
  {
    j<-j+1
  }
  else
  {
    k<-k+1
  }
}

# TRUE Accuracy Percentage
(j*100)/dim(ex)[1]

# False Percentage
(k*100)/dim(ex)[1]

 
# for(i in seq(1:length(nas$ARR_DELAY)))
# {
#  if(nas$ACTUAL_ELAPSED_TIME[i]>nas$CRS_ELAPSED_TIME[i])
#  {
#    diff<-nas$ACTUAL_ELAPSED_TIME[i]-nas$CRS_ELAPSED_TIME[i]
#    if(diff<=nas$ARR_DELAY)
#    {
#      nas$nas_delay_calculated[i]<-diff
#    #  nas$carrier_delay_calculated[i]<-nas$ARR_DELAY-diff
#    }
#    else if(diff>nas$ARR_DELAY)
#    {
#      nas$nas_delay_calculated[i]<-nas$ARR_DELAY[i]
#     # nas$carrier_delay_calculated[i]<-0
#    }
#  }
#  else
#  {
#    nas$nas_delay_calculated[i]<-0
#  }
# }


