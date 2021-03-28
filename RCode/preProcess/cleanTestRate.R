library(data.table)
library(stringr)
library(magrittr)
library(dplyr)
caseRate=fread("positiveTestRate_2021_01_20.csv",sep=",")
caseRate$posCaseRate=caseRate$positive/caseRate$totalTestResults
#caseRate$posCaseRate[is.na(caseRate$posCaseRate)]=0
keepCols=c("date","state","posCaseRate")
caseRate=caseRate[,..keepCols]
caseRate$month=substr(caseRate$date,5,6)
caseRate$year=substr(caseRate$date,1,4)
caseRate$day=substr(caseRate$date,7,8)
caseRateKeep=caseRate[(caseRate$month%in%c("11","12"))|(caseRate$month=="01"&caseRate$year=="2021"),]
caseRateKeep%>%group_by(state,month)%>%summarize("avgCaseRate"=mean(posCaseRate))->caseRateState

caseRateUnmelt=dcast(caseRateState,state~month)
colnames(caseRateUnmelt)=c("state","posTestRate_January","posTestRate_November","posTestRate_December")

fwrite(caseRateUnmelt,"finalPositiveTestRate_2021_01_21.txt",sep="\t")
