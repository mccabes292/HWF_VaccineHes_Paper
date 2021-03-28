library(data.table)
library(dplyr)
#datePull="2020-12-21"
datePull="2021-01-10"
#Read Case Rate Data
caseRate=fread("covid_confirmed_usafacts.csv",sep=",")
deathRate=fread("covid_deaths_usafacts.csv",sep=",")

popDat=fread("covid_county_population_usafacts.csv",sep=",")


indices=c(1:4,ncol(caseRate))
indices2=c(1:4,ncol(deathRate))
caseRate=caseRate[,..indices]
colnames(caseRate)=c("FIPS","County","State","stateFIPS","caseNumber")
deathRate=deathRate[,..indices2]
colnames(deathRate)=c("FIPS","County","State","stateFIPS","deathNumber")

merge1=merge(caseRate,deathRate,by=c("FIPS","State","stateFIPS"),all.x=TRUE,all.y=TRUE)

fullCaseRate=merge(merge1,popDat,by.x=c("FIPS","State"),by.y=c("countyFIPS","State"),all.x=TRUE,all.y=TRUE)
keepCols=c("FIPS","State","County.x","caseNumber","deathNumber","population")
fullCaseRate=fullCaseRate[which(fullCaseRate$population!=0),..keepCols]
colnames(fullCaseRate)=c("FIPS","State","County","caseNumber","deathNumber","population")
fullCaseRate$caseRate=fullCaseRate$caseNumber/fullCaseRate$population*100
fullCaseRate$deathRate=fullCaseRate$deathNumber/fullCaseRate$population*1000


fullCaseRate$FIPS=sprintf("%05d",fullCaseRate$FIPS)
 

fullCaseRate%>%group_by(State)%>%summarize("totStateCase"=sum(caseNumber),"totStateDeath"=sum(deathNumber),"totStatePop"=sum(population))->stateCaseRate
stateCaseRate$stateCaseRate=stateCaseRate$totStateCase/stateCaseRate$totStatePop*100
stateCaseRate$stateDeathRate=stateCaseRate$totStateDeath/stateCaseRate$totStatePop*1000

#fwrite(stateCaseRate,paste("fullStateCaseRate_",datePull,".txt",sep=""),sep="\t")

fullCaseRateMerge=merge(fullCaseRate,stateCaseRate,by="State")


fwrite(fullCaseRateMerge,paste("fullCaseRate_",datePull,".txt",sep=""),sep="\t")
