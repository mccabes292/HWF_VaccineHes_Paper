library(readr)
library(stringr)
Sys.setenv(CENSUS_KEY="5b978f0e407305b77a0595545ee6170b9b8b9ae1")
library(censusapi)
library(data.table)
census=read_delim("census_acs_2018_joint_dist_retrieval_codes.txt",delim="\t")
dropRow=c("SEX BY AGE","UNWEIGHTED SAMPLE COUNT OF THE POPULATION","UNWEIGHTED SAMPLE HOUSING UNITS")
census=census[!(census$Concept%in%dropRow),]
census$sex=ifelse(str_detect(census$Label,"Male"),"male",ifelse(str_detect(census$Label,"Female"),"female","NA") )


ageList=list(
  "Under 18"=c("Under 5 years","5 to 9 years","10 to 14 years","15 to 17 years"),
  "[18,30)"=c("18 and 19 years","20 to 24 years","20 years","21 years","22 to 24 years","25 to 29 years"),
  "[30,45)"=c("30 to 34 years","35 to 39 years","35 to 44 years","40 to 44 years"),
  "[45,55)"=c("45 to 49 years","45 to 54 years","50 to 54 years"),
  "[55,65)"=c("55 to 59 years","55 to 64 years","60 and 61 years","62 to 64 years"),
  "[65,100)"=c("65 and 66 years","65 to 74 years","67 to 69 years","70 to 74 years","75 to 79 years","75 to 84 years","80 to 84 years","85 years and over")
  
)

census$ageGroup=NA
for(i in 1:length(ageList)){
  census$ageGroup=ifelse(!is.na(census$ageGroup),census$ageGroup,
                         ifelse(str_detect(census$Label,paste(ageList[[i]],collapse="|" )),names(ageList)[i],NA ))
}

census$race=NA

finalRaceList=list("white"="(WHITE ALONE, NOT HISPANIC OR LATINO)",
                   "african_american"= "(BLACK OR AFRICAN AMERICAN ALONE)",
                   "hispanic_latino"="(HISPANIC OR LATINO)",
                   "asian"="(ASIAN ALONE)",
                   "multiracial/other"=c("(TWO OR MORE RACES)","(AMERICAN INDIAN AND ALASKA NATIVE ALONE)","(NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)","(SOME OTHER RACE ALONE)"))


for(i in 1:length(finalRaceList)){
  census$race=ifelse(!is.na(census$race),census$race, ifelse(str_detect(census$Concept,paste(finalRaceList[[i]],collapse="|")),names(finalRaceList)[i],census$race ))
}

census=census[!is.na(census$race)&!is.na(census$ageGroup)&!is.na(census$sex)&census$ageGroup!="Under 18",]
census$fullName=paste(census$sex,":",census$ageGroup,":",census$race)
uniqueNames=unique(census$fullName)

cenVal=data.table(getCensus(
  vars = c("NAME",census$Name),
  name="acs/acs5",
  region = "division:*",
  vintage=2018))

cenVal$NAME=str_replace(cenVal$NAME," Division","")

fullCensus=data.table("location"=cenVal$NAME)
for(i in 1:length(uniqueNames)){
  refCol=census$Name[census$fullName==uniqueNames[i]]
  newCol=uniqueNames[i]
  rsTemp=rowSums(cenVal[,..refCol])
  fullCensus[,(newCol):= rsTemp]
  
}

getSex=function(x){
  return(str_split(x,":")[[1]][1])
}
getAge=function(x){
  return(str_split(x,":")[[1]][2])
}
getRace=function(x){
  return(str_split(x,":")[[1]][3])
}

censusMelt=melt(fullCensus,id.vars = "location")
censusMelt$sex=sapply(censusMelt$variable,getSex)
censusMelt$age=sapply(censusMelt$variable,getAge)
censusMelt$race=sapply(censusMelt$variable,getRace)
censusMelt$prop=censusMelt$value/sum(censusMelt$value)

censusMelt$popGroup=paste(censusMelt$location,":",censusMelt$variable)
censusMeltFinal=censusMelt[,c("popGroup","value","prop")]
colnames(censusMeltFinal)=c("popGroup","censusTotal","censusProp")


fwrite(censusMeltFinal,"censusWeights_CombineMultiRace.txt",sep="\t")
