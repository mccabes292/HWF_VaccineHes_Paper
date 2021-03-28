
library(data.table)
library(dplyr)
library(readr)
library(stringr)

#Functions
#Date of most recent vaccine response (can change to first response if better) Also check in future pulls, parent vaccine was 
#observed if self question was observed.
findVaccineResponseDate=function(timestamp,vaccSelf){
  timestamp=timestamp[!is.na(vaccSelf)]
  return(min(timestamp))
}

#Find profession - Take most recent non missing profession
findProfession=function(timestamp,prof){
  if(all(prof=="missing")){
    return("missing")
  }
  timestamp=timestamp[prof!="missing"]
  prof=prof[prof!="missing"]
  return(prof[which.max(as.Date(timestamp))])
}


preexistingFunc=function(val){
  if(sum(c("True","TRUE")%in%val)>0   ){
    return(TRUE)
  }
  return(FALSE)
}


preexistingFunc_None=function(val){
  if(all(val==TRUE)){
    return(TRUE)
  }
  return(FALSE)
}

smokeFunc=function(smokeStatus){
  if("currently_smoke"%in%smokeStatus){
    return("currently_smoke")
  }
  if("used_to_smoke"%in%smokeStatus){
    return("used_to_smoke")
  }
  return("never_smoked")
}

findZipcode=function(zipcode){
  zipcode=zipcode[!is.na(zipcode)&zipcode!=""]
  return(zipcode[order(table(zipcode),decreasing=TRUE)[1]])
  
}
findCountry=function(zipcode,country){
  country=country[!is.na(zipcode)&zipcode!=""]
  zipcode=zipcode[!is.na(zipcode)&zipcode!=""]
  return(country[order(table(zipcode),decreasing=TRUE)[1]])
}

findVaccineTime=function(timestamp,vaccineRes,first=TRUE){
  ts=timestamp[!is.na(vaccineRes)]
  if(first==TRUE){
    return(min(ts))
  }
  return(max(ts))
}

##Load Data
dataDate="2021-02-15"
hwf=fread(paste("hwf_clean_",dataDate,".csv",sep=""))

#Subset Data to users which responded to vaccine question
answerVacc=unique(hwf$session_id[!is.na(hwf$vaccine_yourself)|!is.na(hwf$vaccine_children)])
hwf=hwf[hwf$session_id%in%answerVacc,]


######12/21/2020 Temporary fix of the stayed home variable to create the protected measure
######Current code in cleaning step does not account for new questions relating to leaving the house

leftVars=c("left_home_dining_inside",
           "left_home_dining_outside",
           "left_home_doctor",
           "left_home_exercise_inside"
           ,"left_home_family"
           ,"left_home_grocery"
           ,"left_home_personal_care"
           ,"left_home_pharmacy"
           ,"left_home_protest"
           ,"left_home_recreation_inside"
           ,"left_home_recreation_outside"
           ,"left_home_religious_services"
           ,"left_home_retail",
           "left_home_left_for_other",
           "left_home_left_for_work")

leftOther=rowSums(hwf[,..leftVars]=="True")>0

#Protective Measures
hwf$protectMeasure=ifelse(hwf$combined_stayed_home=="not_asked"|hwf$combined_stayed_home=="missing",NA,
                          ifelse(hwf$combined_stayed_home=="True",TRUE,
                          ifelse(hwf$protective_measures_face_mask=="True"|hwf$protective_measures_other_face_covering=="True",TRUE,
                          ifelse(hwf$combined_left_for_outdoorExercise=="True"&hwf$protective_measures_social_distancing=="True"&leftOther==FALSE,TRUE,FALSE))))

hwf$employmentGroup=factor(ifelse(hwf$employment_status%in%c("full_time","part_time"),"Employed",
                    ifelse(hwf$employment_status%in%c("furloughed","job_seeker"),"Furlough/Seeker",
                    ifelse(hwf$employment_status%in%c("","not_say"),"Missing",
                    ifelse(hwf$employment_status=="unemployed","Unemployed",NA)))),levels=c("Employed","Furlough/Seeker","Unemployed","Missing"))
#Tested Positive to Virus at any point and any test
hwf$testAny=(hwf$covid_19_swab_test_result=="positive")|(hwf$covid_19_test_result=="positive")|(hwf$covid_19_antibodies_test_result=="positive")
hwf$receivedTest=(hwf$covid_19_swab_test_result%in%c("positive","negative"))|(hwf$covid_19_test_result%in%c("positive","negative"))|(hwf$covid_19_antibodies_test_result%in%c("positive","negative"))


hwf%>%group_by(session_id)%>%summarize("vaccineRespDate"=findVaccineResponseDate(timestamp = timestamp,vaccSelf=vaccine_yourself),
                                       "profession"=findProfession(timestamp = timestamp,prof = profession_essential_cat),
                                        "everPositive"=max(testAny),
                                       "everReceivedTest"=max(receivedTest),
                                       "pregnant"=preexistingFunc(preexisting_pregnant),
                                       "liverDisease"=preexistingFunc(preexisting_liver_disease),
                                       "kidneyDisease"=preexistingFunc(preexisting_chronic_kidney_disease),
                                       "diabetes"=preexistingFunc(preexisting_diabetes),
                                       "hypertension"=preexistingFunc(preexisting_hypertension),
                                       "preexisting_NotSay"=preexistingFunc_None(preexisting_not_say),
                                       "immunodeficiency"=preexistingFunc(preexisting_immunodeficiency),
                                       "allergies"=preexistingFunc(preexisting_allergies),
                                       "cancer"=preexistingFunc(preexisting_cancer),
                                       "lungDisease"=preexistingFunc(preexisting_chronic_lung_disease),
                                       "asthma"=preexistingFunc(preexisting_asthma),
                                       "autoimmuneDisease"=preexistingFunc(preexisting_autoimmune_disease),
                                       "cardiovascularDisease"=preexistingFunc(preexisting_cardiovascular_disease),
                                       "smokeStatus"=smokeFunc(smoking_history),
                                       "percentProtect"=mean(protectMeasure,na.rm=TRUE),
                                       "numResponse"=n(),
                                       "zipcodeMostFreq"=findZipcode(zipcode),
                                       "countryMostFreq"=findCountry(zipcode,location_country),
                                       "dateFirstResp"=findVaccineTime(timestamp,vaccine_yourself,first=TRUE)
                                       )->vaccineSummary




hwfVaccine=data.table(merge(vaccineSummary,hwf,by.x=c("session_id","vaccineRespDate"),by.y=c("session_id","timestamp"),all.x=TRUE,all.y=FALSE))
#Set Not Say to FALSE if smoking status is TRUE
hwfVaccine$anySmoke=ifelse(hwfVaccine$smokeStatus%in%c("currently_smoke","used_to_smoke"),TRUE,FALSE)
hwfVaccine$preexisting_NotSay[hwfVaccine$anySmoke==TRUE]=FALSE
#Calculate Number of Preexisting Conditions
preExistVars=c("anySmoke", "pregnant","liverDisease","kidneyDisease","diabetes","hypertension","immunodeficiency","allergies","cancer","lungDisease","asthma","autoimmuneDisease","cardiovascularDisease")
hwfVaccine$numPreexist=rowSums(hwfVaccine[,..preExistVars])

hwfVaccine$numPreexist_Group=factor(ifelse(hwfVaccine$preexisting_NotSay==TRUE,"Not Say",
                                            ifelse(hwfVaccine$numPreexist==0,"0",
                                                   ifelse(hwfVaccine$numPreexist==1,"1",
                                                          ifelse(hwfVaccine$numPreexist==2,"2",
                                                                 ifelse(hwfVaccine$numPreexist>2,"3+",NA)))       )  ),levels=c("0","1","2","3+","Not Say"))

hwfVaccine$anyPreexist=factor(ifelse(hwfVaccine$preexisting_NotSay==TRUE,"Not Say",
                                      ifelse(hwfVaccine$numPreexist==0,"No",
                                             ifelse(hwfVaccine$numPreexist>0,"Yes",NA))),levels=c("No","Yes","Not Say"))
#Change age to different scale
hwfVaccine$ageGroup=factor(ifelse(is.na(as.numeric(hwfVaccine$age)),NA,ifelse(as.numeric(hwfVaccine$age)<30,"[18,30)",ifelse(as.numeric(hwfVaccine$age)<45,"[30,45)",
                                                                                                                          ifelse(as.numeric(hwfVaccine$age)<55,"[45,55)",
                                                                                                                                 ifelse(as.numeric(hwfVaccine$age)<65,"[55,65)",
                                                                                                                                        ifelse(as.numeric(hwfVaccine$age)<100,"[65,100)",NA)))  )  )),
                                    levels=c("[65,100)","[18,30)","[30,45)","[45,55)","[55,65)"))

hwfVaccine$gender=factor(hwfVaccine$gender,levels=c("male","female","other"))
hwfVaccine$profession=factor(hwfVaccine$profession,levels=c("nonessential","healthcare","other_essential","missing"))
hwfVaccine$race_cat2=factor(ifelse(hwfVaccine$race_cat%in%c("multiracial_white","other"),"multiracial/other",hwfVaccine$race_cat ),levels=c("white","african_american","hispanic_latino","asian","multiracial/other","missing"))
hwfVaccine$race_cat=factor(hwfVaccine$race_cat,levels=c("white","african_american","hispanic_latino","asian","multiracial_white","other","missing"))


hwfVaccine$hesitant_yourself=ifelse( hwfVaccine$vaccine_yourself%in%c(3,4),FALSE,TRUE)
hwfVaccine$hesitant_children=ifelse(is.na(hwfVaccine$vaccine_children),NA,ifelse(hwfVaccine$vaccine_children%in%c(3,4),FALSE,TRUE))
hwfVaccine$vaccine_yourself_3Level=factor(ifelse(hwfVaccine$vaccine_yourself%in%c(0,1),"Unlikely",
                                   ifelse(hwfVaccine$vaccine_yourself==2,"Undecided",
                                   ifelse(hwfVaccine$vaccine_yourself%in%c(3,4),"Likely",NA))),levels=c("Likely","Unlikely","Undecided"))

hwfVaccine$vaccine_children_3Level=factor(ifelse(hwfVaccine$vaccine_children%in%c(0,1),"Unlikely",
                                          ifelse(hwfVaccine$vaccine_children==2,"Undecided",
                                                 ifelse(hwfVaccine$vaccine_children%in%c(3,4),"Likely",NA))),levels=c("Likely","Unlikely","Undecided"))

hwfVaccine$undecided_yourself=ifelse(hwfVaccine$vaccine_yourself==2,TRUE,FALSE)
hwfVaccine$unlikely_yourself=ifelse(hwfVaccine$vaccine_yourself_3Level=="Unlikely",TRUE,FALSE) 
hwfVaccine$likely_yourself=ifelse(hwfVaccine$vaccine_yourself_3Level=="Likely",TRUE,FALSE)


hwfVaccine$undecided_children=ifelse(is.na(hwfVaccine$vaccine_children),NA,ifelse(hwfVaccine$vaccine_children==2,TRUE,FALSE))
hwfVaccine$unlikely_children=ifelse(is.na(hwfVaccine$vaccine_children),NA,ifelse(hwfVaccine$vaccine_children_3Level=="Unlikely",TRUE,FALSE))
hwfVaccine$likely_children=ifelse(is.na(hwfVaccine$vaccine_children),NA,ifelse(hwfVaccine$vaccine_children_3Level=="Likely",TRUE,FALSE))

###Read other datasets to merge with US based participants
hwfVaccineUS=hwfVaccine[hwfVaccine$location_country=="US",]

####Change ZIPCODE for one old zipcode
hwfVaccineUS$zipcode[hwfVaccineUS$session_id=="03D33E6D-A227-46C0-A6FC-3E2D85145F81"]="85003"

#Read in other datasets of interest
#zipDat=read_delim("zip_to_zcta_2019.txt",delim="\t")
zipDat=read_delim("Zip_to_zcta_crosswalk_2020.csv",delim=",")

testRate=read_delim("finalPositiveTestRate_2021_01_21.txt",delim="\t")
#incomeDat=read_delim("census_income.csv",delim=",")
incomeDat=read_delim("zcta_MedIncome_Loc.txt",delim="\t")
incomeDat$state=substr(incomeDat$zcta,1,2)
incomeDat$medIncome[incomeDat$medIncome<0]=NA

countyDat=read_delim("Yu_Group_County_level_downloaded_04_14_2020_cleaned_demo.csv",delim=",")
#countyDat$popDensity=countyDat$PopulationEstimate2018 / countyDat$LandAreainSquareMiles2010
countyPct=read_delim("fips_Covariates.txt",delim=",")
countyDat=merge(countyDat,countyPct,by="fips")
fipsDat=read_delim("fips_codes_2021_1_22.csv",delim=",")
#Reformat some FIPS Counties
fipsDat$County=ifelse(fipsDat$County=="De Kalb","DeKalb",
                      ifelse(fipsDat$County=="Prince Georges","Prince George's",
                             ifelse(fipsDat$County=="Du Page","DuPage",
                             ifelse(fipsDat$County=="La Porte","LaPorte",
                             ifelse(fipsDat$County=="La Salle","LaSalle",
                             ifelse(fipsDat$County=="St Marys"&fipsDat$state_abbr=="MD","St Mary's",
                             ifelse(fipsDat$County=="Queen Annes"&fipsDat$state_abbr=="MD","Queen Anne's",
                             
                                    fipsDat$County)))))))
fipsDat$countyState=paste(fipsDat$County,"-",fipsDat$state_abbr,sep="")
caseRate=read_delim("fullCaseRate_2021-01-10.txt",delim="\t")

fipsDatFull=merge(fipsDat,caseRate,by="FIPS",all.x=TRUE,all.y=TRUE)

findMissingPopDensity=function(fips){
  stateNum=substr(fips,1,2)
  t1=countyDat[which(substr(countyDat$fips,1,2)==stateNum),]
  return(t1$PopDensity[which.min(abs(as.numeric(t1$fips)-as.numeric(fips)))])
}

findMissingHighSchool=function(fips){
  stateNum=substr(fips,1,2)
  t1=countyDat[which(substr(countyDat$fips,1,2)==stateNum),]
  return(t1$noHighSchool[which.min(abs(as.numeric(t1$fips)-as.numeric(fips)))])
}

#Define county state variable and merge all datasets
hwfVaccineUS$countyState=paste(hwfVaccineUS$location_admin_level_2,"-",hwfVaccineUS$location_admin_level_1,sep="")
hwfVaccineUS=merge(hwfVaccineUS,zipDat,by.x=c('zipcode'),by.y=c("ZIP_CODE"),all.x=TRUE,all.y=FALSE)
hwfVaccineUS=merge(hwfVaccineUS,incomeDat,by.x="ZCTA",by.y="zcta",all.x=TRUE,all.y=FALSE)
hwfVaccineUS=merge(hwfVaccineUS,fipsDatFull,by="countyState",all.x=TRUE,all.y=FALSE)
hwfVaccineUS=merge(hwfVaccineUS,countyDat,by.x="FIPS",by.y="fips",all.x=TRUE,all.y=FALSE)
hwfVaccineUS=merge(hwfVaccineUS,testRate,by.x='location_admin_level_1',by.y="state",all.x=TRUE,all.y=FALSE)


pacificStates=c("AK","WA","OR","CA","HI")
mountainStates=c("MT","ID","WY","NV","UT","CO","AZ","NM")
westNCStates=c("ND","SD","NE","KS","MN","IA","MO")
eastNCStates=c("WI","MI","IL","IN","OH")
westSCStates=c("TX","OK","AR","LA")
eastSCStates=c("MS","AL","TN","KY")
southAtlStates=c("FL","GA","SC","NC","VA","WV","DE","MD","DC")
middAtlStates=c("NJ","PA","NY")
newEngStates=c("CT","RI","MA","NH","VT","ME")

hwfVaccineUS$censusLocSmall=factor(ifelse(hwfVaccineUS$location_admin_level_1%in%pacificStates,"Pacific",
                            ifelse(hwfVaccineUS$location_admin_level_1%in%mountainStates,"Mountain",
                            ifelse(hwfVaccineUS$location_admin_level_1%in%westNCStates,"West North Central",
                            ifelse(hwfVaccineUS$location_admin_level_1%in%eastNCStates,"East North Central",
                            ifelse(hwfVaccineUS$location_admin_level_1%in%westSCStates,"West South Central",
                            ifelse(hwfVaccineUS$location_admin_level_1%in%eastSCStates,"East South Central",
                            ifelse(hwfVaccineUS$location_admin_level_1%in%southAtlStates,"South Atlantic",
                            ifelse(hwfVaccineUS$location_admin_level_1%in%middAtlStates,"Middle Atlantic",
                            ifelse(hwfVaccineUS$location_admin_level_1%in%newEngStates,"New England",NA))))))))),
                            levels=c("New England","Pacific","Mountain","West North Central","East North Central",
                                     "West South Central","East South Central","South Atlantic","Middle Atlantic"
                                     ))


westStates=c("AK","OR","CA","NV","MT","ID","WY","CO","UT","AZ","NM","HI","WA")
midwestStates=c("ND","SD","NE","KS","MN","IA","MO","IL","WI","MI","IN","OH")
southStates=c("TX","OK","AR","LA","MS","TN","KY","AL","GA","FL","SC","NC","VA","WV","DC","MD","DE")
neStates=c("PA","NJ","NY","CT","RI","MA","VT","NH","ME")
hwfVaccineUS$censusLoc=factor(ifelse(hwfVaccineUS$location_country=="US",ifelse(hwfVaccineUS$location_admin_level_1%in%westStates,"West",
                                    ifelse(hwfVaccineUS$location_admin_level_1%in%midwestStates,"Midwest",
                                           ifelse(hwfVaccineUS$location_admin_level_1%in%southStates,"South",
                                                  ifelse(hwfVaccineUS$location_admin_level_1%in%neStates,"Northeast",NA)))),NA),
                             levels=c("Northeast","West","Midwest","South"))


#Set Alaska which has missing population density to smallest region (validated by searching online)
hwfVaccineUS$PopDensity[which(is.na(hwfVaccineUS$PopDensity)&hwfVaccineUS$location_admin_level_1=="AK"  )]=10

hwfVaccineUS$PopDensity[is.na(hwfVaccineUS$PopDensity)]=sapply(hwfVaccineUS$FIPS[is.na(hwfVaccineUS$PopDensity)],findMissingPopDensity )

#Calc Pop Density
hwfVaccineUS$popDensity=factor(ifelse(hwfVaccineUS$PopDensity<150,"0-149",
                                         ifelse(hwfVaccineUS$PopDensity<1000,"150-999",
                                         ifelse(hwfVaccineUS$PopDensity>=1000,"1000+",NA))),levels=c("1000+","0-149","150-999") )

hwfVaccineUS$noHighSchool[is.na(hwfVaccineUS$noHighSchool)]=sapply(hwfVaccineUS$FIPS[is.na(hwfVaccineUS$noHighSchool)],findMissingHighSchool)

#Finding Median income for closests non missing value (can return itself if non-missing)
findIncome=function(zcta){
  tempIncome=incomeDat[incomeDat$state==substr(zcta,1,2)&!is.na(incomeDat$medIncome),]
  zctaLoc=unlist(incomeDat[incomeDat$zcta==zcta,3:4])
  dist=sqrt(colSums((t(data.matrix(tempIncome[,3:4]))-zctaLoc )^2))
  return(tempIncome$medIncome[which.min(dist)])
  
}

findPctBach=function(zcta){
  tempIncome=incomeDat[incomeDat$state==substr(zcta,1,2)&!is.na(incomeDat$medIncome),]
  zctaLoc=unlist(incomeDat[incomeDat$zcta==zcta,3:4])
  dist=sqrt(colSums((t(data.matrix(tempIncome[,3:4]))-zctaLoc )^2))
  return(tempIncome$pctBachelor[which.min(dist)])
}

hwfVaccineUS$medIncome[is.na(hwfVaccineUS$medIncome)]=sapply(hwfVaccineUS$ZCTA[is.na(hwfVaccineUS$medIncome)],findIncome)
hwfVaccineUS$pctBachelor[is.na(hwfVaccineUS$pctBachelor)]=sapply(hwfVaccineUS$ZCTA[is.na(hwfVaccineUS$pctBachelor)],findPctBach)


hwfVaccineUS$income=factor(ifelse(is.na(hwfVaccineUS$medIncome),NA,
                                     ifelse(hwfVaccineUS$medIncome<40000,"0-40k",
                                            ifelse(hwfVaccineUS$medIncome<70000,"40-70k",
                                                   ifelse(hwfVaccineUS$medIncome<100000,"70-100k",
                                                          ifelse(hwfVaccineUS$medIncome>=100000,"100K+",NA))))),levels=c("100K+","0-40k","40-70k","70-100k"))


hwfVaccineUS$beforeEUA=hwfVaccineUS$timestamp<as.Date("2020-12-11")