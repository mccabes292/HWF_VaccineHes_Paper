source("cleanDataNew.R")

library(ggforestplot)
library(ggplot2)
library(nnet)


hwfVaccineUS$posTestRate_November=hwfVaccineUS$posTestRate_November*100
hwfVaccineUS$fullyProtective=hwfVaccineUS$percentProtect==1
hwfVaccineUS=hwfVaccineUS[hwfVaccineUS$gender!="other"&hwfVaccineUS$race_cat!="missing"&!is.na(hwfVaccineUS$ageGroup)&!is.na(hwfVaccineUS$percentProtect),]
hwfVaccineUS$gender=droplevels(hwfVaccineUS$gender)
hwfVaccineUS$race_cat=droplevels(hwfVaccineUS$race_cat)
hwfVaccineUS$race_cat2=droplevels(hwfVaccineUS$race_cat2)

#Variable if individual chose to have the vaccine given that they responded to the question and were hesitant
hwfVaccineUS$vaccineDecision=factor(ifelse((hwfVaccineUS$hesitant_yourself==TRUE), ifelse(hwfVaccineUS$lastReceivedResponse=="yes","Took Vaccine",
                              ifelse(hwfVaccineUS$lastReceivedResponse=="not_accepted","Rejected Vaccine",NA )),NA),levels=c("Rejected Vaccine","Took Vaccine"))###Multivariable
#Variable indicating if individual was offered vaccine vs. not offered or didn't respond 
hwfVaccineUS$offeredVaccine=!is.na(hwfVaccineUS$vaccineDecision)
#Variable if individual responded to the vaccine decision question and was hesitant
hwfVaccineUS$respondDecision=!is.na(hwfVaccineUS$lastReceivedResponse)&(hwfVaccineUS$hesitant_yourself==TRUE)
#variable if individual was offered vaccine given that they responded to the vaccine decision question
hwfVaccineUS$offeredResponded=factor(ifelse((hwfVaccineUS$hesitant_yourself==TRUE),ifelse(hwfVaccineUS$lastReceivedResponse%in%c("yes","not_accepted"),"Offered Vaccine",
                              ifelse(hwfVaccineUS$lastReceivedResponse=="not_offered","Not Offered",NA)),NA),levels=c("Not Offered","Offered Vaccine") )



censusWeight=fread("censusWeights_CombineMultiRace.txt",sep='\t')

hwfVaccineUS$popGroup=paste(hwfVaccineUS$censusLocSmall,":",hwfVaccineUS$gender,":",hwfVaccineUS$ageGroup,":",hwfVaccineUS$race_cat2)
hwfVaccineUSMerge=merge(hwfVaccineUS,censusWeight,by="popGroup")

uniquePopGroup=unique(hwfVaccineUSMerge$popGroup)
calcPostStratWeights=function(popGroup,censusWeight){
  sampProp=table(popGroup)/nrow(popGroup)
  sampPropDf=data.frame("popGroup"=names(sampProp),sampProp)
  
}


####Calc Post Stratification Weights
sampProp=data.frame(table(hwfVaccineUSMerge$popGroup)/nrow(hwfVaccineUS))
colnames(sampProp)=c("popGroup","sampleProp")
hwfVaccineUSMerge=merge(hwfVaccineUSMerge,sampProp,by="popGroup")
hwfVaccineUSMerge$postStratWeights=hwfVaccineUSMerge$censusProp/(hwfVaccineUSMerge$sampleProp+1e-4)

library(survey)



####Model 1: Probability of Responding to the question - respondDecision

###Weight by response bias - offered vaccine
survDesign=svydesign(id=~1,strata=~gender+ageGroup+race_cat2+censusLocSmall, weights=~postStratWeights,data=hwfVaccineUSMerge)
survDesign2=trimWeights(survDesign,lower=0.3,upper=3)


#Model for responding to the vaccine question
weightGLMResponse=svyglm(respondDecision~gender+ageGroup+profession+employmentGroup+
                           race_cat2+numPreexist_Group+parent+
                           income+popDensity+censusLoc+stateCaseRate+stateDeathRate+posTestRate_November+fullyProtective+noHighSchool+everReceivedTest,design=survDesign2,family="binomial",data=hwfVaccineUSMerge)


confIntervalResponse=confint(weightGLMResponse)
weightDFLogitResponse=data.frame("name"=c("Female","Age [18,30)","Age [30,45)","Age [45,55)", "Age [55,65)",
                                          "Essential Healthcare","Other Essential","Missing Occupation","Furloughed/Job Seeker","Unemployed","Missing Employment", 
                                          "African American/Black",
                                          "Hispanic/Latinx","Asian","Multiracial/Other",
                                          "1 Preexisting","2 Preexisting","3+ Preexisting","Preexisting Not Say",
                                          "Parent", "Income 0-40K","Income 40-70K", "Income 70-100K","PopDensity 0-149", "PopDensity 150-999",
                                          "West","Midwest","South",
                                          "State Cumulative Case Rate","State Cumulative Death Rate","Average Daily Pos Test Rate (Nov.)","Practiced Protective Behavior","County % without HS Degree",
                                          "Received COVID Test"),
                                 "est"=weightGLMResponse$coefficients[-1],"se"=summary(weightGLMResponse)$coefficients[-1,2],
                                 "pvalue"=summary(weightGLMResponse)$coefficients[-1,4],
                                 "lowerCI"=confIntervalResponse[-1,1],
                                 "upperCI"=confIntervalResponse[-1,2],
                                 "refGroup"=c("Male","65+","65+","65+","65+","Nonessential","Nonessential","Nonessential",
                                              "Employed","Employed","Employed","White","White","White","White",
                                              "0","0","0","0","Not a Parent","100K+",
                                              "100K+","100K+","1000+","1000+","Northeast","Northeast","Northeast","","","","Did Not Practice","","Never Received Test"))

fwrite(weightDFLogitResponse,"responded_VaccineDecision_hesitantUptake.txt",sep="\t")



####Model 2: Probability of being eligible given response

##Calculate IPW Weight
###Obtain weights for IPW Analysis
hwfVaccineUSMerge$unTrimRespProb=weightGLMResponse$fitted.values


fittedValTrimResponse5=pmax(0.05,pmin(0.95,hwfVaccineUSMerge$unTrimRespProb))

hwfVaccineUSMerge$ipw_5Response=1/fittedValTrimResponse5

hwfVaccineUSResponse=hwfVaccineUSMerge[!is.na(hwfVaccineUSMerge$offeredResponded),]



#Run Analysis using IPW Weights
survDesignIPW5Response=svydesign(id=~1,strata=~gender+ageGroup+race_cat2+censusLocSmall, weights=~ipw_5Response,data=hwfVaccineUSResponse)


#Model for obtaining a vaccine weighted by response bias
weightGLMOffer=svyglm(offeredResponded~gender+ageGroup+profession+employmentGroup+
                        race_cat2+numPreexist_Group+parent+
                        income+popDensity+censusLoc+stateCaseRate+stateDeathRate+posTestRate_November+fullyProtective+noHighSchool+everReceivedTest+vaccine_yourself_3Level,design=survDesignIPW5Response,family="binomial",data=hwfVaccineUSResponse)


confIntervalWeightOffer=confint(weightGLMOffer)
weightDFLogitWeightOffer=data.frame("name"=c("Female","Age [18,30)","Age [30,45)","Age [45,55)", "Age [55,65)",
                                             "Essential Healthcare","Other Essential","Missing Occupation","Furloughed/Job Seeker","Unemployed","Missing Employment", 
                                             "African American/Black",
                                             "Hispanic/Latinx","Asian","Multiracial/Other",
                                             "1 Preexisting","2 Preexisting","3+ Preexisting","Preexisting Not Say",
                                             "Parent", "Income 0-40K","Income 40-70K", "Income 70-100K","PopDensity 0-149", "PopDensity 150-999",
                                             "West","Midwest","South",
                                             "State Cumulative Case Rate","State Cumulative Death Rate","Average Daily Pos Test Rate (Nov.)","Practiced Protective Behavior","County % without HS Degree",
                                             "Received COVID Test","Vaccine Intent - Undecided"),
                                    "est"=weightGLMOffer$coefficients[-1],"se"=summary(weightGLMOffer)$coefficients[-1,2],
                                    "pvalue"=summary(weightGLMOffer)$coefficients[-1,4],
                                    "lowerCI"=confIntervalWeightOffer[-1,1],
                                    "upperCI"=confIntervalWeightOffer[-1,2],
                                    "refGroup"=c("Male","65+","65+","65+","65+","Nonessential","Nonessential","Nonessential",
                                                 "Employed","Employed","Employed","White","White","White","White",
                                                 "0","0","0","0","Not a Parent","100K+",
                                                 "100K+","100K+","1000+","1000+","Northeast","Northeast","Northeast","","","","Did Not Practice","","Never Received Test","Vaccine Intent - Likely"))


fwrite(weightDFLogitWeightOffer,"offeredVaccine_Weight_hesitantUptake.txt",sep="\t")


####Model 3:


fittedValResponse2=weightGLMOffer$fitted.values*hwfVaccineUSResponse$unTrimRespProb


fittedVal2TrimResponse5=pmax(0.05,pmin(0.95,fittedValResponse2))

hwfVaccineUSResponse$ipw_5Offer=1/fittedVal2TrimResponse5

hwfVaccineUSOffered=hwfVaccineUSResponse[!is.na(hwfVaccineUSResponse$vaccineDecision),]




#Run Analysis using IPW Weights
survDesignIPW5Offer=svydesign(id=~1,strata=~gender+ageGroup+race_cat2+censusLocSmall, weights=~ipw_5Offer,data=hwfVaccineUSOffered)


weightGLMIPW5=svyglm(vaccineDecision~gender+ageGroup+profession+employmentGroup+
                       race_cat2+numPreexist_Group+parent+
                       income+popDensity+censusLoc+stateCaseRate+stateDeathRate+posTestRate_November+fullyProtective+noHighSchool+everReceivedTest+vaccine_yourself_3Level,design=survDesignIPW5Offer,family="binomial",data=hwfVaccineUSOffered)

confIntervalIPW5=confint(weightGLMIPW5)


weightDFLogitIPW5=data.frame("name"=c("Female","Age [18,30)","Age [30,45)","Age [45,55)", "Age [55,65)",
                                      "Essential Healthcare","Other Essential","Missing Occupation","Furloughed/Job Seeker","Unemployed","Missing Employment", 
                                      "African American/Black",
                                      "Hispanic/Latinx","Asian","Multiracial/Other",
                                      "1 Preexisting","2 Preexisting","3+ Preexisting","Preexisting Not Say",
                                      "Parent", "Income 0-40K","Income 40-70K", "Income 70-100K","PopDensity 0-149", "PopDensity 150-999",
                                      "West","Midwest","South",
                                      "State Cumulative Case Rate","State Cumulative Death Rate","Average Daily Pos Test Rate (Nov.)","Practiced Protective Behavior","County % without HS Degree",
                                      "Received COVID Test","Vaccine Intent - Undecided"),
                             "est"=weightGLMIPW5$coefficients[-1],"se"=summary(weightGLMIPW5)$coefficients[-1,2],
                             "pvalue"=summary(weightGLMIPW5)$coefficients[-1,4],
                             "lowerCI"=confIntervalIPW5[-1,1],
                             "upperCI"=confIntervalIPW5[-1,2],
                             "refGroup"=c("Male","65+","65+","65+","65+","Nonessential","Nonessential","Nonessential",
                                          "Employed","Employed","Employed","White","White","White","White",
                                          "0","0","0","0","Not a Parent","100K+",
                                          "100K+","100K+","1000+","1000+","Northeast","Northeast","Northeast","","","","Did Not Practice","","Never Received Test","Vaccine Intent - Likely"))

fwrite(weightDFLogitIPW5,"Weighted_ReceivedVaccineModel_IPW5_hesitantUptake.txt",sep="\t")





#Unweighted Analysis
glm1Large=glm(vaccineDecision~gender+ageGroup+profession+employmentGroup+
                race_cat2+numPreexist_Group+parent+
                income+popDensity+censusLoc+stateCaseRate+stateDeathRate+posTestRate_November+fullyProtective+noHighSchool+everReceivedTest+vaccine_yourself_3Level,family="binomial",data=hwfVaccineUS)
confInterval=confint(glm1Large)
lowerCI=confInterval[-1,1]
upperCI=confInterval[-1,2]


logitDFLarge=data.frame("name"=c("Female","Age [18,30)","Age [30,45)","Age [45,55)", "Age [55,65)",
                                 "Essential Healthcare","Other Essential","Missing Occupation","Furloughed/Job Seeker","Unemployed","Missing Employment", 
                                 "African American/Black",
                                 "Hispanic/Latinx","Asian","Multiracial/Other",
                                 "1 Preexisting","2 Preexisting","3+ Preexisting","Preexisting Not Say",
                                 "Parent", "Income 0-40K","Income 40-70K", "Income 70-100K","PopDensity 0-149", "PopDensity 150-999",
                                 "West","Midwest","South",
                                 "State Cumulative Case Rate","State Cumulative Death Rate","Average Daily Pos Test Rate (Nov.)","Practiced Protective Behavior","County % without HS Degree","Received COVID Test","Vaccine Intent - Undecided"),
                        "est"=glm1Large$coefficients[-1],"se"=summary(glm1Large)$coefficients[-1,2],
                        "pvalue"=summary(glm1Large)$coefficients[-1,4],
                        "lowerCI"=lowerCI,
                        "upperCI"=upperCI,
                        "refGroup"=c("Male","65+","65+","65+","65+","Nonessential","Nonessential","Nonessential",
                                     "Employed","Employed","Employed","White","White","White","White",
                                     "0","0","0","0","Not a Parent","100K+",
                                     "100K+","100K+","1000+","1000+","Northeast","Northeast","Northeast","","","","Did Not Practice","","Never Received Test","Vaccine Intent - Likely"))


fwrite(logitDFLarge,"unweighted_receivingVaccineModel_hesitantUptake.txt",sep="\t")

