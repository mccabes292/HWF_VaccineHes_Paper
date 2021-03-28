source("cleanData.R")

library(ggforestplot)
library(ggplot2)
library(nnet)



hwfVaccineUS$posTestRate_November=hwfVaccineUS$posTestRate_November*100
hwfVaccineUS$fullyProtective=hwfVaccineUS$percentProtect==1
hwfVaccineUS=hwfVaccineUS[hwfVaccineUS$gender!="other"&hwfVaccineUS$race_cat!="missing"&!is.na(hwfVaccineUS$ageGroup)&!is.na(hwfVaccineUS$percentProtect),]
hwfVaccineUS$gender=droplevels(hwfVaccineUS$gender)
hwfVaccineUS$race_cat=droplevels(hwfVaccineUS$race_cat)
hwfVaccineUS$race_cat2=droplevels(hwfVaccineUS$race_cat2)

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


survDesign=svydesign(id=~1,strata=~gender+ageGroup+race_cat2+censusLocSmall, weights=~postStratWeights,data=hwfVaccineUSMerge)
survDesignLower=trimWeights(survDesign,lower=0.1,upper=5)
weightGLMLowWeight=svyglm(hesitant_yourself~gender+ageGroup+profession+employmentGroup+
                   race_cat2+numPreexist_Group+parent+
                   income+popDensity+censusLoc+stateCaseRate+stateDeathRate+posTestRate_November+fullyProtective+noHighSchool+beforeEUA+everReceivedTest,design=survDesignLower,family="binomial",data=hwfVaccineUSMerge)

confInterval=confint(weightGLMLowWeight)
weightDFLogit=data.frame("name"=c("Female","Age [18,30)", "Age [30,45)","Age [45,55)", "Age [55,65)",
                                  "Essential Healthcare","Other Essential","Missing Occupation","Furloughed/Job Seeker","Unemployed","Missing Employment", 
                                  "African American/Black",
                                  "Hispanic/Latinx","Asian","Multiracial/Other",
                                  "1 Preexisting","2 Preexisting","3+ Preexisting","Preexisting Not Say",
                                  "Parent", "Income 0-40K","Income 40-70K","Income 70-100K","PopDensity 0-149", "PopDensity 150-999",
                                  "West","Midwest","South",
                                  "State Cumulative Case Rate","State Cumulative Death Rate","Average Daily Pos Test Rate (Nov.)","Practiced Protective Behavior","County % without HS Degree","Answered before Pfizer EUA","Received COVID Test"),
                         "est"=weightGLMLowWeight$coefficients[-1],"se"=summary(weightGLMLowWeight)$coefficients[-1,2],
                         "pvalue"=summary(weightGLMLowWeight)$coefficients[-1,4],
                         "lowerCI"=confInterval[-1,1],
                         "upperCI"=confInterval[-1,2],
                         "refGroup"=c("Male","65+","65+","65+","65+","Nonessential","Nonessential","Nonessential",
                                      "Employed","Employed","Employed","White","White","White","White",
                                      "0","0","0","0","Not a Parent","100K+",
                                      "100K+","100K+","1000+","1000+","Northeast","Northeast","Northeast","","","","Did Not Practice","","After Pfizer EUA","Never Received Test"))


fwrite(weightDFLogit,"weights_Mult_Trim1.txt",sep="\t")



#Multinomial Models

dfUndecided=hwfVaccineUSMerge[hwfVaccineUSMerge$vaccine_yourself_3Level%in%c("Likely","Undecided"),]
dfUndecided$vaccine_yourself_3Level=droplevels(dfUndecided$vaccine_yourself_3Level)


survDesign=svydesign(id=~1,strata=~gender+ageGroup+race_cat2+censusLocSmall, weights=~postStratWeights,data=dfUndecided)
survDesign2=trimWeights(survDesign,lower=0.1,upper=5)
weightGLMMult=svyglm(vaccine_yourself_3Level~gender+ageGroup+profession+employmentGroup+
                       race_cat2+numPreexist_Group+parent+
                       income+popDensity+censusLoc+stateCaseRate+stateDeathRate+posTestRate_November+fullyProtective+noHighSchool+beforeEUA+everReceivedTest,design=survDesign2,family="binomial",data=dfUndecided)

confInterval=confint(weightGLMMult)

weightGLMMultDF=data.frame("name"=c("Female","Age [18,30)", "Age [30,45)","Age [45,55)", "Age [55,65)",
                                    "Essential Healthcare","Other Essential","Missing Occupation","Furloughed/Job Seeker","Unemployed","Missing Employment", 
                                    "African American/Black",
                                    "Hispanic/Latinx","Asian","Multiracial/Other",
                                    "1 Preexisting","2 Preexisting","3+ Preexisting","Preexisting Not Say",
                                    "Parent", "Income 0-40K","Income 40-70K","Income 70-100K","PopDensity 0-149", "PopDensity 150-999",
                                    "West","Midwest","South",
                                    "State Cumulative Case Rate","State Cumulative Death Rate","Average Daily Pos Test Rate (Nov.)","Mask Wearing/Protective Measures","County % without HS Degree","Answered before Pfizer EUA","Received COVID Test"),
                           "est"=weightGLMMult$coefficients[-1],"se"=summary(weightGLMMult)$coefficients[-1,2],
                           "pvalue"=summary(weightGLMMult)$coefficients[-1,4],
                           "lowerCI"=confInterval[-1,1],
                           "upperCI"=confInterval[-1,2],
                           "refGroup"=c("Male","65+","65+","65+","65+","Nonessential","Nonessential","Nonessential",
                                        "Employed","Employed","Employed","White","White","White","White",
                                        "0","0","0","0","Not a Parent","100K+",
                                        "100K+","100K+","1000+","1000+","Northeast","Northeast","Northeast","","","","Did Not Practice","","After Pfizer EUA","Never Received Test"))



fwrite(weightGLMMultDF,"weighted_Undecided_Likely_Trim1.txt",sep="\t")




##Unlikely vs Likely

dfUnlikely=hwfVaccineUSMerge[hwfVaccineUSMerge$vaccine_yourself_3Level%in%c("Likely","Unlikely"),]
dfUnlikely$vaccine_yourself_3Level=droplevels(dfUnlikely$vaccine_yourself_3Level)


survDesign=svydesign(id=~1,strata=~gender+ageGroup+race_cat2+censusLocSmall, weights=~postStratWeights,data=dfUnlikely)
survDesign2=trimWeights(survDesign,lower=0.1,upper=5)
weightGLMMult2=svyglm(vaccine_yourself_3Level~gender+ageGroup+profession+employmentGroup+
                        race_cat2+numPreexist_Group+parent+
                        income+popDensity+censusLoc+stateCaseRate+stateDeathRate+posTestRate_November+fullyProtective+noHighSchool+beforeEUA+everReceivedTest,design=survDesign2,family="binomial",data=dfUnlikely)
confInterval=confint(weightGLMMult2)

weightGLMMultDF2=data.frame("name"=c("Female","Age [18,30)", "Age [30,45)","Age [45,55)", "Age [55,65)",
                                     "Essential Healthcare","Other Essential","Missing Occupation","Furloughed/Job Seeker","Unemployed","Missing Employment", 
                                     "African American/Black",
                                     "Hispanic/Latinx","Asian","Multiracial/Other",
                                     "1 Preexisting","2 Preexisting","3+ Preexisting","Preexisting Not Say",
                                     "Parent", "Income 0-40K","Income 40-70K","Income 70-100K","PopDensity 0-149", "PopDensity 150-999",
                                     "West","Midwest","South",
                                     "State Cumulative Case Rate","State Cumulative Death Rate","Average Daily Pos Test Rate (Nov.)","Mask Wearing/Protective Measures","County % without HS Degree","Answered before Pfizer EUA","Received COVID Test"),
                            "est"=weightGLMMult2$coefficients[-1],"se"=summary(weightGLMMult2)$coefficients[-1,2],
                            "pvalue"=summary(weightGLMMult2)$coefficients[-1,4],
                            "lowerCI"=confInterval[-1,1],
                            "upperCI"=confInterval[-1,2],
                            "refGroup"=c("Male","65+","65+","65+","65+","Nonessential","Nonessential","Nonessential",
                                         "Employed","Employed","Employed","White","White","White","White",
                                         "0","0","0","0","Not a Parent","100K+",
                                         "100K+","100K+","1000+","1000+","Northeast","Northeast","Northeast","","","","Did Not Practice","","After Pfizer EUA","Never Received Test"))

fwrite(weightGLMMultDF2,"weighted_Unlikely_Likely_Trim1.txt",sep="\t")





