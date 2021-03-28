source("cleanData.R")

library(ggforestplot)
library(ggplot2)
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
survDesign2=trimWeights(survDesign,lower=0.3,upper=3)
weightGLM=svyglm(hesitant_yourself~gender+ageGroup+profession+employmentGroup+
                race_cat2+numPreexist_Group+parent+
                income+popDensity+censusLoc+stateCaseRate+stateDeathRate+posTestRate_November+fullyProtective+noHighSchool+beforeEUA+everReceivedTest,design=survDesign2,family="binomial",data=hwfVaccineUSMerge)

confInterval=confint(weightGLM)
weightDFLogit=data.frame("name"=c("Female","Age [18,30)", "Age [30,45)","Age [45,55)", "Age [55,65)",
                                 "Essential Healthcare","Other Essential","Missing Occupation","Furloughed/Job Seeker","Unemployed","Missing Employment", 
                                 "African American/Black",
                                 "Hispanic/Latinx","Asian","Multiracial/Other",
                                 "1 Preexisting","2 Preexisting","3+ Preexisting","Preexisting Not Say",
                                 "Parent", "Income 0-40K","Income 40-70K","Income 70-100K","PopDensity 0-149", "PopDensity 150-999",
                                 "West","Midwest","South",
                                 "State Cumulative Case Rate","State Cumulative Death Rate","Average Daily Pos Test Rate (Nov.)","Practiced Protective Behavior","County % without HS Degree","Answered before Pfizer EUA","Received COVID Test"),
                        "est"=weightGLM$coefficients[-1],"se"=summary(weightGLM)$coefficients[-1,2],
                        "pvalue"=summary(weightGLM)$coefficients[-1,4],
                        "lowerCI"=confInterval[-1,1],
                        "upperCI"=confInterval[-1,2],
                        "refGroup"=c("Male","65+","65+","65+","65+","Nonessential","Nonessential","Nonessential",
                                     "Employed","Employed","Employed","White","White","White","White",
                                     "0","0","0","0","Not a Parent","100K+",
                                     "100K+","100K+","1000+","1000+","Northeast","Northeast","Northeast","","","","Did Not Practice","","After Pfizer EUA","Never Received Test"))


forestWeight=forestplot(df=weightDFLogit,
                              estimate=est,
                              se=se,
                              pvalue=pvalue,
                              logodds=TRUE,
                              xlab="Odds Ratio",
                              title="Vaccine Hesitancy (Census Weighted)")

fwrite(weightDFLogit,"hesitancy_Yourself_LargeCensus_Weighted.txt",sep="\t")
ggsave(plot=forestWeight,file="forestPlot_Multivariable_Weighted.png")








###Weighted Univariate Models
###Univariate Models
univVar=c("gender","ageGroup","profession","employmentGroup",
          "race_cat2","numPreexist_Group","everPositive","parent",
          "income","popDensity","censusLoc", "stateCaseRate","stateDeathRate","posTestRate_November","noHighSchool", "beforeEUA","everReceivedTest")
univEst=NULL
univSE=NULL
univPval=NULL
lowerCI=NULL
upperCI=NULL
for(var in univVar){
  hwfVaccineUSMerge$tempVar=(eval(parse(text=paste("hwfVaccineUSMerge$",var,sep=""))))
  survDesignTemp=svydesign(id=~1,strata=~gender+ageGroup+race_cat2+censusLocSmall, weights=~postStratWeights,data=hwfVaccineUSMerge)
  survDesign2Temp=trimWeights(survDesignTemp,lower=0.3,upper=3)
  
  weightGLMTemp=svyglm(hesitant_yourself~tempVar,design=survDesign2Temp,family="binomial",data=hwfVaccineUSMerge)
  
  univEst=c(univEst,weightGLMTemp$coefficients[-1])
  univSE=c(univSE,summary(weightGLMTemp)$coefficients[-1,2])
  univPval=c(univPval,summary(weightGLMTemp)$coefficients[-1,4])
  confInterval=confint(weightGLMTemp)
  lowerCI=c(lowerCI,confInterval[-1,1])
  upperCI=c(upperCI,confInterval[-1,2])
}

univDFWeight=data.frame("name"=c("Female","Age [18,30)","Age [30,45)","Age [45,55)", "Age [55,65)",
                                 "Essential Healthcare","Other Essential","Missing Occupation","Furloughed/Job Seeker","Unemployed","Missing Employment", 
                                 "African American/Black",
                                 "Hispanic/Latinx","Asian","Multiracial/Other",
                                 "1 Preexisting","2 Preexisting","3+ Preexisting","Preexisting Not Say",
                                 "Parent", "Income 0-40K","Income 40-70K", "Income 70-100K","PopDensity 0-149", "PopDensity 150-999",
                                 "West","Midwest","South",
                                 "State Cumulative Case Rate","State Cumulative Death Rate","Average Daily Pos Test Rate (Nov.)","Practiced Protective Behavior","County % without HS Degree","Answered before Pfizer EUA","Received COVID Test"),
                  "est"=univEst,
                  "se"=univSE,
                  "pvalue"=univPval,
                  "lowerCI"=lowerCI,
                  "upperCI"=upperCI,
                  "refGroup"=c("Male","65+","65+","65+","65+","Nonessential","Nonessential","Nonessential",
                               "Employed","Employed","Employed","White","White","White","White",
                               "0","0","0","0","Not a Parent","100K+",
                               "100K+","100K+","1000+","1000+","Northeast","Northeast","Northeast","","","","Did Not Practice","","After Pfizer EUA","Never Received Test"))



forestUnivariateWeight=forestplot(df=univDFWeight,
                                   estimate=est,
                                   se=se,
                                   pvalue=pvalue,
                                   logodds=TRUE,
                                   xlab="Odds Ratio",
                                   title="Vaccine Hesitancy (Univariate Weighted)")

fwrite(univDFWeight,"hesitancy_Yourself_Univariate_Weighted.txt",sep="\t")
ggsave(plot=forestUnivariateWeight,file="forestPlot_Univariate_Weight.png")





###Weighted Multinomial
##Undecided vs Likely

dfUndecided=hwfVaccineUSMerge[hwfVaccineUSMerge$vaccine_yourself_3Level%in%c("Likely","Undecided"),]
dfUndecided$vaccine_yourself_3Level=droplevels(dfUndecided$vaccine_yourself_3Level)


survDesign=svydesign(id=~1,strata=~gender+ageGroup+race_cat2+censusLocSmall, weights=~postStratWeights,data=dfUndecided)
survDesign2=trimWeights(survDesign,lower=0.3,upper=3)
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


forestWeightMult=forestplot(df=weightGLMMultDF,
                        estimate=est,
                        se=se,
                        pvalue=pvalue,
                        logodds=TRUE,
                        xlab="Odds Ratio",
                        title="Undecided vs. Likely (Census Weighted)")

fwrite(weightGLMMultDF,"weighted_Undecided_Likely.txt",sep="\t")
ggsave(plot=forestWeightMult,file="forestPlot_weighted_Undecided_Likely.png")




##Unlikely vs Likely

dfUnlikely=hwfVaccineUSMerge[hwfVaccineUSMerge$vaccine_yourself_3Level%in%c("Likely","Unlikely"),]
dfUnlikely$vaccine_yourself_3Level=droplevels(dfUnlikely$vaccine_yourself_3Level)


survDesign=svydesign(id=~1,strata=~gender+ageGroup+race_cat2+censusLocSmall, weights=~postStratWeights,data=dfUnlikely)
survDesign2=trimWeights(survDesign,lower=0.3,upper=3)
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


forestWeightMult2=forestplot(df=weightGLMMultDF2,
                        estimate=est,
                        se=se,
                        pvalue=pvalue,
                        logodds=TRUE,
                        xlab="Odds Ratio",
                        title="Unlikely vs. Likely (Census Weighted)")

fwrite(weightGLMMultDF2,"weighted_Unlikely_Likely.txt",sep="\t")
ggsave(plot=forestWeightMult2,file="forestPlot_weighted_Unlikely_Likely.png")




