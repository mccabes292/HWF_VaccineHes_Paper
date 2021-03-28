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

##Run analysis with census adjusting
survDesign=svydesign(id=~1,strata=~gender+ageGroup+race_cat2+censusLocSmall, weights=~postStratWeights,data=hwfVaccineUSMerge)

survDesign2=trimWeights(survDesign,lower=0.3,upper=3,strict=TRUE)
testPosGLM=svyglm(everReceivedTest~gender+ageGroup+profession+employmentGroup+
                   race_cat2+numPreexist_Group+parent+
                   income+popDensity+censusLoc+stateCaseRate+stateDeathRate+posTestRate_November+fullyProtective+noHighSchool+beforeEUA,design=survDesign2,family="binomial",data=hwfVaccineUSMerge)




confInterval=confint(testPosGLM)
testedDF=data.frame("name"=c("Female","Age [18,30)", "Age [30,45)","Age [45,55)", "Age [55,65)",
                                  "Essential Healthcare","Other Essential","Missing Occupation","Furloughed/Job Seeker","Unemployed","Missing Employment", 
                                  "African American/Black",
                                  "Hispanic/Latinx","Asian","Multiracial/Other",
                                  "1 Preexisting","2 Preexisting","3+ Preexisting","Preexisting Not Say",
                                  "Parent", "Income 0-40K","Income 40-70K","Income 70-100K","PopDensity 0-149", "PopDensity 150-999",
                                  "West","Midwest","South",
                                  "State Cumulative Case Rate","State Cumulative Death Rate","Average Daily Pos Test Rate (Nov.)","Practiced Protective Behavior","County % without HS Degree","Answered before Pfizer EUA"),
                         "est"=testPosGLM$coefficients[-1],"se"=summary(testPosGLM)$coefficients[-1,2],
                         "pvalue"=summary(testPosGLM)$coefficients[-1,4],
                         "lowerCI"=confInterval[-1,1],
                         "upperCI"=confInterval[-1,2],
                         "refGroup"=c("Male","65+","65+","65+","65+","Nonessential","Nonessential","Nonessential",
                                      "Employed","Employed","Employed","White","White","White","White",
                                      "0","0","0","0","Not a Parent","100K+",
                                      "100K+","100K+","1000+","1000+","Northeast","Northeast","Northeast","","","","Did Not Practice","","After Pfizer EUA"))


forestTest=forestplot(df=testedDF,
                        estimate=est,
                        se=se,
                        pvalue=pvalue,
                        logodds=TRUE,
                        xlab="Odds Ratio",
                        title="Receiving Test (Census Weighted)")

fwrite(testedDF,"receivedTest_GLM.txt",sep="\t")
ggsave(plot=forestTest,file="forestPlot_receivedTest.png")







#Obtain fitted values and calculate IPW 
fittedVal=testPosGLM$fitted.values
fittedValTrim10=pmax(0.1,pmin(0.9,fittedVal))

fittedValTrim5=pmax(0.05,pmin(0.95,fittedVal))

hwfVaccineUSMerge$ipw_5=1/fittedValTrim5
hwfVaccineUSMerge$ipw_10=1/fittedValTrim10

hwfVaccineUSTest=hwfVaccineUSMerge[hwfVaccineUSMerge$everReceivedTest==TRUE,]

survDesignIPW5=svydesign(id=~1,strata=~gender+ageGroup+race_cat2+censusLocSmall, weights=~ipw_5,data=hwfVaccineUSTest)

survDesignIPW10=svydesign(id=~1,strata=~gender+ageGroup+race_cat2+censusLocSmall, weights=~ipw_10,data=hwfVaccineUSTest)



###Multivariable IPW 5 Trim
glm1LargeIPW5=svyglm(hesitant_yourself~gender+ageGroup+profession+employmentGroup+
                race_cat2+numPreexist_Group+parent+
                income+popDensity+censusLoc+stateCaseRate+stateDeathRate+posTestRate_November+fullyProtective+noHighSchool+beforeEUA+everPositive,family="binomial",design=survDesignIPW5,data=hwfVaccineUSTest)
confInterval=confint(glm1LargeIPW5)
lowerCI=confInterval[-1,1]
upperCI=confInterval[-1,2]
logitDFIPW5=data.frame("name"=c("Female","Age [18,30)","Age [30,45)","Age [45,55)", "Age [55,65)",
                                 "Essential Healthcare","Other Essential","Missing Occupation","Furloughed/Job Seeker","Unemployed","Missing Employment", 
                                 "African American/Black",
                                 "Hispanic/Latinx","Asian","Multiracial/Other",
                                 "1 Preexisting","2 Preexisting","3+ Preexisting","Preexisting Not Say",
                                 "Parent", "Income 0-40K","Income 40-70K", "Income 70-100K","PopDensity 0-149", "PopDensity 150-999",
                                 "West","Midwest","South",
                                 "State Cumulative Case Rate","State Cumulative Death Rate","Average Daily Pos Test Rate (Nov.)","Practiced Protective Behavior","County % without HS Degree","Answered before Pfizer EUA","Tested Positive"),
                        "est"=glm1LargeIPW5$coefficients[-1],"se"=summary(glm1LargeIPW5)$coefficients[-1,2],
                        "pvalue"=summary(glm1LargeIPW5)$coefficients[-1,4],
                        "lowerCI"=lowerCI,
                        "upperCI"=upperCI,
                        "refGroup"=c("Male","65+","65+","65+","65+","Nonessential","Nonessential","Nonessential",
                                     "Employed","Employed","Employed","White","White","White","White",
                                     "0","0","0","0","Not a Parent","100K+",
                                     "100K+","100K+","1000+","1000+","Northeast","Northeast","Northeast","","","","Did Not Practice","","After Pfizer EUA","Tested Negative"))


forestLogitIPW5=forestplot(df=logitDFIPW5,
                              estimate=est,
                              se=se,
                              pvalue=pvalue,
                              logodds=TRUE,
                              xlab="Odds Ratio",
                              title="Vaccine Hesitancy (IPW Weight)")

fwrite(logitDFIPW5,"ipw5_MultivariableRes.txt",sep="\t")
ggsave(plot=forestLogitIPW5,file="sensitivity/forestPlot_Multivariable_IPW5.png")








###Multivariable IPW 10 Trim
glm1LargeIPW10=svyglm(hesitant_yourself~gender+ageGroup+profession+employmentGroup+
                       race_cat2+numPreexist_Group+parent+
                       income+popDensity+censusLoc+stateCaseRate+stateDeathRate+posTestRate_November+fullyProtective+noHighSchool+beforeEUA+everPositive,family="binomial",design=survDesignIPW10,data=hwfVaccineUSTest)
confInterval=confint(glm1LargeIPW10)
confInterval=confint(glm1LargeIPW10)
lowerCI=confInterval[-1,1]
upperCI=confInterval[-1,2]
logitDFIPW10=data.frame("name"=c("Female","Age [18,30)","Age [30,45)","Age [45,55)", "Age [55,65)",
                                "Essential Healthcare","Other Essential","Missing Occupation","Furloughed/Job Seeker","Unemployed","Missing Employment", 
                                "African American/Black",
                                "Hispanic/Latinx","Asian","Multiracial/Other",
                                "1 Preexisting","2 Preexisting","3+ Preexisting","Preexisting Not Say",
                                "Parent", "Income 0-40K","Income 40-70K", "Income 70-100K","PopDensity 0-149", "PopDensity 150-999",
                                "West","Midwest","South",
                                "State Cumulative Case Rate","State Cumulative Death Rate","Average Daily Pos Test Rate (Nov.)","Practiced Protective Behavior","County % without HS Degree","Answered before Pfizer EUA","Tested Positive"),
                       "est"=glm1LargeIPW10$coefficients[-1],"se"=summary(glm1LargeIPW10)$coefficients[-1,2],
                       "pvalue"=summary(glm1LargeIPW10)$coefficients[-1,4],
                       "lowerCI"=lowerCI,
                       "upperCI"=upperCI,
                       "refGroup"=c("Male","65+","65+","65+","65+","Nonessential","Nonessential","Nonessential",
                                    "Employed","Employed","Employed","White","White","White","White",
                                    "0","0","0","0","Not a Parent","100K+",
                                    "100K+","100K+","1000+","1000+","Northeast","Northeast","Northeast","","","","Did Not Practice","","After Pfizer EUA","Tested Negative"))


forestLogitIPW10=forestplot(df=logitDFIPW10,
                           estimate=est,
                           se=se,
                           pvalue=pvalue,
                           logodds=TRUE,
                           xlab="Odds Ratio",
                           title="Vaccine Hesitancy (IPW Weight)")

fwrite(logitDFIPW10,"ipw10_MultivariableRes.txt",sep="\t")
ggsave(plot=forestLogitIPW10,file="forestPlot_Multivariable_IPW10.png")




