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
  hwfVaccineUS$tempVar=(eval(parse(text=paste("hwfVaccineUS$",var,sep=""))))
  
  glmTemp=glm(hesitant_yourself~tempVar,family="binomial",data=hwfVaccineUS)
  univEst=c(univEst,glmTemp$coefficients[-1])
  univSE=c(univSE,summary(glmTemp)$coefficients[-1,2])
  univPval=c(univPval,summary(glmTemp)$coefficients[-1,4])
  confInterval=confint(glmTemp)
  lowerCI=c(lowerCI,confInterval[-1,1])
  upperCI=c(upperCI,confInterval[-1,2])
}

univDF=data.frame("name"=c("Female","Age [18,30)","Age [30,45)","Age [45,55)", "Age [55,65)",
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



forestUnivariateUSLarge=forestplot(df=univDF,
                                   estimate=est,
                                   se=se,
                                   pvalue=pvalue,
                                   logodds=TRUE,
                                   xlab="Odds Ratio",
                                   title="Vaccine Hesitancy (Univariate)")


fwrite(univDF,"hesitancy_Yourself_LargeCensus_Univariate.txt",sep="\t")
ggsave(plot=forestUnivariateUSLarge,file="forestPlot_Hesitancy_Yourself_LargeCensus_Univariate.png")


###Multivariable
glm1Large=glm(hesitant_yourself~gender+ageGroup+profession+employmentGroup+
                race_cat2+numPreexist_Group+parent+
                income+popDensity+censusLoc+stateCaseRate+stateDeathRate+posTestRate_November+fullyProtective+noHighSchool+beforeEUA+everReceivedTest,family="binomial",data=hwfVaccineUS)
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
                                 "State Cumulative Case Rate","State Cumulative Death Rate","Average Daily Pos Test Rate (Nov.)","Practiced Protective Behavior","County % without HS Degree","Answered before Pfizer EUA","Received COVID Test"),
                        "est"=glm1Large$coefficients[-1],"se"=summary(glm1Large)$coefficients[-1,2],
                        "pvalue"=summary(glm1Large)$coefficients[-1,4],
                        "lowerCI"=lowerCI,
                        "upperCI"=upperCI,
                        "refGroup"=c("Male","65+","65+","65+","65+","Nonessential","Nonessential","Nonessential",
                                     "Employed","Employed","Employed","White","White","White","White",
                                     "0","0","0","0","Not a Parent","100K+",
                                     "100K+","100K+","1000+","1000+","Northeast","Northeast","Northeast","","","","Did Not Practice","","After Pfizer EUA","Never Received Test"))


forestLogitUSLarge=forestplot(df=logitDFLarge,
                              estimate=est,
                              se=se,
                              pvalue=pvalue,
                              logodds=TRUE,
                              xlab="Odds Ratio",
                              title="Vaccine Hesitancy")

fwrite(logitDFLarge,"hesitancy_Yourself_LargeCensus.txt",sep="\t")
ggsave(plot=forestLogitUSLarge,file="forestPlot_Multivariable.png")





###Multinomial Model

mult2=multinom(vaccine_yourself_3Level~gender+ageGroup+profession+employmentGroup+
                 race_cat2+numPreexist_Group+parent+
                 income+popDensity+censusLoc+stateCaseRate+stateDeathRate+posTestRate_November+fullyProtective+noHighSchool+beforeEUA+everReceivedTest,data=hwfVaccineUS)
z2 <- summary(mult2)$coefficients/summary(mult2)$standard.errors

p2 <- (1 - pnorm(abs(z2), 0, 1)) * 2

unlikelyDF2=data.frame("name"=c("Female","Age [18,30)","Age [30,45)","Age [45,55)", "Age [55,65)",
                                "Essential Healthcare","Other Essential","Missing Occupation","Furloughed/Job Seeker","Unemployed","Missing Employment", 
                                "African American/Black",
                                "Hispanic/Latinx","Asian","Multiracial/Other",
                                "1 Preexisting","2 Preexisting","3+ Preexisting","Preexisting Not Say",
                                "Parent", "Income 0-40K","Income 40-70K", "Income 70-100K","PopDensity 0-149", "PopDensity 150-999",
                                "West","Midwest","South",
                                "State Cumulative Case Rate","State Cumulative Death Rate","Average Daily Pos Test Rate (Nov.)","Practiced Protective Behavior","County % without HS Degree","Answered before Pfizer EUA","Received COVID Test"),
                       "est"=summary(mult2)$coefficients[1,-1],
                       "se"=summary(mult2)$standard.errors[1,-1],
                       "pvalue"=p2[1,-1],
                       "refGroup"=c("Male","65+","65+","65+","65+","Nonessential","Nonessential","Nonessential",
                                    "Employed","Employed","Employed","White","White","White","White",
                                    "0","0","0","0","Not a Parent","100K+",
                                    "100K+","100K+","1000+","1000+","Northeast","Northeast","Northeast","","","","Did Not Practice","","After Pfizer EUA","Never Received Test"))

unlikelyDF2$lowerCI=unlikelyDF2$est+qnorm(0.025)*unlikelyDF2$se
unlikelyDF2$upperCI=unlikelyDF2$est+qnorm(0.975)*unlikelyDF2$se

undecidedDF2=data.frame("name"=c("Female","Age [18,30)","Age [30,45)","Age [45,55)", "Age [55,65)",
                                 "Essential Healthcare","Other Essential","Missing Occupation","Furloughed/Job Seeker","Unemployed","Missing Employment", 
                                 "African American/Black",
                                 "Hispanic/Latinx","Asian","Multiracial/Other",
                                 "1 Preexisting","2 Preexisting","3+ Preexisting","Preexisting Not Say",
                                 "Parent", "Income 0-40K","Income 40-70K", "Income 70-100K","PopDensity 0-149", "PopDensity 150-999",
                                 "West","Midwest","South",
                                 "State Cumulative Case Rate","State Cumulative Death Rate","Average Daily Pos Test Rate (Nov.)","Practiced Protective Behavior","County % without HS Degree","Answered before Pfizer EUA","Received COVID Test"),
                        "est"=summary(mult2)$coefficients[2,-1],
                        "se"=summary(mult2)$standard.errors[2,-1],
                        "pvalue"=p2[2,-1],
                        "refGroup"=c("Male","65+","65+","65+","65+","Nonessential","Nonessential","Nonessential",
                                     "Employed","Employed","Employed","White","White","White","White",
                                     "0","0","0","0","Not a Parent","100K+",
                                     "100K+","100K+","1000+","1000+","Northeast","Northeast","Northeast","","","","Did Not Practice","","After Pfizer EUA","Never Received Test"))


undecidedDF2$lowerCI=undecidedDF2$est+qnorm(0.025)*undecidedDF2$se
undecidedDF2$upperCI=undecidedDF2$est+qnorm(0.975)*undecidedDF2$se

forestMultiUnlikely2=forestplot(df=unlikelyDF2,
                                estimate=est,
                                se=se,
                                pvalue=pvalue,
                                logodds=TRUE,
                                xlab="Odds Ratio",
                                title="Unlikely vs. Likely Models")

fwrite(unlikelyDF2,"unlikely_Likely_Model_LargeCensus.txt",sep="\t")
ggsave(plot=forestMultiUnlikely2,file="forestPlot_Unlikely_Likely_LargeCensus.png")


forestMultiUndecided2=forestplot(df=undecidedDF2,
                                 estimate=est,
                                 se=se,
                                 pvalue=pvalue,
                                 logodds=TRUE,
                                 xlab="Odds Ratio",
                                 title="Undecided vs. Likely Models")




fwrite(undecidedDF2,"undecided_Likely_Model_LargeCensus.txt",sep="\t")
ggsave(plot=forestMultiUndecided2,file="forestPlot_Undecided_Likely_LargeCensus.png")





