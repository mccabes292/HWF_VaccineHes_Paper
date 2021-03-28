library(readr)
library(stringr)
Sys.setenv(CENSUS_KEY="5b978f0e407305b77a0595545ee6170b9b8b9ae1")
library(censusapi)
library(data.table)




cenVal=data.table(getCensus(
  vars = c("NAME","B19013_001E","B19013_001EA"),
  name="acs/acs5",
  region = "zip code tabulation area:*",
  vintage=2019))

cenVal2=data.table(getCensus(
  vars = c("NAME","B19013_001E","B19013_001EA"),
  name="acs/acs5",
  region = "zip code tabulation area:*",
  vintage=2018))

cenValFinal=cenVal[,c(2,4)]
colnames(cenValFinal)=c("zcta","medIncome")


cenState=data.table(getCensus(
  vars = c("NAME","B19013_001E"),
  name="acs/acs5",
  region = "state:*",
  vintage=2019))
colnames(cenState)=c("stateNum","state","medIncome")

write_delim(cenValFinal,"zcta_MedianIncome.txt",delim="\t")
write_delim(cenState,"state_MedianIncome.txt",delim="\t")
