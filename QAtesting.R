
library(tidyverse)
library(pool)
library(config)
library(pins)
library(lubridate)
library(openxlsx)

source('global.R')
conn <- config::get("connectionSettings")

masterTaxaGenus <- pin_get("ejones/masterTaxaGenus", board = "rsconnect") %>%
  mutate(PTCscore = ifelse(FinalID == Genus | FinalID == Species, 1, 0)) # helper to flag when taxonomist ID's sample to desired level

## For testing: connect to ODS production using local credentials
pool <- dbPool(
  drv = odbc::odbc(),
  Driver =  "ODBC Driver 11 for SQL Server",#Driver = "SQL Server Native Client 11.0",
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)


# Test real data 
realSites <- read_csv('fromRick/realData/2019 Pond QAQC_EVJ3ways.csv')#read_csv('fromRick/realData/template (19).csv')#read_csv('fromRick/realData/2019 Pond QAQC.csv')#read_csv('fromRick/realData/2020QA_EVJ.csv')# %>%
  # for testing
  #filter(StationID %in% c('2-OTC001.54', '2-PCT002.46', '8-MPN075.84','7-FRB001.94','4ALOA000.62', '3-RAP082.43',
  #                        '4AGEO006.73', '1BBKS000.57')) %>%
#  rename('RepNum' = 'Rep')

# Known issues
# 4AGEO006.73 QA sample is rep12 when original is rep1
# 1BBKS000.57  QA sample is rep1
# 1BCKS001.03 QA sample is rep1
# 2-BLD012.56 QA sample is rep1
# 2-WDS002.28 QA sample is rep1

#rickInputDF <- realSites
#rm(dateIssues); rm(timeIssues); rm(querySites); rm(benSamps); rm(missingData)


# first identify any issues before proceeding with QA
stationIssues <- queryPreCheck(pool, realSites)


# pull only data that will work
realData <- pullQAQCsample(pool, filter(realSites, ! StationID %in% stationIssues$StationID))

#rm(dataOut); rm(problemDataOut); rm(z); rm(i); rm(repToQA); rm(dataPasses); rm(problemData); rm(finalDataOut)

# double check data is correctly entered in CEDS before doing QA
test1 <- QAdataEntry(realData, realSites)

# show user good data
#View(test1$cleanData)

# show user bad data that needs fixing
#View(test1$problemData)
# and then show them what the data look like in CEDS
#View(filter(realData, StationID %in% test1$problemData$StationID))

test1$cleanData %>%
  group_by(StationID) %>%
  summarize(BenSampID) %>%
  distinct(BenSampID, .keep_all = T) %>%
  mutate(Type = case_when(str_detect(BenSampID, "^QAQC") ~ "QA Sample",
                          str_detect(BenSampID, "^EPAQAQC") ~ "EPA QA Sample",
                          TRUE~ as.character('Original Sample') )) %>%
  pivot_wider(names_from = Type, values_from = BenSampID)

# Real QA time
results <- QAQCmasterFunction_df(test1$cleanData, masterTaxaGenus)

DEQQAresults <- map(results, "QAdata")
DEQQAmetrics <- map_df(results, "QAmetrics")

DEQQAresults[['8-MPN075.84']]


EPAQAresults <- map(results, "EPAQAdata")
EPAQAmetrics <- map_df(results, "EPAQAmetrics")


DEQQAvsEPAQAresults <- map(results, "DEQQAvsEPAdata")
DEQQAvsEPAQAmetrics <- map_df(results, "DEQQAvsEPAmetrics")

EPAQAresults[['7-RSS001.40']]
filter(EPAQAmetrics, StationID %in% c('7-RSS001.40'))
