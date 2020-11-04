# script to pull test data and build bio agreement functions

library(tidyverse)
library(pool)
library(config)
library(pins)
library(lubridate)



## For testing: connect to ODS production using local credentials
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "SQL Server Native Client 11.0", 
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)

# For server deployment: Set up pool connection to production environment
#conn <- config::get("connectionSettings")

#pool <- dbPool(
#  drv = odbc::odbc(),
#  Driver = "SQLServer",   # note the LACK OF space between SQL and Server ( how RStudio named driver)
#  # Production Environment
#  Server= "DEQ-SQLODS-PROD,50000",
#  dbname = "ODS",
#  UID = conn$UID_prod, 
#  PWD = conn$PWD_prod,
#  #UID = Sys.getenv("userid_production"), # need to change in Connect {vars}
#  #PWD = Sys.getenv("pwd_production")   # need to change in Connect {vars}
#  # Test environment
#  #Server= "WSQ04151,50000",
#  #dbname = "ODS_test",
#  #UID = Sys.getenv("userid"),  # need to change in Connect {vars}
#  #PWD = Sys.getenv("pwd"),  # need to change in Connect {vars}
#  trusted_connection = "yes"
#)

# for working ID's back up taxonomic order
masterTaxaGenus <- pin_get("ejones/masterTaxaGenus", board = "rsconnect") %>%
  mutate(PTCscore = ifelse(FinalID == Genus | FinalID == Species, 1, 0)) # helper to flag when taxonomist ID's sample to desired level

#vmast <- masterTaxaGenus %>%
#  # get Family level tolerance value, FFG
#  rename('GenusTolVal' = 'TolVal',
#         'TolVal' = 'FamTolVal',
#         'GenusFFG' = 'FFG',
#         'FFG' = 'FamFFG',
#         'GenusHabit' = 'Habit',
#         'Habit' = 'FamHabit') %>%
#  mutate(e=ifelse(Order=="Ephemeroptera", 1, 0),
#         p=ifelse(Order=="Plecoptera",1,0),
#         t=ifelse(Order=="Trichoptera", 1, 0),
#         tmin=ifelse((Order=="Trichoptera" & Family != "Hydropsychidae") | 
#                       (Order=="Trichoptera" & is.na(Family)) , 1, 0), 
#         ept=ifelse(e+p+t>0,1,0), 
#         scraper = ifelse(FFG=="Scraper", 1, 0),
#         chiro = ifelse(Family=="Chironomidae",1, 0),
#         ptmin = ifelse(p + tmin > 0,1,0),
#         `clinger-HS` = ifelse(Habit == 'Clinger' & ! Family %in% c("Hydropsychidae","Simuliidae"), 1, 0)) %>%
#  # Then put it in long format so it can be merged to and input taxa list
#  select(`Final VA Family ID`,TolVal, e,p,t, ept,ptmin, scraper, chiro,`clinger-HS`) %>% 
#  distinct(`Final VA Family ID`, .keep_all = T) %>% # drop multiple rows bc working back to family level data from genus
#  filter(!is.na(`Final VA Family ID`)) %>%
#  pivot_longer(-`Final VA Family ID`, names_to = 'metric', values_to = 'metric_val') %>%
#  #  pivot_longer(-`Final VA Family ID`, names_to = 'metric', values_to = 'metric_val') %>%
#  filter(!is.na(metric_val))



## Actual work
# pull some test data to match billy's spreadsheet
#benthics <- pin_get("ejones/benthics", board = "rsconnect")
#benSamps <- pin_get("ejones/benSamps", board = "rsconnect")

station <- "2AXEM000.35"

test <- pool %>% tbl("Edas_Benthic_Sample_View") %>%
  filter(STA_ID %in% !! toupper(station)) %>%
  as_tibble() %>%
  # fix names
  rename( "StationID" = "STA_ID",
          "BenSampID"  = "WBS_SAMP_ID",
          "RepNum" = "WBS_REP_NUM",
          "Sample Comments" = "WBS_COMMENT",
          "Entered By" = "WBS_INSERTED_BY", # not in EDAS table but good info
          "Collected By" = "COLLECTOR_NAME",  # not in EDAS table but good info
          "Entered Date" = "WBS_INSERTED_DATE",
          "Gradient" = "WBCM_DESCRIPTION",
          "Taxonomist" = "TAXONOMIST_NAME",  # not in EDAS table but good info
          "Target Count" = "WBS_TARGET_COUNT",
          "Field Team" = "WBS_FIELD_TEAM",
          "Collection Date" = "FDT_DATE_TIME") %>%
  # Add sample season 
  mutate(monthday = as.numeric(paste0(sprintf("%02d",month(`Collection Date`)),
                                      sprintf("%02d",day(`Collection Date`)))),
         Season = case_when(monthday >= 0215 & monthday <= 0615 ~ 'Spring',
                            monthday >= 0815 & monthday <= 1215 ~ 'Fall',
                            TRUE ~ as.character("Outside Sample Window"))) %>%
  dplyr::select(StationID, BenSampID, RepNum, `Collection Date`, `Sample Comments`, `Collected By`, `Field Team`, `Entered By`,
                Taxonomist, `Entered Date`, Gradient, `Target Count`, Season)

testBenthics <- pool %>% tbl("Edas_Benthic_View") %>%
  filter(STA_ID %in% !! toupper(station)) %>%
  as_tibble() %>%
  rename( "StationID" = "STA_ID",
          "BenSampID"  = "WBS_SAMP_ID",
          "RepNum" = "WBS_REP_NUM",
          "FinalID" = "WBMT_FINAL_ID",
          "Individuals" = "WBE_INDIVIDUALS",
          "ID Comments" = "WBE_COMMENT",
          "Entered By" = "WBE_INSERTED_BY", # not in EDAS table but good info
          "Taxonomist" = "TAXONOMIST_NAME",  # not in EDAS table but good info
          "Entered Date" = "WBE_INSERTED_DATE") %>%
  mutate(`Excluded Taxa` = ifelse(WBE_EXCLUDED_TAXA_YN == "Y", -1, 0)) %>%
  dplyr::select(StationID, BenSampID, RepNum, FinalID, Individuals, `Excluded Taxa`, `ID Comments`, Taxonomist, `Entered By`, `Entered Date`) %>%
  filter(BenSampID %in% c('XEM9247', 'QAQC19100', 'QAQCEPA19101'))
write.csv(testBenthics, 'testBenthicPull.csv', row.names = F)



## QAQC process
# 1. Pull data based on Rick's input. This will be original StationID, and Date. This should produce a 
#  dataset with 3-4 samples (1 original sample, 1 rarified sample, 1 QA sample, and maybe 1 EPA QA sample)
rickInput <- tibble(StationID = '2AXEM000.35',
                    `Collection Date` = "2016-10-20 11:00:00")

benSamps <- pool %>% tbl("Edas_Benthic_Sample_View") %>%
  filter(STA_ID %in% !! toupper(rickInput$StationID) &
           FDT_DATE_TIME %in% !! rickInput$`Collection Date`) %>%
  as_tibble() %>%
  # fix names
  rename( "StationID" = "STA_ID",
          "BenSampID"  = "WBS_SAMP_ID",
          "RepNum" = "WBS_REP_NUM",
          "Sample Comments" = "WBS_COMMENT",
          "Entered By" = "WBS_INSERTED_BY", # not in EDAS table but good info
          "Collected By" = "COLLECTOR_NAME",  # not in EDAS table but good info
          "Entered Date" = "WBS_INSERTED_DATE",
          "Gradient" = "WBCM_DESCRIPTION",
          "Taxonomist" = "TAXONOMIST_NAME",  # not in EDAS table but good info
          "Target Count" = "WBS_TARGET_COUNT",
          "Field Team" = "WBS_FIELD_TEAM",
          "Collection Date" = "FDT_DATE_TIME") %>%
  # Add sample season 
  mutate(monthday = as.numeric(paste0(sprintf("%02d",month(`Collection Date`)),
                                      sprintf("%02d",day(`Collection Date`)))),
         Season = case_when(monthday >= 0215 & monthday <= 0615 ~ 'Spring',
                            monthday >= 0815 & monthday <= 1215 ~ 'Fall',
                            TRUE ~ as.character("Outside Sample Window"))) %>%
  dplyr::select(StationID, BenSampID, RepNum, `Collection Date`, `Sample Comments`, `Collected By`, `Field Team`, `Entered By`,
                Taxonomist, `Entered Date`, Gradient, `Target Count`, Season) %>%
  # Drop rarified sample
  filter(! str_detect(BenSampID, "R110" )) 

# Now need to get actual taxa associated with samples
benthics <- pool %>% tbl("Edas_Benthic_View") %>%
  filter(WBS_SAMP_ID %in% !! toupper(benSamps$BenSampID)) %>%
  as_tibble() %>%
  rename( "StationID" = "STA_ID",
          "BenSampID"  = "WBS_SAMP_ID",
          "RepNum" = "WBS_REP_NUM",
          "FinalID" = "WBMT_FINAL_ID",
          "Individuals" = "WBE_INDIVIDUALS",
          "ID Comments" = "WBE_COMMENT",
          "Entered By" = "WBE_INSERTED_BY", # not in EDAS table but good info
          "Taxonomist" = "TAXONOMIST_NAME",  # not in EDAS table but good info
          "Entered Date" = "WBE_INSERTED_DATE") %>%
  mutate(`Excluded Taxa` = ifelse(WBE_EXCLUDED_TAXA_YN == "Y", -1, 0)) %>%
  dplyr::select(StationID, BenSampID, RepNum, FinalID, Individuals, 
                `Excluded Taxa`, `ID Comments`, Taxonomist, `Entered By`, `Entered Date`) %>%
  # add in benSamp info to only return one dataset
  left_join(benSamps, by = c("StationID", "BenSampID", "RepNum", "Taxonomist")) %>% # enter date and enter by caused join issues so drop
  dplyr::select(-starts_with('Entered'))


# now convert to a function
pullQAQCsample <- function(poolName, rickInputDF){
  benSamps <- poolName %>% tbl("Edas_Benthic_Sample_View") %>%
    filter(STA_ID %in% !! toupper(rickInputDF$StationID) &
             FDT_DATE_TIME %in% !! rickInputDF$`Collection Date`) %>%
    as_tibble() %>%
    # fix names
    rename( "StationID" = "STA_ID",
            "BenSampID"  = "WBS_SAMP_ID",
            "RepNum" = "WBS_REP_NUM",
            "Sample Comments" = "WBS_COMMENT",
            "Entered By" = "WBS_INSERTED_BY", # not in EDAS table but good info
            "Collected By" = "COLLECTOR_NAME",  # not in EDAS table but good info
            "Entered Date" = "WBS_INSERTED_DATE",
            "Gradient" = "WBCM_DESCRIPTION",
            "Taxonomist" = "TAXONOMIST_NAME",  # not in EDAS table but good info
            "Target Count" = "WBS_TARGET_COUNT",
            "Field Team" = "WBS_FIELD_TEAM",
            "Collection Date" = "FDT_DATE_TIME") %>%
    # Add sample season 
    mutate(monthday = as.numeric(paste0(sprintf("%02d",month(`Collection Date`)),
                                        sprintf("%02d",day(`Collection Date`)))),
           Season = case_when(monthday >= 0215 & monthday <= 0615 ~ 'Spring',
                              monthday >= 0815 & monthday <= 1215 ~ 'Fall',
                              TRUE ~ as.character("Outside Sample Window"))) %>%
    dplyr::select(StationID, BenSampID, RepNum, `Collection Date`, `Sample Comments`, `Collected By`, `Field Team`, `Entered By`,
                  Taxonomist, `Entered Date`, Gradient, `Target Count`, Season) %>%
    # Drop rarified sample
    filter(! str_detect(BenSampID, "R110" )) 
  
  if(nrow(benSamps) > 0){
    return(
      poolName %>% tbl("Edas_Benthic_View") %>%
      filter(WBS_SAMP_ID %in% !! toupper(benSamps$BenSampID)) %>%
      as_tibble() %>%
      rename( "StationID" = "STA_ID",
              "BenSampID"  = "WBS_SAMP_ID",
              "RepNum" = "WBS_REP_NUM",
              "FinalID" = "WBMT_FINAL_ID",
              "Individuals" = "WBE_INDIVIDUALS",
              "ID Comments" = "WBE_COMMENT",
              "Entered By" = "WBE_INSERTED_BY", # not in EDAS table but good info
              "Taxonomist" = "TAXONOMIST_NAME",  # not in EDAS table but good info
              "Entered Date" = "WBE_INSERTED_DATE") %>%
      mutate(`Excluded Taxa` = ifelse(WBE_EXCLUDED_TAXA_YN == "Y", -1, 0)) %>%
      dplyr::select(StationID, BenSampID, RepNum, FinalID, Individuals, 
                    `Excluded Taxa`, `ID Comments`, Taxonomist, `Entered By`, `Entered Date`) %>%
      # add in benSamp info to only return one dataset
      left_join(benSamps, by = c("StationID", "BenSampID", "RepNum", "Taxonomist")) %>% # enter date and enter by caused join issues so drop
      dplyr::select(-starts_with('Entered')) %>%
        arrange(StationID, BenSampID))
  } else {
    return(
      tibble(StationID = as.character(NA), BenSampID = as.character(NA), RepNum = as.numeric(NA),
             FinalID = as.character(NA), Individuals = as.numeric(NA), `Excluded Taxa` = as.numeric(NA),
             `ID Comments` = as.character(NA), Taxonomist = as.character(NA), `Collection Date` = as.POSIXct(NA), 
             `Sample Comments` = as.character(NA),`Collected By` = as.character(NA), `Field Team` = as.character(NA), 
             Gradient = as.character(NA), `Target Count` = as.numeric(NA),  Season = as.character(NA)) )
  }
}

# Function Testing
x <- pullQAQCsample(pool, rickInput)
# test function works for multiple stations
x <- pullQAQCsample(pool, tibble(StationID = c('2AXEM000.35', '2-TLR000.03'),
                                 `Collection Date` = c("2016-10-20 11:00:00",
                                                       "2016-05-04 09:30:00")))


# 2. Line up sample, QA sample, and EPA QA samples (if available) by Order, Family, FinalID. Need to 
#  join to master taxa list to get here
x1 <- left_join(x, dplyr::select(masterTaxaGenus, Order, Family, FinalID, PTCscore )) %>%
  dplyr::select(Order, Family, FinalID, PTCscore, BenSampID, Individuals) %>%
  pivot_wider(names_from = BenSampID, values_from = Individuals) %>%
  replace(is.na(.), 0) %>% # change any NAs to 0
  mutate_if(is.character, list(~na_if(., 0))) %>% # but that messes up any NAs in Order/family so change those back
  arrange(Order, Family, FinalID)

# rename to make string searches easier
x1 <- rename(x1, 'EPAQAQC19101' = 'QAQCEPA19101')


# need to build functions that takes QA field and other field (real sample) and runs analysis on n agreements/disagreements
x2 <- dplyr::select(x1, -starts_with('QAQC'))
x3 <- dplyr::select(x1, -starts_with('EPAQAQC'))

# keep sample names for later
QAsample1 <- dplyr::select(x1, starts_with('QAQC')) %>% names()
EPAQAsample <- dplyr::select(x1, starts_with('EPAQAQC')) %>% names() # will only populate if exists
Sample <- dplyr::select(x1, -c(Order, Family, FinalID, PTCscore, starts_with('QAQC'), starts_with('EPAQAQC'))) %>% names()

# reformat data to match expected format
x33 <- x3 %>% 
  dplyr::select(Order, Family, FinalID, PTCscore, QAsample = starts_with('QAQC'), everything()) %>% # reorganize columns and rename for consistency
  rename( Sample =  !!names(.[6])) %>%
  filter(QAsample > 0 | Sample > 0) %>%
  rowwise() %>%
  mutate(Difference = abs(QAsample - Sample),
         Agreement = min(QAsample, Sample),
         PTC_QA = QAsample * PTCscore,
         PTC_O = Sample * PTCscore) %>%
  ungroup()

x33sum <-  summarise(x33, 
                     PTD = (1 - (sum(Agreement) / sum(QAsample))) * 100, # percent taxonomic disagreement
                     PTA = 100 - PTD, # percent taxonomic agreement
                     PDE = ((sum(QAsample) - sum(Sample)) / (sum(QAsample) + sum(Sample))) *100 , # percent difference in enumeration
                     PTC_QA = (sum(PTC_QA, na.rm = T) / sum(QAsample)) * 100, #(sum((QAsample * PTCscore)/QAsample))),
                     PTC_O = (sum(PTC_O, na.rm = T) / sum(Sample)) * 100,
                     PTCabs = abs(PTC_QA - PTC_O)) # absolute difference in PTC between QA sample and Sample sample
# what we want to return to user
x33 <- rename(x33, !!QAsample := QAsample,
              !!Sample := Sample)

x33sum <- mutate(x33sum, QAsample = !!QAsample,
                 Sample = !!Sample) %>%
  dplyr::select(QAsample, Sample, )


# Build method into a function
organizeTaxaLists <- function(singleQAdataset, # data from one StationID
                              masterTaxaGenus # with PTC addition
                              ){
  left_join(singleQAdataset, dplyr::select(masterTaxaGenus, Order, Family, FinalID, PTCscore )) %>%
    dplyr::select(Order, Family, FinalID, PTCscore, BenSampID, Individuals) %>%
    pivot_wider(names_from = BenSampID, values_from = Individuals) %>%
    replace(is.na(.), 0) %>% # change any NAs to 0
    mutate_if(is.character, list(~na_if(., 0))) %>% # but that messes up any NAs in Order/family so change those back
    arrange(Order, Family, FinalID) 
}
                                
reformatForQA <- function(x, QAtype){
  # reformat data to match expected format
  x %>% 
    dplyr::select(Order, Family, FinalID, PTCscore, QAsample = starts_with(QAtype), everything()) %>% # reorganize columns and rename for consistency
    rename( Sample =  !!names(.[6])) %>%
    filter(QAsample > 0 | Sample > 0) %>%
    rowwise() %>%
    mutate(Difference = abs(QAsample - Sample),
           Agreement = min(QAsample, Sample),
           PTC_QA = QAsample * PTCscore,
           PTC_O = Sample * PTCscore) %>%
    ungroup()
}


QAsummaryMetrics <- function(x, QAsample, Sample){
  summarise(x, 
            PTD = (1 - (sum(Agreement) / sum(QAsample))) * 100, # percent taxonomic disagreement
            PTA = 100 - PTD, # percent taxonomic agreement
            PDE = ((sum(QAsample) - sum(Sample)) / (sum(QAsample) + sum(Sample))) *100 , # percent difference in enumeration
            PTC_QA = (sum(PTC_QA, na.rm = T) / sum(QAsample)) * 100, #(sum((QAsample * PTCscore)/QAsample))),
            PTC_O = (sum(PTC_O, na.rm = T) / sum(Sample)) * 100,
            PTCabs = abs(PTC_QA - PTC_O)) %>% # absolute difference in PTC between QA sample and Sample sample
    mutate(QAsample = !!QAsample,
           Sample = !!Sample) %>%
    dplyr::select(QAsample, Sample, everything())
}


QAQCmasterFunction <- function(organizedDataset){
  # Identify if there are more than one type of QA sample per sample
  QAsampleData <- dplyr::select(organizedDataset, -starts_with('EPAQAQC'))
  EPAQAsampleData <- dplyr::select(organizedDataset, -starts_with('QAQC'))
  
  # keep sample names for later
  QAsample <- dplyr::select(organizedDataset, starts_with('QAQC')) %>% names()
  EPAQAsample <- dplyr::select(organizedDataset, starts_with('EPAQAQC')) %>% names() # will only populate if exists
  Sample <- dplyr::select(organizedDataset, -c(Order, Family, FinalID, PTCscore, starts_with('QAQC'), starts_with('EPAQAQC'))) %>% names()
  
  if(ncol(QAsampleData) == 6){ # if there is a QA and sample to analyze
    QAdata <- reformatForQA(QAsampleData, "QAQC")
    QAmetrics <- QAsummaryMetrics(QAdata, QAsample, Sample) 
    # fix QAdata names before returning to user
    QAdata <- rename(QAdata, !!QAsample := QAsample,
                     !!Sample := Sample)
    QAoutput <- list(QAdata = QAdata,  QAmetrics = QAmetrics)
  }
  
  if(ncol(EPAQAsampleData) == 6){ # if there is a EPAQA and sample to analyze
    EPAQAdata <- reformatForQA(EPAQAsampleData, "EPAQAQC")
    EPAQAmetrics <- QAsummaryMetrics(EPAQAdata, EPAQAsample, Sample) 
    # fix QAdata names before returning to user
    EPAQAdata <- rename(EPAQAdata, !!EPAQAsample := QAsample,
                        !!Sample := Sample)
    QAoutput <- list(QAdata = QAdata,  QAmetrics = QAmetrics,
                     EPAQAdata = EPAQAdata,  EPAQAmetrics = EPAQAmetrics)
  }
  return(QAoutput)
}

# necessary steps

# set up pool
masterTaxaGenus <- pin_get("ejones/masterTaxaGenus", board = "rsconnect") %>%
  mutate(PTCscore = ifelse(FinalID == Genus | FinalID == Species, 1, 0)) # helper to flag when taxonomist ID's sample to desired level

x <- pullQAQCsample(pool, tibble(StationID = c('2AXEM000.35', '2-TLR000.03'),
                                 `Collection Date` = c("2016-10-20 11:00:00",
                                                       "2016-05-04 09:30:00"))) %>%
  bind_rows(pullQAQCsample(pool, tibble(StationID = c('2-TLR000.03'),
                                        `Collection Date` = c("2016-05-04 09:30:00"))) %>%
              mutate(BenSampID = "QAQC011")) %>%
  mutate(BenSampID = case_when(BenSampID == 'QAQCEPA19101' ~ 'EPAQAQC19101',
                               TRUE ~ as.character(BenSampID)))

x1 <- organizeTaxaLists(x, masterTaxaGenus) #%>%
#  # rename to make string searches easier
 # rename('EPAQAQC19101' = 'QAQCEPA19101')

x2 <- QAQCmasterFunction(x1)
x2 <- QAQCmasterFunction(x1 %>% dplyr::select(-starts_with('EPAQAQC')))
x2 <- QAQCmasterFunction(x1 %>% dplyr::select(-starts_with('QAQC'))) # bombs out bc we assume always DEQ QA data before EPA data


# How to repeat for each sample
out <- list()
#out <- list(`DEQ QA Data` = list(),
#            `DEQ QA Results` = tibble(),
#            `EPA QA Data` = list(),
#            `EPA QA Results` = tibble())
for(i in unique(x$StationID)){
  x1 <- organizeTaxaLists(filter(x, StationID %in% i), masterTaxaGenus) 
  out[[i]] <- QAQCmasterFunction(x1)
  #output <- QAQCmasterFunction(x1)
  #out$`DEQ QA Data`[i] <- output[1]
  #out$`DEQ QA Results`[i,] <- output[2]
  #if(length(output) > 2){
  #  out$`EPA QA Data`[i] <- output[3]
  #  out$`EPA QA Results`[i,] <- output[4]
  #}
}
# QA data
z <- out %>%
  map_df(1)

# QA metrics
out %>%
  map_df(2)

# EPA QA data
out %>%
  map_df(3)

# EPA QA metrics
out %>%
  map_df(4)
