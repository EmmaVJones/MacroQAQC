httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))

library(tidyverse)
library(shiny)
library(pool)
library(config)
library(pins)
library(lubridate)
library(openxlsx)
library(dbplyr)

# get configuration settings
conn <- config::get("connectionSettings")

board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))



## For testing: connect to ODS production using local credentials
# pool <- dbPool(
#  drv = odbc::odbc(),
#  Driver =  "ODBC Driver 11 for SQL Server",#Driver = "SQL Server Native Client 11.0",
#  Server= "DEQ-SQLODS-PROD,50000",
#  dbname = "ODS",
#  trusted_connection = "yes"
# )

# # Set up pool connection to production environment
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "SQLServer",   # note the LACK OF space between SQL and Server ( how RStudio named driver)
  # Production Environment
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  UID = conn$UID_prod,
  PWD = conn$PWD_prod,
  #UID = Sys.getenv("userid_production"), # need to change in Connect {vars}
  #PWD = Sys.getenv("pwd_production")   # need to change in Connect {vars}
  # Test environment
  #Server= "WSQ04151,50000",
  #dbname = "ODS_test",
  #UID = Sys.getenv("userid"),  # need to change in Connect {vars}
  #PWD = Sys.getenv("pwd"),  # need to change in Connect {vars}
  trusted_connection = "yes"
)
onStop(function() {
  poolClose(pool)
})


# Data template
template <- read_csv('data/template.csv')



# Necessary Functions

# identify data issues before proceeding to full query
queryPreCheck <- function(pool, inputStations){
  # first make sure datetime correct
  dateIssues <- filter(inputStations, str_detect(`Collection Date`, '-')) %>% # find dates in - format not /
    mutate(`Potential Issue` = 'Date uses - not / as separator')
  timeIssues <- filter(inputStations, str_detect(`Collection Date`, ':', negate = TRUE)) %>% # find times without :
    mutate(`Potential Issue` = 'Time does not have appropriate separator ( : )')
  methodIssues <- filter(inputStations, ! Gradient %in% c('Riffle', 'Boatable', 'MACS')) %>% # find inappropriate method in input
    mutate(`Potential Issue` = 'Gradient is not correct. Must be one of: Riffle, Boatable, MACS')
  
  # remove sites with known issues
  querySites <- filter(inputStations, ! StationID %in% dateIssues$StationID) %>%
    filter( ! StationID %in% timeIssues$StationID)
  
  # Test query
  if(nrow(querySites) > 0){
    benSamps <- pool %>% tbl(in_schema("wqm",  "Edas_Benthic_Sample_View")) %>%
      filter(STA_ID %in% !! toupper(querySites$StationID) &
               FDT_DATE_TIME %in% !! querySites$`Collection Date` &
               WBS_REP_NUM %in% !! querySites$RepNum & 
               WBCM_DESCRIPTION %in% !! querySites$Gradient) %>%
      as_tibble()
    
    # make sure all stations that were supposed to return data do
    missingData <- filter(querySites, ! StationID %in% benSamps$STA_ID) %>%
      mutate(`Potential Issue` = 'No data in CEDS with the StationID, Collection Date/Time, Gradient, and RepNum combination')
    
    return(bind_rows(dateIssues, timeIssues, methodIssues) %>%
             bind_rows(missingData))
    
  } else { return(bind_rows(dateIssues, timeIssues, methodIssues)) }
  
}

# Pull CEDS data
pullQAQCsample <- function(pool, inputStations){
  benSamps <- pool %>% tbl(in_schema("wqm",  "Edas_Benthic_Sample_View")) %>%
    filter(STA_ID %in% !! toupper(inputStations$StationID) &
             FDT_DATE_TIME %in% !! inputStations$`Collection Date` & 
             WBCM_DESCRIPTION %in% !! inputStations$Gradient) %>%
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
      pool %>% tbl(in_schema("wqm",  "Edas_Benthic_View")) %>%
        filter(WBS_SAMP_ID %in% !! toupper(benSamps$BenSampID)) %>%
        as_tibble() %>%
        rename( "StationID" = "STA_ID",
                "BenSampID"  = "WBS_SAMP_ID",
                "RepNum" = "WBS_REP_NUM",
                "Gradient" = "WBCM_DESCRIPTION",
                "FinalID" = "WBMT_FINAL_ID",
                "Individuals" = "WBE_INDIVIDUALS",
                "ID Comments" = "WBE_COMMENT",
                "Entered By" = "WBE_INSERTED_BY", # not in EDAS table but good info
                "Taxonomist" = "TAXONOMIST_NAME",  # not in EDAS table but good info
                "Entered Date" = "WBE_INSERTED_DATE") %>%
        mutate(`Excluded Taxa` = ifelse(WBE_EXCLUDED_TAXA_YN == "Y", -1, 0)) %>%
        dplyr::select(StationID, BenSampID, RepNum, Gradient, FinalID, Individuals, 
                      `Excluded Taxa`, `ID Comments`, Taxonomist, `Entered By`, `Entered Date`) %>%
        # add in benSamp info to only return one dataset
        left_join(benSamps, by = c("StationID", "BenSampID", "RepNum", "Gradient", "Taxonomist")) %>% # enter date and enter by caused join issues so drop
        dplyr::select(-starts_with('Entered')) %>%
        arrange(StationID, BenSampID))
  } else {
    return(
      tibble(StationID = as.character(NA), BenSampID = as.character(NA), RepNum = as.numeric(NA), Gradient = as.character(NA), 
             FinalID = as.character(NA), Individuals = as.numeric(NA), `Excluded Taxa` = as.numeric(NA),
             `ID Comments` = as.character(NA), Taxonomist = as.character(NA), `Collection Date` = as.POSIXct(NA), 
             `Sample Comments` = as.character(NA),`Collected By` = as.character(NA), `Field Team` = as.character(NA), 
             `Target Count` = as.numeric(NA),  Season = as.character(NA)) )
  }
}


# double check data is correctly entered in CEDS before doing QA
QAdataEntry <- function(preliminaryBenthics, inputStations){
  
  dataOut <- preliminaryBenthics[0,]
  problemDataOut <- tibble(StationID = NA, Problem = NA)
  
  # now get appropriate samples based on Rick's Rep Number and anything with a rep num > 10
  for(i in 1:nrow(inputStations)){
    print(paste(inputStations$StationID[i], inputStations$`Collection Date`[i], inputStations$RepNum[i], inputStations$Gradient[i]))
    z <- filter(preliminaryBenthics, StationID == inputStations$StationID[i] &
                  `Collection Date` == as.POSIXct(inputStations$`Collection Date`[i],format = "%m/%d/%Y %H:%M", tz = 'UTC') &
                  Gradient == inputStations$Gradient[i]) 
    repToQA <- as.numeric(inputStations$RepNum[i])#filter(inputStations, StationID == i)$RepNum
    
    # Rick's desired RepNum in CEDS
    if(repToQA %in% z$RepNum){
      # Rick's desired RepNum in CEDS AND some sort of QA sample entered
      if(nrow(filter(z, RepNum > 10)) > 0 ){
        # Rick's desired RepNum in CEDS AND QA sample entered correctly
        if(nrow(filter(z, RepNum %in% c(repToQA + 10, repToQA + 20))) > 0 ){
          # output only desired data for QA
          dataPasses <- filter(z, RepNum %in% repToQA | RepNum > 10)
          problemData <- tibble(StationID = NA, Problem = NA)
        } else {
          dataPasses <- preliminaryBenthics[0,]
          problemData <- tibble(StationID = unique(z$StationID),
                                Problem = 'QA RepNum incorrectly entered') }
      } else {    # Rick's desired RepNum in CEDS but no QA sample (determined by RepNum > 10) in CEDS
        dataPasses <- preliminaryBenthics[0,]
        problemData <- tibble(StationID = unique(z$StationID),
                              Problem = 'No data with RepNum > 10, QA RepNum or Method data either incorrectly entered or not entered at all') }
    } else { # Rick's desired RepNum NOT in CEDS
      dataPasses <- preliminaryBenthics[0,]
      problemData <- tibble(StationID = unique(z$StationID),
                            Problem = 'No data for desired RepNum in CEDS')   }
    
    dataOut <- bind_rows(dataOut, dataPasses)
    problemDataOut <- bind_rows(problemDataOut, problemData)
  }
  problemDataOut <- drop_na(problemDataOut)
  finalDataOut <- list(cleanData = dataOut, problemData = problemDataOut)
}


organizeTaxaLists <- function(singleQAdataset, # data from one StationID
                              masterTaxaGenus # with PTC addition
){
  left_join(singleQAdataset, dplyr::select(masterTaxaGenus, Order, Family, FinalID, PTCscore )) %>%
    dplyr::select(StationID, Order, Family, FinalID, PTCscore, BenSampID, Individuals) %>%
    pivot_wider(names_from = BenSampID, values_from = Individuals) %>%
    replace(is.na(.), 0) %>% # change any NAs to 0
    mutate_if(is.character, list(~na_if(., 0))) %>% # but that messes up any NAs in Order/family so change those back
    arrange(Order, Family, FinalID) 
}

reformatForQA <- function(x, QAtype){
  # reformat data to match expected format
  x %>% 
    dplyr::select(StationID, Order, Family, FinalID, PTCscore, QAsample = starts_with(QAtype), everything()) %>% # reorganize columns and rename for consistency
    rename( Sample =  !!names(.[7])) %>%
    filter(QAsample > 0 | Sample > 0) %>%
    rowwise() %>%
    mutate(Difference = abs(QAsample - Sample),
           Agreement = min(QAsample, Sample),
           PTC_QA = QAsample * PTCscore,
           PTC_O = Sample * PTCscore) %>%
    ungroup()
}


QAsummaryMetrics <- function(x, QAsample, Sample, StationID){
  summarise(x, 
            PTD = (1 - (sum(Agreement, na.rm = T) / max( sum(QAsample, na.rm = T), sum(Sample, na.rm = T) ))) * 100, # percent taxonomic disagreement
            PTA = 100 - PTD, # percent taxonomic agreement
            PDE = ((sum(QAsample, na.rm = T) - sum(Sample, na.rm = T)) / (sum(QAsample, na.rm = T) + sum(Sample, na.rm = T))) * 100 , # percent difference in enumeration
            PTC_QA = (sum(PTC_QA, na.rm = T) / sum(QAsample, na.rm = T)) * 100, #(sum((QAsample * PTCscore)/QAsample))),
            PTC_O = (sum(PTC_O, na.rm = T) / sum(Sample, na.rm = T)) * 100,
            PTCabs = abs(PTC_QA - PTC_O)) %>% # absolute difference in PTC between QA sample and Sample sample
    mutate(QAsample = !! QAsample,
           Sample = !! Sample,
           StationID = !! StationID) %>%
    dplyr::select(StationID, QAsample, Sample, everything())
}



QAQCmasterFunction <- function(organizedDataset){
  # Identify if there are more than one type of QA sample per sample
  QAsampleData <- dplyr::select(organizedDataset, -starts_with('EPAQAQC'))
  EPAQAsampleData <- dplyr::select(organizedDataset, -starts_with('QAQC'))
  
  # keep sample names for later
  StationID <- dplyr::select(organizedDataset, StationID) %>% slice_head() %>% pull()
  QAsample <- dplyr::select(organizedDataset, starts_with('QAQC')) %>% names()
  EPAQAsample <- dplyr::select(organizedDataset, starts_with('EPAQAQC')) %>% names() # will only populate if exists
  Sample <- dplyr::select(organizedDataset, -c(StationID, Order, Family, FinalID, PTCscore, starts_with('QAQC'), starts_with('EPAQAQC'))) %>% names()
  
  if(ncol(QAsampleData) == 7){ # if there is a QA and sample to analyze
    QAdata <- reformatForQA(QAsampleData, "QAQC")
    QAmetrics <- QAsummaryMetrics(QAdata, QAsample, Sample, StationID) 
    # fix QAdata names before returning to user
    QAdata <- rename(QAdata, !!QAsample := QAsample,
                     !!Sample := Sample)
    QAoutput <- list(QAdata = QAdata,  QAmetrics = QAmetrics)
  }
  
  if(ncol(EPAQAsampleData) == 7){ # if there is a EPAQA and sample to analyze
    if(length(QAsample) >0){
      EPAQAdata <- reformatForQA(EPAQAsampleData, "EPAQAQC")
      EPAQAmetrics <- QAsummaryMetrics(EPAQAdata, EPAQAsample, Sample, StationID) 
      # fix QAdata names before returning to user
      EPAQAdata <- rename(EPAQAdata, !!EPAQAsample := QAsample,
                          !!Sample := Sample)
      # QAoutput <- list(QAdata = QAdata,  QAmetrics = QAmetrics,
      #                  EPAQAdata = EPAQAdata,  EPAQAmetrics = EPAQAmetrics)
      # 
      
      # organize data for EPA vs DEQ QA comparison
      DEQQAvsEPAsampleData <- dplyr::select(EPAQAsampleData, StationID:PTCscore, starts_with('EPAQAQC')) %>% 
        filter_at(vars(starts_with('EPAQAQC')), all_vars(. > 0)) %>% #drop taxa not IDed by Pond
        full_join(dplyr::select(QAsampleData, StationID:PTCscore, starts_with('QAQC')) %>% 
                    filter_at(vars(starts_with('QAQC')), all_vars(. > 0)), #drop taxa not IDed by DEQ QA biologist
                  by = c("StationID", "Order", "Family", "FinalID", "PTCscore") ) %>% 
        mutate_if(is.numeric, ~replace_na(., 0)) # replace any NA's produced through join but only to numeric columns in case NA in a name

      DEQQAvsEPAdata <- reformatForQA(DEQQAvsEPAsampleData, "EPAQAQC")
      DEQQAvsEPAmetrics <- QAsummaryMetrics(DEQQAvsEPAdata, EPAQAsample, QAsample, StationID) 
      # fix QAdata names before returning to user
      DEQQAvsEPAdata <- rename(DEQQAvsEPAdata, !!EPAQAsample := QAsample,
                          !!QAsample := Sample)
      QAoutput <- list(QAdata = QAdata,  QAmetrics = QAmetrics,
                       EPAQAdata = EPAQAdata,  EPAQAmetrics = EPAQAmetrics,
                       DEQQAvsEPAdata = DEQQAvsEPAdata,  DEQQAvsEPAmetrics = DEQQAvsEPAmetrics)
      
      
    } else {
      EPAQAdata <- reformatForQA(EPAQAsampleData, "EPAQAQC")
      EPAQAmetrics <- QAsummaryMetrics(EPAQAdata, EPAQAsample, Sample, StationID) 
      # fix QAdata names before returning to user
      EPAQAdata <- rename(EPAQAdata, !!EPAQAsample := QAsample,
                          !!Sample := Sample)
      QAoutput <- list(#QAdata = QAdata,  QAmetrics = QAmetrics,
                       EPAQAdata = EPAQAdata,  EPAQAmetrics = EPAQAmetrics)
    }
  }
  return(QAoutput)
}

# How to repeat for each sample
QAQCmasterFunction_df <- function(inputStations,
                                  benthics, # pulled benthic data
                                  masterTaxaGenus){
  out <- list()
  for(i in 1:nrow(inputStations)){
    # catch in case >1 sample event per station in input
    # sampleEvents <- filter(benthics, StationID %in% i) %>% 
    #   group_by(StationID, `Collection Date`) %>% 
    #   summarize()
    # 
    # if(nrow(sampleEvents) > 1){
    #   for( k in as.character(unique(sampleEvents$`Collection Date`))){
    #     newName <- paste(i, gsub(":", "", k)) # have to take out : or Excel will not write sheet names properly upon download
    #     print(newName)
    #     benthics1 <- organizeTaxaLists(filter(benthics, StationID %in% i &
    #                                    `Collection Date` %in% as.POSIXct(k, tz = 'UTC')), masterTaxaGenus) 
    #     if(ncol(benthics1) > 6){
    #       out[[newName]] <- QAQCmasterFunction(benthics1)
    #     } else { # means no QA data yet
    #       out[[newName]] <- list(QAdata = benthics1,
    #                        QAmetrics = dplyr::select(benthics, StationID, Sample = BenSampID) %>%
    #                          slice(1) %>% mutate(QAsample = NA, PTD = NA, PTA = NA, PDE = NA, PTC_QA = NA, PTC_O = NA, PTCabs= NA) %>%
    #                          dplyr::select(StationID, QAsample, Sample, everything()) )
    #     }
    #   }
    #   
    #   
    # } else {
      
      benthics1 <- organizeTaxaLists(filter(benthics, StationID == inputStations$StationID[i] &
                                              `Collection Date` == as.POSIXct(inputStations$`Collection Date`[i],format = "%m/%d/%Y %H:%M", tz = 'UTC') &
                                              Gradient == inputStations$Gradient[i]), 
                                     masterTaxaGenus) 
      newName <- paste(inputStations$StationID[i], 
                       gsub(":", "", inputStations$`Collection Date`[i]), # have to take out : or Excel will not write sheet names properly upon download
                       inputStations$Gradient[i])
                      #substr(inputStations$Gradient[i], 1,3)) # first letters bc name can get too long for Excel
      print(newName)
      if(ncol(benthics1) > 6){
        out[[newName]] <- QAQCmasterFunction(benthics1)
      } else { # means no QA data yet
        out[[newName]] <- list(QAdata = benthics1,
                         QAmetrics = dplyr::select(benthics, StationID, Sample = BenSampID) %>%
                           slice(1) %>% mutate(QAsample = NA, PTD = NA, PTA = NA, PDE = NA, PTC_QA = NA, PTC_O = NA, PTCabs= NA) %>%
                           dplyr::select(StationID, QAsample, Sample, everything()) )
      }
    }
    
    
  #}
  return(out)
}
