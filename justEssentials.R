
library(tidyverse)
library(pool)
library(config)
library(pins)
library(lubridate)
library(openxlsx)
conn <- config::get("connectionSettings")



## For testing: connect to ODS production using local credentials
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "SQL Server Native Client 11.0", 
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)

# Necessary Functions

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
            PTD = (1 - (sum(Agreement) / sum(QAsample))) * 100, # percent taxonomic disagreement
            PTA = 100 - PTD, # percent taxonomic agreement
            PDE = ((sum(QAsample) - sum(Sample)) / (sum(QAsample) + sum(Sample))) *100 , # percent difference in enumeration
            PTC_QA = (sum(PTC_QA, na.rm = T) / sum(QAsample)) * 100, #(sum((QAsample * PTCscore)/QAsample))),
            PTC_O = (sum(PTC_O, na.rm = T) / sum(Sample)) * 100,
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
    EPAQAdata <- reformatForQA(EPAQAsampleData, "EPAQAQC")
    EPAQAmetrics <- QAsummaryMetrics(EPAQAdata, EPAQAsample, Sample, StationID) 
    # fix QAdata names before returning to user
    EPAQAdata <- rename(EPAQAdata, !!EPAQAsample := QAsample,
                        !!Sample := Sample)
    QAoutput <- list(QAdata = QAdata,  QAmetrics = QAmetrics,
                     EPAQAdata = EPAQAdata,  EPAQAmetrics = EPAQAmetrics)
  }
  return(QAoutput)
}

# How to repeat for each sample
QAQCmasterFunction_df <- function(x, # pulled benthic data
                                  masterTaxaGenus){
  out <- list()
  for(i in unique(x$StationID)){
    x1 <- organizeTaxaLists(filter(x, StationID %in% i), masterTaxaGenus) 
    if(ncol(x1) > 6){
      out[[i]] <- QAQCmasterFunction(x1)
    } else { # means no QA data yet
      out[[i]] <- list(QAdata = x1,
                       QAmetrics = dplyr::select(x, StationID, Sample = BenSampID) %>%
                         slice(1) %>% mutate(QAsample = NA, PTD = NA, PTA = NA, PDE = NA, PTC_QA = NA, PTC_O = NA, PTCabs= NA) %>%
                         dplyr::select(StationID, QAsample, Sample, everything()) )
    }
    
  }
  return(out)
}


# necessary steps

# set up pool
masterTaxaGenus <- pin_get("ejones/masterTaxaGenus", board = "rsconnect") %>%
  mutate(PTCscore = ifelse(FinalID == Genus | FinalID == Species, 1, 0)) # helper to flag when taxonomist ID's sample to desired level

family <- filter(masterTaxaGenus, FinalID == `Final VA Family ID`)

# pull some real data and some fake data, adjust the QAQCEPA code to newer EPAQAQC 
x <- pullQAQCsample(pool, tibble(StationID = c('2AXEM000.35', '2-TLR000.03'),
                                 `Collection Date` = c("2016-10-20 11:00:00",
                                                       "2016-05-04 09:30:00"))) %>%
  bind_rows(pullQAQCsample(pool, tibble(StationID = c('2-TLR000.03'),
                                        `Collection Date` = c("2016-05-04 09:30:00"))) %>%
              mutate(BenSampID = "QAQC011")) %>%
  mutate(BenSampID = case_when(BenSampID == 'QAQCEPA19101' ~ 'EPAQAQC19101',
                               TRUE ~ as.character(BenSampID)))

x1 <- organizeTaxaLists(x, masterTaxaGenus) #%>%

  
results <- QAQCmasterFunction_df(x, masterTaxaGenus)

# QA data
z <- results %>%
  map_df(1)


# With totally made up data to see what happens with multiple different types of sites
# pull data
x_fake <- read_csv('testData.csv')

results_fake <-  QAQCmasterFunction_df(x_fake, masterTaxaGenus)


# show data in app
QAresults <- map(results_fake, "QAdata")
EPAQAresults <- map(results_fake, "EPAQAdata")
# selectInput choose from names(QAresults)

# show metrics in app
QAmetrics <- map_df(results_fake, "QAmetrics")
EPAQAmetrics <- map_df(results_fake, "EPAQAmetrics")

# save results from app
QAresults[["QAmetrics"]] <-  map_df(results_fake, "QAmetrics") # add metrics df to QAresults structure
openxlsx::write.xlsx(QAresults, file = "DEQ_QAresults.xlsx")

EPAQAresults[["EPAQAmetrics"]] <-  map_df(results_fake, "EPAQAmetrics") # add metrics df to EPAQAresults structure
EPAQAresults <- purrr::map(EPAQAresults, ~ purrr::compact(.)) %>% 
   purrr::keep(~length(.) != 0)
openxlsx::write.xlsx(EPAQAresults, file = "EPA_QAresults.xlsx")


# New test data in CEDS
samples <- read_csv('data/template.csv')
testData <- pullQAQCsample(pool, samples)
results <- QAQCmasterFunction_df(testData, masterTaxaGenus)
