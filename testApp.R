source('global.R')

masterTaxaGenus <- pin_get("ejones/masterTaxaGenus", board = "rsconnect") %>%
  mutate(PTCscore = ifelse(FinalID == Genus | FinalID == Species, 1, 0)) # helper to flag when taxonomist ID's sample to desired level


inputStations <- read_csv('data/2021_QAQC_RConnect_breakEVJ.csv') %>% #read_csv('data/template.csv')
  filter(StationID %in% c('2-JKS006.67') ) 
# inputStations[1,]<- c('2-JKS006.67', '8/24/2021 9:50', 1, 'Riffle' )
# inputStations[2,]<- c('2-JKS006.67', '8/24/2021 9:50', 1, 'Boatable' )


queryPreCheck(pool, inputStations) %>% arrange(StationID)

preliminaryBenthics <- pullQAQCsample(pool, inputStations) %>% arrange(StationID)
# preliminaryBenthics <- read_csv('data/boatableTest.csv') %>% 
#   mutate(`Sample Comments` = as.character(`Sample Comments`))

CEDSdataIssues <- QAdataEntry(preliminaryBenthics, inputStations)

#benthicDataForQA
benthics <- CEDSdataIssues$cleanData %>% arrange(StationID)

QAanalysis <- QAQCmasterFunction_df(inputStations, benthics, masterTaxaGenus)



# list of tibbles with benthic comparisons
DEQQAresults <- map(QAanalysis, "QAdata")
EPAQAresults <- map(QAanalysis, "EPAQAdata")
DEQQAvsEPAQAresults <- map(QAanalysis, "DEQQAvsEPAdata")

# tibbles of QA metric information
DEQQAmetrics <- map_df(QAanalysis, "QAmetrics")
EPAQAmetrics <- map_df(QAanalysis, "EPAQAmetrics")
DEQQAvsEPAQAmetrics <- map_df(QAanalysis, "DEQQAvsEPAmetrics")


# for data download
DEQQAresultsOUT <- DEQQAresults
DEQQAresultsOUT[["QAmetrics"]] <- DEQQAmetrics # add metrics df to QAresults structure
DEQQAresultsOUT <- purrr::map(DEQQAresultsOUT, ~ purrr::compact(.)) %>%
  purrr::keep(~length(.) != 0) # only send out objects with data

names(DEQQAresultsOUT) <- sub(" .*", "",  names(DEQQAresultsOUT)) # shorten names for Excel output

openxlsx::write.xlsx(DEQQAresultsOUT, file = paste0('DEQbenthicQAresults_',Sys.Date(),'.xlsx'))

EPAQAresultsOUT <- EPAQAresults
EPAQAresultsOUT[["EPAQAmetrics"]] <- EPAQAmetrics # add metrics df to QAresults structure
EPAQAresultsOUT <- purrr::map(EPAQAresultsOUT, ~ purrr::compact(.)) %>%
  purrr::keep(~length(.) != 0) # only send out objects with data

DEQQAvsEPAQAresultsOUT <- DEQQAvsEPAQAresults
DEQQAvsEPAQAresultsOUT[["DEQQAvsEPAQAmetrics"]] <- DEQQAvsEPAQAmetrics # add metrics df to QAresults structure
DEQQAvsEPAQAresultsOUT <- purrr::map(DEQQAvsEPAQAresultsOUT, ~ purrr::compact(.)) %>%
  purrr::keep(~length(.) != 0) # only send out objects with data






benthicsSummary <- benthics %>%
  group_by(StationID) %>%
  summarize(BenSampID) %>%
  distinct(BenSampID, .keep_all = T) %>%
  mutate(Type = case_when(str_detect(BenSampID, "^QAQC") ~ "QA Sample",
                          str_detect(BenSampID, "^EPAQAQC") ~ "EPA QA Sample",
                          TRUE~ as.character('Original Sample') )) %>%
  pivot_wider(names_from = Type, values_from = BenSampID)

left_join(inputStations, benthicsSummary, by = 'StationID')



benthics %>% mutate(`Collection Date` = as.character(`Collection Date`)) %>%
  dplyr::select(StationID, BenSampID, `Collection Date`, everything())

