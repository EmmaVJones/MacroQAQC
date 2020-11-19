source('global.R')

 masterTaxaGenus <- pin_get("ejones/masterTaxaGenus", board = "rsconnect") %>%
  mutate(PTCscore = ifelse(FinalID == Genus | FinalID == Species, 1, 0)) # helper to flag when taxonomist ID's sample to desired level


inputStations <- read_csv('data/template.csv')

benthics <- pullQAQCsample(pool, inputStations) %>% arrange(StationID)
QAanalysis <- QAQCmasterFunction_df(benthics, masterTaxaGenus)


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
