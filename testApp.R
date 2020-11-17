source('global.R')

 masterTaxaGenus <- pin_get("ejones/masterTaxaGenus", board = "rsconnect") %>%
  mutate(PTCscore = ifelse(FinalID == Genus | FinalID == Species, 1, 0)) # helper to flag when taxonomist ID's sample to desired level


inputStations <- read_csv('data/template.csv')

benthics <- pullQAQCsample(pool, inputStations) %>% arrange(StationID)
QAanalysis <- QAQCmasterFunction_df(benthics, masterTaxaGenus)
