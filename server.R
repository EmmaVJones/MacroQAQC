source('global.R')

shinyServer(function(input, output, session) {
  
  # empty reactive objects list
  reactive_objects = reactiveValues() 
  
  ##################################### How to page
  output$BenthicDataQAQCToolHowTo <- renderUI({includeHTML("BenthicDataQAQCToolHowTo.html")})
  
  #output$BenthicQueryToolHowTo <- renderUI({includeHTML("BenthicQueryToolHowTo.html")})
  
  # Get master taxa data from weekly pin update
  observe({
    reactive_objects$masterTaxaGenus <- pin_get("ejones/masterTaxaGenus", board = "rsconnect") %>%
      mutate(PTCscore = ifelse(FinalID == Genus | FinalID == Species, 1, 0)) # helper to flag when taxonomist ID's sample to desired level
  })
  
  #################################### QA Tool Page###################################
  
  ##### Sidebar Panel
  
  # User Upload- Bring in user desired stations
  # Download data template
  output$downloadTemplate <- downloadHandler(filename=function(){'template.csv'},
                                             content=function(file){write.csv(template,file,row.names=FALSE)})
  
  # Upload user station list
  inputStations <- reactive({inFile <- input$inputStations
  if(is.null(inFile))
    return(NULL)
  read_csv(inFile$datapath) })
  
  ##### Main Panel- Data PreCheck Tab Panel
  
  # Display user input data
  output$uploadedStationTable <- DT::renderDataTable({req(inputStations())#, reactive_objects$benthics)
    DT::datatable(inputStations() %>% mutate(`Collection Date` = as.character(`Collection Date`)) %>%
                    #left_join(reactive_objects$benthicsSummary, by = 'StationID') %>%
                    arrange(StationID),
                  escape=F, rownames = F,
                  options=list(dom = 'it', scrollY = "200px",pageLength=nrow(inputStations())))})
  
  # check uploaded data for issues
  issueSites <- reactive({req(inputStations())
    queryPreCheck(pool, inputStations()) %>% arrange(StationID)})
  
  output$issuesTable <-  DT::renderDataTable({req(issueSites())
    DT::datatable(issueSites(), escape=F, rownames = F,
                  options=list(dom = 'it', scrollY = "200px",pageLength=nrow(issueSites())))})
  
  output$stationsForQATable <- DT::renderDataTable({req(issueSites())
    z <- filter(inputStations(),! StationID %in% issueSites()$StationID) %>%
      arrange(StationID)
    DT::datatable(z, escape=F, rownames = F,
                  options=list(dom = 'it', scrollY = "200px",pageLength=nrow(z)))})
  
  
  ##### Main Panel- Data Retrieved Tab Panel
  
  # Pull Benthic Data from CEDS
  observeEvent(issueSites(), {
    ## Benthic Information
    reactive_objects$preliminaryBenthics <- pullQAQCsample(pool, filter(inputStations(),! StationID %in% issueSites()$StationID)) %>%
      arrange(StationID)})
  
  # Display data retrieved from CEDS
  output$benthicData <- DT::renderDataTable({req(reactive_objects$preliminaryBenthics)
    DT::datatable(reactive_objects$preliminaryBenthics %>% mutate(`Collection Date` = as.character(`Collection Date`)) %>%
                    dplyr::select(StationID, BenSampID, `Collection Date`, everything()),
                  escape=F, rownames = F, extensions = 'Buttons',
                  options=list(dom = 'Bift', scrollY = "400px", buttons=list('copy','colvis'),
                               pageLength=nrow(reactive_objects$preliminaryBenthics)))})
  
  # Identify CEDS data entry issues
  CEDSdataIssues <- reactive({req(reactive_objects$preliminaryBenthics)
    QAdataEntry(reactive_objects$preliminaryBenthics, inputStations()) })
  
  # Display CEDS data entry issues
  output$CEDSdataEntryTable <- DT::renderDataTable({req(CEDSdataIssues())
    DT::datatable(CEDSdataIssues()$problemData %>% arrange(StationID),
                  escape=F, rownames = F, extensions = 'Buttons',
                  options=list(dom = 'Bit', scrollY = "400px", buttons=list('copy','colvis'),
                               pageLength=nrow(CEDSdataIssues()$problemData)))})
      
  # Display data retrieved from CEDS
  output$benthicDataForQA <- DT::renderDataTable({req(reactive_objects$benthics)
    DT::datatable(reactive_objects$benthics, escape=F, rownames = F, extensions = 'Buttons',
                  options=list(dom = 'Bift', scrollY = "400px", buttons=list('copy','colvis'),
                               pageLength=nrow(reactive_objects$benthics)))})
  
  
  observeEvent(CEDSdataIssues(), {
    # Remoe issue sites from benthic information for review
    reactive_objects$benthics <- CEDSdataIssues()$cleanData %>% arrange(StationID)

    # benthics Summary
    reactive_objects$benthicsSummary <- reactive_objects$benthics %>%
      group_by(StationID) %>%
      summarize(BenSampID) %>%
      distinct(BenSampID, .keep_all = T) %>%
      mutate(Type = case_when(str_detect(BenSampID, "^QAQC") ~ "QA Sample",
                              str_detect(BenSampID, "^EPAQAQC") ~ "EPA QA Sample",
                              TRUE~ as.character('Original Sample') )) %>%
      pivot_wider(names_from = Type, values_from = BenSampID)

    ## QA results
    reactive_objects$QAanalysis <- QAQCmasterFunction_df(reactive_objects$benthics, reactive_objects$masterTaxaGenus)
    # list of tibbles with benthic comparisons
    reactive_objects$DEQQAresults <- map(reactive_objects$QAanalysis, "QAdata")
    reactive_objects$EPAQAresults <- map(reactive_objects$QAanalysis, "EPAQAdata")
    reactive_objects$DEQQAvsEPAQAresults <- map(reactive_objects$QAanalysis, "DEQQAvsEPAdata")
    
    # tibbles of QA metric information
    reactive_objects$DEQQAmetrics <- map_df(reactive_objects$QAanalysis, "QAmetrics")
    reactive_objects$EPAQAmetrics <- map_df(reactive_objects$QAanalysis, "EPAQAmetrics")
    reactive_objects$DEQQAvsEPAQAmetrics <- map_df(reactive_objects$QAanalysis, "DEQQAvsEPAmetrics")
    

    # for data download
    reactive_objects$DEQQAresultsOUT <- reactive_objects$DEQQAresults
    reactive_objects$DEQQAresultsOUT[["QAmetrics"]] <- reactive_objects$DEQQAmetrics # add metrics df to QAresults structure
    reactive_objects$DEQQAresultsOUT <- purrr::map(reactive_objects$DEQQAresultsOUT, ~ purrr::compact(.)) %>%
      purrr::keep(~length(.) != 0) # only send out objects with data
    
    
    reactive_objects$EPAQAresultsOUT <- reactive_objects$EPAQAresults
    reactive_objects$EPAQAresultsOUT[["EPAQAmetrics"]] <- reactive_objects$EPAQAmetrics # add metrics df to QAresults structure
    reactive_objects$EPAQAresultsOUT <- purrr::map(reactive_objects$EPAQAresultsOUT, ~ purrr::compact(.)) %>%
      purrr::keep(~length(.) != 0) # only send out objects with data
    
    reactive_objects$DEQQAvsEPAQAresultsOUT <- reactive_objects$DEQQAvsEPAQAresults
    reactive_objects$DEQQAvsEPAQAresultsOUT[["DEQQAvsEPAQAmetrics"]] <- reactive_objects$DEQQAvsEPAQAmetrics # add metrics df to QAresults structure
    reactive_objects$DEQQAvsEPAQAresultsOUT <- purrr::map(reactive_objects$DEQQAvsEPAQAresultsOUT, ~ purrr::compact(.)) %>%
      purrr::keep(~length(.) != 0) # only send out objects with data
  })

  
  ##### Main Panel- QA Metrics Tab Panel
  
  output$QAStationTable <- DT::renderDataTable({req(inputStations(), reactive_objects$benthics)
    DT::datatable(reactive_objects$benthicsSummary %>%
                    arrange(StationID), escape=F, rownames = F,
                  options=list(dom = 'it', scrollY = "200px",pageLength=nrow(reactive_objects$benthicsSummary)))})
  
  
  ## DEQ metrics table
  output$DEQresults <- DT::renderDataTable({req(nrow(reactive_objects$DEQQAmetrics) > 0)
    DT::datatable(reactive_objects$DEQQAmetrics,
                  escape=F, rownames = F, extensions = 'Buttons',
                  options=list(dom = 'Bift', scrollX=TRUE,
                               scrollY = "200px", buttons=list('copy','colvis'),
                               pageLength=nrow(reactive_objects$DEQQAmetrics))) %>%
      DT::formatRound(columns=c('PTD', 'PTA', "PDE", "PTC_QA", "PTC_O", "PTCabs"), digits=2)})

  ## EPA metrics table
  output$EPAresults <- DT::renderDataTable({req(nrow(reactive_objects$EPAQAmetrics) > 0)
    DT::datatable(reactive_objects$EPAQAmetrics,
                  escape=F, rownames = F, extensions = 'Buttons',
                  options=list(dom = 'Bift', scrollX=TRUE,
                               scrollY = "200px", buttons=list('copy','colvis'),
                               pageLength=nrow(reactive_objects$EPAQAmetrics))) %>%
      DT::formatRound(columns=c('PTD', 'PTA', "PDE", "PTC_QA", "PTC_O", "PTCabs"), digits=2)})

  ## DEQ QA vs EPA metrics table
  output$DEQQAvsEPAresults <- DT::renderDataTable({req(nrow(reactive_objects$DEQQAvsEPAQAmetrics) > 0)
    DT::datatable(reactive_objects$DEQQAvsEPAQAmetrics,
                  escape=F, rownames = F, extensions = 'Buttons',
                  options=list(dom = 'Bift', scrollX=TRUE,
                               scrollY = "200px", buttons=list('copy','colvis'),
                               pageLength=nrow(reactive_objects$DEQQAvsEPAQAmetrics))) %>%
      DT::formatRound(columns=c('PTD', 'PTA', "PDE", "PTC_QA", "PTC_O", "PTCabs"), digits=2)})
  
  

  ##### Main Panel- QA Results Tab Panel

  output$stationSelectionUI <- renderUI({req(reactive_objects$DEQQAresults)
    selectInput('stationSelection', 'Select StationID to analyze', choices = names(reactive_objects$DEQQAresults)) })

  ## DEQ Taxa Comparison table
  
  #output$testtest <- renderPrint({reactive_objects$DEQQAresults[[input$stationSelection]]})

  output$DEQstationLineup <- DT::renderDataTable({req(reactive_objects$DEQQAresults, input$stationSelection)
    z <- reactive_objects$DEQQAresults[[input$stationSelection]]
    DT::datatable(z, escape=F, rownames = F, extensions = 'Buttons',
                  options=list(dom = 'Bift', scrollX=TRUE,
                               scrollY = "300px", buttons=list('copy','colvis'),
                               pageLength=nrow(z))) })

  # metric results underneath sample
  output$DEQstationLineupMetrics <- DT::renderDataTable({req(nrow(reactive_objects$DEQQAmetrics) > 0, input$stationSelection)
    z <- filter(reactive_objects$DEQQAmetrics, StationID %in% input$stationSelection)
    DT::datatable(z, escape=F, rownames = F, extensions = 'Buttons',
                  options=list(dom = 'it', scrollX=TRUE)) })


  ## EPA Taxa Comparison table 
  
  output$EPAstationLineup <- DT::renderDataTable({req(reactive_objects$EPAQAresults, input$stationSelection)
    z <- reactive_objects$EPAQAresults[[input$stationSelection]]
    DT::datatable(z,escape=F, rownames = F, extensions = 'Buttons',
                    options=list(dom = 'Bift', scrollX=TRUE,
                                 scrollY = "300px", buttons=list('copy','colvis'),
                                 pageLength=nrow(z)))   })


  # metric results underneath sample
  output$EPAstationLineupMetrics <- DT::renderDataTable({req(nrow(reactive_objects$EPAQAmetrics) > 0, input$stationSelection)
      z <- filter(reactive_objects$EPAQAmetrics, StationID %in% input$stationSelection)
      DT::datatable(z, escape=F, rownames = F, extensions = 'Buttons',
                    options=list(dom = 'it', scrollX=TRUE)) })
    

  ## DEQ QA vs EPA Taxa Comparison table 
  
  output$DEQQAvsEPAstationLineup <- DT::renderDataTable({req(reactive_objects$DEQQAvsEPAQAresults, input$stationSelection)
    z <- reactive_objects$DEQQAvsEPAQAresults[[input$stationSelection]]
    DT::datatable(z,escape=F, rownames = F, extensions = 'Buttons',
                  options=list(dom = 'Bift', scrollX=TRUE,
                               scrollY = "300px", buttons=list('copy','colvis'),
                               pageLength=nrow(z)))   })
  
  
  # metric results underneath sample
  output$DEQQAvsEPAstationLineupMetrics <- DT::renderDataTable({req(nrow(reactive_objects$DEQQAvsEPAQAmetrics) > 0, input$stationSelection)
    z <- filter(reactive_objects$DEQQAvsEPAQAmetrics, StationID %in% input$stationSelection)
    DT::datatable(z, escape=F, rownames = F, extensions = 'Buttons',
                  options=list(dom = 'it', scrollX=TRUE)) })
  

  ##### Main Panel- Download Results Tab Panel

  # Don't let user click pull data button if no data to download
  #observe({
  #  shinyjs::toggleState('downloadDEQResults', length(reactive_objects$DEQQAresultsOUT) == 0 )
  #})
  output$downloadDEQResults <- downloadHandler(filename=function(){paste0('DEQbenthicQAresults_',Sys.Date(),'.xlsx')},
                                               content=function(file){
                                                 openxlsx::write.xlsx(reactive_objects$DEQQAresultsOUT, file = file) })
  output$downloadEPAResults <- downloadHandler(filename=function(){paste0('EPAbenthicQAresults_',Sys.Date(),'.xlsx')},
                                               content=function(file){
                                                 openxlsx::write.xlsx(reactive_objects$EPAQAresultsOUT, file = file) })
  output$downloadDEQQAvsEPAResults <- downloadHandler(filename=function(){paste0('DEQQAvsEPAbenthicQAresults_',Sys.Date(),'.xlsx')},
                                               content=function(file){
                                                 openxlsx::write.xlsx(reactive_objects$DEQQAvsEPAQAresultsOUT, file = file) })
  

})