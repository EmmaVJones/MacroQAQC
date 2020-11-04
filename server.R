source('global.R')

shinyServer(function(input, output, session) {
  
  # empty reactive objects list
  reactive_objects = reactiveValues() 
  
  ##################################### How to page
  output$BenthicDataQAQCToolHowTo <- renderUI({includeHTML("BenthicDataQAQCToolHowTo.html")})
  
  output$BenthicQueryToolHowTo <- renderUI({includeHTML("BenthicQueryToolHowTo.html")})
  
  # Get master taxa data from weekly pin update
  observe({
    reactive_objects$masterTaxaGenus <- masterTaxaGenus <- pin_get("ejones/masterTaxaGenus", board = "rsconnect") %>%
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
  
  ##### Main Panel- Data Retrieved Tab Panel
  
  # Display user input data
  output$uploadedStationTable <- DT::renderDataTable({req(inputStations())
    DT::datatable(inputStations() %>% mutate(`Collection Date` = as.character(`Collection Date`)),
                  escape=F, rownames = F,
                  options=list(dom = 't', scrollY = "200px",pageLength=nrow(inputStations())))})
  
  # Pull Benthic Data from CEDS
  observeEvent(inputStations(), {
    ## Benthic Information
    reactive_objects$benthics <- #pullQAQCsample(pool, inputStations()) })
      
      # for testing
      read_csv('testData.csv') })
      
      
  observeEvent(reactive_objects$benthics, {
    ## QA results
    reactive_objects$QAanalysis <- QAQCmasterFunction_df(reactive_objects$benthics, reactive_objects$masterTaxaGenus)
    # list of tibbles with benthic comparisons
    reactive_objects$DEQQAresults <- map(reactive_objects$QAanalysis, "QAdata")
    reactive_objects$EPAQAresults <- map(reactive_objects$QAanalysis, "EPAQAdata")
    # tibbles of QA metric information
    reactive_objects$DEQQAmetrics <- map_df(reactive_objects$QAanalysis, "QAmetrics")
    reactive_objects$EPAQAmetrics <- map_df(reactive_objects$QAanalysis, "EPAQAmetrics")
    
    # for data download
    reactive_objects$DEQQAresultsOUT <- reactive_objects$DEQQAresults
    reactive_objects$DEQQAresultsOUT[["QAmetrics"]] <- reactive_objects$DEQQAmetrics # add metrics df to QAresults structure
    reactive_objects$EPAQAresultsOUT <- reactive_objects$EPAQAresults
    reactive_objects$EPAQAresultsOUT[["EPAQAmetrics"]] <- reactive_objects$EPAQAmetrics # add metrics df to QAresults structure
    reactive_objects$EPAQAresultsOUT <- purrr::map(reactive_objects$EPAQAresultsOUT, ~ purrr::compact(.)) %>% 
      purrr::keep(~length(.) != 0) # only send out objects with data
  })
  
  # Display data retrieved from CEDS
  output$benthicData <- DT::renderDataTable({req(reactive_objects$benthics)
    DT::datatable(reactive_objects$benthics %>% mutate(`Collection Date` = as.character(`Collection Date`)),
                  escape=F, rownames = F, extensions = 'Buttons',
                  options=list(dom = 'Bft', scrollY = "400px", buttons=list('copy','colvis'),
                               pageLength=nrow(reactive_objects$benthics)))})
  
  ##### Main Panel- QA Metrics Tab Panel
  
  ## DEQ metrics table
  output$DEQresults <- DT::renderDataTable({req(reactive_objects$DEQQAmetrics)
    DT::datatable(reactive_objects$DEQQAmetrics,
                  escape=F, rownames = F, extensions = 'Buttons',
                  options=list(dom = 'Bft', scrollX=TRUE,
                               scrollY = "200px", buttons=list('copy','colvis'),
                               pageLength=nrow(reactive_objects$DEQQAmetrics))) %>%
      DT::formatRound(columns=c('PTD', 'PTA', "PDE", "PTC_QA", "PTC_O", "PTCabs"), digits=2)})
  
  ## EPA metrics table
  output$EPAresults <- DT::renderDataTable({req(reactive_objects$EPAQAmetrics)
    DT::datatable(reactive_objects$EPAQAmetrics,
                  escape=F, rownames = F, extensions = 'Buttons',
                  options=list(dom = 'Bft', scrollX=TRUE,
                               scrollY = "200px", buttons=list('copy','colvis'),
                               pageLength=nrow(reactive_objects$EPAQAmetrics))) %>%
      DT::formatRound(columns=c('PTD', 'PTA', "PDE", "PTC_QA", "PTC_O", "PTCabs"), digits=2)})
  
  
  ##### Main Panel- QA Results Tab Panel
  
  output$stationSelectionUI <- renderUI({
    req(reactive_objects$DEQQAresults)
    selectInput('stationSelection', 'Select StationID to analyze', choices = names(reactive_objects$DEQQAresults)) })
  
  ## DEQ Taxa Comparison table
  
  #output$DEQstationLineup <- renderPrint({reactive_objects$DEQQAresults[[input$stationSelection]]})
  output$DEQstationLineup <- DT::renderDataTable({req(reactive_objects$DEQQAresults, input$stationSelection)
    DT::datatable(reactive_objects$DEQQAresults[[input$stationSelection]],
                  escape=F, rownames = F, extensions = 'Buttons',
                  options=list(dom = 'Bft', scrollX=TRUE,
                               scrollY = "300px", buttons=list('copy','colvis'),
                               pageLength=nrow(reactive_objects$DEQQAresults[[input$stationSelection]])))})
  
  output$EPAstationLineup <- DT::renderDataTable({req(reactive_objects$EPAQAresults, input$stationSelection)
    DT::datatable(reactive_objects$EPAQAresults[[input$stationSelection]],
                  escape=F, rownames = F, extensions = 'Buttons',
                  options=list(dom = 'Bft', scrollX=TRUE,
                               scrollY = "300px", buttons=list('copy','colvis'),
                               pageLength=nrow(reactive_objects$EPAQAresults[[input$stationSelection]])))})
  
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

})