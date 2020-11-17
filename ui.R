shinyUI(fluidPage(#theme= "yeti.css",
                  navbarPage("Benthic Data QAQC Tool",
                             
                             tabPanel('How To',
                                      htmlOutput("BenthicDataQAQCToolHowTo") ),
                             tabPanel("QA Tool",
                                      sidebarPanel(
                                        h4("Instructions:"),
                                        p("Please upload a flat file (.csv) of all stations for review. All data uploaded 
                                          to the app must be formatted correctly. If you are unsure whether your data is in the correct 
                                          format, please download the 'template.csv' file first to check your data structure."),
                                        helpText("Query will pull directly from CEDS based on stations identified in 
                                                 the uploaded spreadsheet. CEDS data is refreshed nightly."),
                                        fileInput("inputStations", "Stations for QAQC", accept = ".csv"),
                                        br(),
                                        downloadButton('downloadTemplate',"Download template.csv")),
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel('Data Retrieved',
                                                   h4("Uploaded Stations"),
                                                   DT::dataTableOutput('uploadedStationTable'),
                                                   br(),
                                                   h4("Retrieved Benthic Data"),
                                                   DT::dataTableOutput('benthicData') ),
                                          tabPanel("QA Metrics",
                                                   h4("DEQ Internal QA Results"),
                                                   DT::dataTableOutput('DEQresults'),
                                                   br(),
                                                   h4("EPA QA Results"),
                                                   DT::dataTableOutput('EPAresults') ),
                                          tabPanel("QA Results",
                                                   uiOutput('stationSelectionUI'),
                                                   h4('DEQ Taxa Comparison'),
                                                   DT::dataTableOutput('DEQstationLineup'),
                                                   DT::dataTableOutput('DEQstationLineupMetrics'),
                                                   hr(),
                                                   h4('EPA Taxa Comparison'),
                                                   DT::dataTableOutput('EPAstationLineup'),
                                                   DT::dataTableOutput('EPAstationLineupMetrics')),
                                          tabPanel("Download Results",
                                                   helpText('Click the below button to download a MS Excel Workbook of all
                                                            DEQ QA metrics and QA results.'),
                                                   downloadButton('downloadDEQResults',"Download DEQ Results"),
                                                   helpText('Click the below button to download a MS Excel Workbook of all
                                                            EPA QA metrics and QA results.'),
                                                   downloadButton('downloadEPAResults',"Download EPA Results") )
                                                   
                                                   
                                        ))
                             ))))     