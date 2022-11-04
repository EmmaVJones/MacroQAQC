shinyUI(fluidPage(theme= "yeti.css",
                  navbarPage("Benthic Data QAQC Tool",
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
                                        h4(strong('Note: Template updated November 2022! Please download a new template version in order to use app.')),
                                        downloadButton('downloadTemplate',"Download template.csv"),
                                        width = 3),
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel('Data Precheck',
                                                   fluidRow(
                                                     column(5,h4("Uploaded Stations"),
                                                            helpText("This table shows all stations uploaded to the application, as R understands it."),
                                                            DT::dataTableOutput('uploadedStationTable')),
                                                     column(7,h4("Potential Issues"),
                                                            helpText('These stations have problems that need to be resolved before they can
                                                                     be used for any QA analyses. These stations will be removed from further
                                                                     analyses.'),
                                                            DT::dataTableOutput('issuesTable'))),
                                                   br(),
                                                   h4("Stations For QA Analyses"),
                                                   helpText("This table shows all stations that can proceed with QA analyses."),
                                                   DT::dataTableOutput('stationsForQATable'), br(),br(),br()),
                                          tabPanel('Data Retrieved',
                                                   h4("Retrieved Benthic Data"),
                                                   DT::dataTableOutput('benthicData'),
                                                   hr(), 
                                                   h4('CEDS QA Assistance'),
                                                   helpText('This table identifies any CEDS data entry errors and potential solutions to resolve the issues.
                                                            Until the CEDS data entry errors are fixed, these stations will not be analyzed for QA metrics.'),
                                                   DT::dataTableOutput('CEDSdataEntryTable'), 
                                                   hr(),
                                                   h4("Benthic Data For QA"),
                                                   helpText('This benthic data passes all initial data quality checks and can proceed with QA metric analyses.'),
                                                   DT::dataTableOutput('benthicDataForQA'),
                                                   br(),br(),br()),
                                          tabPanel("QA Metrics",
                                                   h4('QA Sample Summary'),
                                                   DT::dataTableOutput('QAStationTable'),
                                                   hr(),
                                                   h4("DEQ Internal QA Results"),
                                                   DT::dataTableOutput('DEQresults'),
                                                   br(),
                                                   h4("EPA QA Results"),
                                                   DT::dataTableOutput('EPAresults'),
                                                   br(),
                                                   h4("DEQ QA vs EPA QA Results"),
                                                   DT::dataTableOutput('DEQQAvsEPAresults'),br(),br(),br() ),
                                          tabPanel("QA Results",
                                                   #verbatimTextOutput('testtest'),
                                                   uiOutput('stationSelectionUI'),
                                                   h4('DEQ Taxa Comparison'),
                                                   DT::dataTableOutput('DEQstationLineup'),
                                                   DT::dataTableOutput('DEQstationLineupMetrics'),
                                                   hr(),
                                                   h4('EPA Taxa Comparison'),
                                                   DT::dataTableOutput('EPAstationLineup'),
                                                   DT::dataTableOutput('EPAstationLineupMetrics'), 
                                                   hr(),
                                                   h4('DEQ QA vs EPA Taxa Comparison'),
                                                   DT::dataTableOutput('DEQQAvsEPAstationLineup'),
                                                   DT::dataTableOutput('DEQQAvsEPAstationLineupMetrics'),br(),br(),br()),
                                          tabPanel("Download Results",
                                                   helpText('Click the below button to download a MS Excel Workbook of all
                                                            DEQ QA metrics and QA results.'),
                                                   downloadButton('downloadDEQResults',"Download DEQ Results"),
                                                   helpText('Click the below button to download a MS Excel Workbook of all
                                                            EPA QA metrics and QA results.'),
                                                   downloadButton('downloadEPAResults',"Download EPA Results"), 
                                                   helpText('Click the below button to download a MS Excel Workbook of all
                                                            DEQ QA vs EPA QA metrics and QA results.'),
                                                   downloadButton('downloadDEQQAvsEPAResults',"Download  DEQ QA vs EPA Results"), br(),br(),br() )
                                          ))),
                             tabPanel('How To', htmlOutput("BenthicDataQAQCToolHowTo") )
                  )))     