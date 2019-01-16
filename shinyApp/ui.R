source('https://raw.githubusercontent.com/bolus123/NonparBootstrapTest/master/shinyApp/head.ui.R', local = TRUE)

    # Define UI for Statistical Performance Analysis (spa)

    ui <- shinyUI(fluidPage(

        
    
      # Application theme
      
        theme = shinytheme("flatly"),
      
        tags$head(tags$style(
                        "#ControlChartWarning{color: red;
                        font-size: 20px;
                        font-style: italic;
                        }"
                )
        ),
      
      # Application title
      
        titlePanel("Nonparametric Bootstrap Test"),

        sidebarLayout(
            sidebarPanel(
              
                    #selectInput("sampOption", "Method Option:", 
                    #      choices = c(
                    #        "One Sample" 
                    #        ,"Two Samples"
                    #        #,"One Sample test"
                    #        #,"Two Samples test"
                    #      )),

              
                    #selectInput("inputOption", "Data Input Option:", 
                    #      choices = c(
                            
                    #        "Manual"
                    #        ,"Excel" 
                    #        ,"Test"
                    #      )),
              
                    selectInput("alterOption", "Alternative Option:", 
                          choices = c("Two-sided", "Less", "Greater")),
              
                    numericInput("signLvl", "Significance Level:", 0.05, min = 1e-6, max = 1 - 1e-6),
              
                    numericInput("bootNum", "Bootstrap Repeatation(Must be integer):", 10000, min = 100)
              
                    #conditionalPanel(
                    #    condition = "input.sampOption == 'One Sample' & input.inputOption == 'Excel'",
                    #    fileInput('sampA', 'Choose a Sample',
                    #        accept = c(".xlsx")),
                    #    numericInput("Mu0", "Target Mean:", 0)    
                    #),
                    
                    #conditionalPanel(
                    #    condition = "input.sampOption == 'Two Samples' & input.inputOption == 'Excel'",
                    #    fileInput('sampA', 'Choose Sample A',
                    #        accept = c(".xlsx")),
                    #    fileInput('sampB', 'Choose Sample B',
                    #        accept = c(".xlsx"))    
                    #)
                       
              
                ),
                

                mainPanel(
                
                    tabsetPanel(type = "tabs", 
                           
                        tabPanel("Data",
                            fluidRow(
                            
                            
                                splitLayout(
                                        cellWidths = c('50%', '50%')
                                        , rHandsontableOutput("hotA")
                                        , rHandsontableOutput("hotB")
                                    )
                            
                                #conditionalPanel(
                                #    condition = "input.sampOption == 'One Sample'",
                                #    rHandsontableOutput("hotA")
                                #    
                                #),
                                
                                #conditionalPanel(
                                #    condition = "input.sampOption == 'Two Samples'",
                                #    splitLayout(
                                #        cellWidths = c('50%', '50%')
                                #        , rHandsontableOutput("hotA")
                                #        , rHandsontableOutput("hotB")
                                #    )
                                #)
                            )                        
                        ),
                        
                           
                        tabPanel("Exploration",
                            fluidRow(
                            
                                splitLayout(
                                    cellWidths = c('50%', '50%')
                                    , tableOutput("SampStat")
                                    , plotOutput("boxPlot")
                                )
                                #textOutput('TEST')
                            
                            )                        
                        ),
                            
                        tabPanel("Test",
                            fluidRow(
                                column(10,
                                    plotOutput("testPlot")                                
                                )        
                            )
                        )
                            
                            
                            
                    )
                
                )   
            
        
        )
    ))