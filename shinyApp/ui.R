source('https://raw.githubusercontent.com/bolus123/PH1ANDPH2App/master/shinyApp/head.ui.R', local = TRUE)

# Define UI for Statistical Performance Analysis (spa)

shinyUI(fluidPage(

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
  
    titlePanel("Phase I and Phase II Control Charts"),

    sidebarLayout(
        sidebarPanel(
          
                selectInput("Ph1cc", "Phase I Control Chart:", 
                      choices = c("Basic Phase I X-bar Chart", "Basic Ph I Testing")),
                      #choices = c("Basic Phase I X-bar Chart")),
          
                fileInput('Ph1Data', 'Choose Phase I Data',
                    accept = c(".xlsx")),
                    
                    
                #######################################################################################    
                #numericInput("FAP", "Ph1 False Alarm Probability", 0.05, min = 0, max = 1),
                #######################################################################################
                conditionalPanel(
                    condition = "input.Ph1cc == 'Basic Phase I X-bar Chart'",
                    numericInput("FAP", "Ph1 False Alarm Probability", 0.05, min = 0, max = 1)
                ),
                
                conditionalPanel(
                    condition = "input.Ph1cc == 'Basic Ph I Testing'",
                    numericInput("Ph1testNormMu", "Ph1 Test parameters mu", 0),
                    numericInput("Ph1testNormSigma2", "Ph1 Test parameters sigma2", 1, min = 1e-6),
                    numericInput("Ph1testBatches", "Ph1 Test parameters batches", 10, min = 1),
                    numericInput("Ph1testSampleSize", "Ph1 Test parameters sample size", 5, min = 2),
                    numericInput("Ph1testCC", "Ph1 Test Charting constant", 3, min = 1e-6)
                ),
              
                #######################################################################################
          
                selectInput("Ph2cc", "Phase II Control Chart:", 
                      choices = c("Basic Phase II X-bar Chart(UC)", "Basic Phase II X-bar Chart(EPC)", "Basic Ph II Testing")),
                #        choices = c("Basic Phase II X-bar Chart")),
          
                fileInput('Ph2Data', 'Choose Phase II Data',
                    accept = c(".xlsx")),
                
                ####################################################################################### 
                conditionalPanel(
                    condition = "input.Ph2cc == 'Basic Phase II X-bar Chart(UC)'",
                    numericInput("ARL0", "Ph2 Nominal In-control Average Run Length", 370, min = 1)
                ),                
                
                conditionalPanel(
                    condition = "input.Ph2cc == 'Basic Phase II X-bar Chart(EPC)'",
                    numericInput("ARL0", "Ph2 Nominal In-control Average Run Length", 370, min = 1),
                    numericInput("Eps", "Tolerance Factor", 0.1, min = 0, max = 1-1e-6),
                    numericInput("P", "Significance Level", 0.05, min = 1e-6, max = 1)
                ),  
                
                conditionalPanel(
                    condition = "input.Ph2cc == 'Basic Ph II Testing'",
                    numericInput("Ph2testNormMu", "Ph2 Test parameters mu", 0),      
                    numericInput("Ph2testNormSigma2", "Ph2 Test parameters sigma2", 1, min = 1e-6),
                    numericInput("Ph2testBatches", "Ph2 Test parameters batches", 10, min = 1),
                    numericInput("Ph2testSampleSize", "Ph2 Test parameters sample size", 5, min = 2),
                    numericInput("Ph2testCC", "Ph2 Test Charting constant", 3, min = 1e-6)
                )
                
                #######################################################################################
          
            ),
            

            mainPanel(
            
                tabsetPanel(type = "tabs", 
                        
                    tabPanel("Exploration",
                        fluidRow(
                            column(6,
                                tableOutput("Ph1stat")
                            ),
                            column(6,
                                tableOutput("Ph2stat")
                            ),
                            column(6,
                                plotOutput("boxplot1")
                            ),
                            column(6,
                                plotOutput("boxplot2")
                            ),
                            column(6,
                                plotOutput("qqplot1")
                            ),
                            column(6,
                                plotOutput("qqplot2")
                            )
                        )
                    ),
                        
                    tabPanel("Monitoring",
                        fluidRow(
                            column(12,
                                plotOutput("ControlChart"),
                                textOutput("ControlChartWarning")
                                
                            )        
                        )
                    ),
                        
                    tabPanel("Performance",
                        fluidRow(
                            column(6,
                                plotOutput("plot2")
                            ),
                            column(6,
                                plotOutput("plot3")
                            ),
                            column(6,
                                plotOutput("plot4")
                            ),
                            column(6,
                                plotOutput("plot5")
                            )   
                                    
                        )   
                    )
                        
                        
                        
                )
            
            )   
        
    
    )
))
      
      

      
      

