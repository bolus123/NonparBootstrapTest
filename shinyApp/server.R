shinyServer(function(input, output) {

  
    source('https://raw.githubusercontent.com/bolus123/NonparBootstrapTest/master/shinyApp/head.server.R', local = TRUE)

    
################################################################################################################ 
    
    sampA <- reactive({

        req(input$sampA)
        inFile <- input$sampA
        sampA <- as.matrix(read_excel(inFile$datapath, 1))
        
        sampA
        
    })
    
    sampB <- reactive({

        req(input$sampB)
        inFile <- input$sampB
        sampB <- as.matrix(read_excel(inFile$datapath, 1))

        sampB
        
    })
    
################################################################################################################
    
    tbA <- reactive({
        Statistics(sampA())
    })
    
    tbB <-  reactive({
        Statistics(sampB())
    })
    
################################################################################################################    
    
    output$SampAStat <- renderTable({

        tb <- tbA()
        
        data.frame(
            'Sample A Metric' = tb$Metric,
            Value = tb$Value,
            stringsAsFactors = FALSE
        )

    }, digits = 5)
    
    output$SampBStat <- renderTable({

        tb <- tbB()
        
        data.frame(
            'Sample B Metric' = tb$Metric,
            Value = tb$Value,
            stringsAsFactors = FALSE
        )

    }, digits = 5)
    
    output$boxplotA <- renderPlot({
        x <- as.vector(sampA())
        boxplot(x)
    })
    
    output$boxplotB <- renderPlot({
        x <- as.vector(sampB())
        boxplot(x)
    })
  
################################################################################################################


################################################################################################################


################################################################################################################


################################################################################################################
  
})
