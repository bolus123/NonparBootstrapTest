shinyServer(function(input, output) {

  
    source('https://raw.githubusercontent.com/bolus123/PH1ANDPH2App/master/shinyApp/head.server.R', local = TRUE)

    
################################################################################################################ 

    #Ph1Data <- reactive({
    #    req(input$Ph1Data)
    #    inFile <- input$Ph1Data
    #    as.matrix(read_excel(inFile$datapath, 1))
    #})

    Ph1Dimension <- reactive({
        dim(Ph1Data())
    })
    
    Ph1Data <- reactive({
        if (input$Ph1cc == 'Basic Ph I Testing') {
            Ph1Data <- rnorm(input$Ph1testBatches * input$Ph1testSampleSize, input$Ph1testNormMu, sqrt(input$Ph1testNormSigma2))
            Ph1Data <- matrix(Ph1Data, ncol = input$Ph1testSampleSize, nrow = input$Ph1testBatches)
        } else {
            req(input$Ph1Data)
            inFile <- input$Ph1Data
            Ph1Data <- as.matrix(read_excel(inFile$datapath, 1))
        }
        
        Ph1Data
        
    })
    
    Ph2Data <- reactive({
        if (input$Ph2cc == 'Basic Ph II Testing') {
            Ph2Data <- rnorm(input$Ph2testBatches * input$Ph2testSampleSize, input$Ph2testNormMu, sqrt(input$Ph2testNormSigma2))
            Ph2Data <- matrix(Ph2Data, ncol = input$Ph2testSampleSize, nrow = input$Ph2testBatches)
        } else {
            req(input$Ph2Data)
            inFile <- input$Ph2Data
            Ph2Data <- as.matrix(read_excel(inFile$datapath, 1))
        }
        
        Ph2Data
        
    })
 
################################################################################################################
    
    tb1 <- reactive({
        Statistics(Ph1Data())
    })
    
    tb2 <-  reactive({
        Statistics(Ph2Data())
    })
    
    output$Ph1stat <- renderTable({

        tb <- tb1()
        
        data.frame(
            'Phase I Metric' = tb$Metric,
            Value = tb$Value,
            stringsAsFactors = FALSE
        )

    }, digits = 5)
    
    output$Ph2stat <- renderTable({

        tb <- tb2()
        
        data.frame(
            'Phase II Metric' = tb$Metric,
            Value = tb$Value,
            stringsAsFactors = FALSE
        )

    }, digits = 5)
    
    output$boxplot1 <- renderPlot({
        x <- as.vector(Ph1Data())
        boxplot(x)
    })
    
    output$boxplot2 <- renderPlot({
        x <- as.vector(Ph2Data())
        boxplot(x)
    })
    
    output$qqplot1 <- renderPlot({
        x <- as.vector(Ph1Data())
        qqnorm(x)
        qqline(x)
    })
    
    output$qqplot2 <- renderPlot({
        x <- as.vector(Ph2Data())
        qqnorm(x)
        qqline(x)
    })
  
################################################################################################################ 
  
  
    Ph1.cc <- reactive({
    
        d <- Ph1Dimension()
    
        if (input$Ph1cc == 'Basic Phase I X-bar Chart') {
            Ph1.cc <- PH1.get.cc(
                    d[1], 
                    d[1] * (d[2] - 1), 
                    input$FAP
                  )$c.i
        } else if (input$Ph1cc == 'Basic Ph I Testing') {
        
            Ph1.cc <- input$Ph1testCC
        
        }
    
        Ph1.cc
    
    })
    
    Ph2.cc <- reactive({
    
        d <- Ph1Dimension()
    
        if (input$Ph2cc == 'Basic Phase II X-bar Chart(UC)') {
        
            Ph2.cc <- LadjSp(input$ARL0, d[1], d[2])
            
        } else if (input$Ph2cc == 'Basic Phase II X-bar Chart(EPC)') {
        
            Ph2.cc <- LadjSpEPC(input$ARL0, input$Eps, input$P, d[1], d[2])
            
        } else if (input$Ph2cc == 'Basic Ph II Testing') {
        
            Ph2.cc <- input$Ph2testCC
            
        }
        
        Ph2.cc

    })
  

    output$ControlChart <- renderPlot({
        
        Ph1Obj <- Ph1ChartStatAndPars(Ph1Data())
        
        Ph2Obj <- Ph2ChartStat(Ph2Data())
        
        Ph1.cc <- Ph1.cc()
        
        Ph2.cc <- Ph2.cc()        
        
        ControlChartPlot(
            Ph1ChartStat = Ph1Obj$Ph1ChartStat, 
            Ph2ChartStat = Ph2Obj$Ph2ChartStat, 
            Ph1Mu = Ph1Obj$Ph1Mu, 
            Ph1Sigma2 = Ph1Obj$Ph1Sigma2, 
            Ph1CC = Ph1.cc, 
            Ph2CC = Ph2.cc
        )

    })
    
    output$ControlChartWarning <- renderText({
        tb1 <- tb1()
        tb2 <- tb2()
        
        mes <- NULL
        
        if (tail(tb1$Value, 1) <= 0.05) {
            
            if (tail(tb2$Value, 1) <= 0.05) {
            
                mes <- '*Both of your Phase I and II data may be not from normal distributions'
            
            } else {
            
                mes <- '*Your Phase I data may be not from a normal distribution' 
           
            }
        
        } else if (tail(tb2$Value, 1) <= 0.05) {
        
            mes <- '*Your Phase II data may be not from a normal distribution' 
        
        } 
        
        mes
        
    })
    
################################################################################################################    
    
    
################################################################################################################
    
    output$plot2 <- renderPlot({
        x <- rgamma(100, 3, 4)
        hist(x, col = 'darkgray', border = 'white')
    
    })
    
    output$plot3 <- renderPlot({
        x <- rt(100, 2)
        hist(x, col = 'darkgray', border = 'white')
    
    })
    
    output$plot4 <- renderPlot({
        x <- rbeta(100, 3, 4)
        hist(x, col = 'darkgray', border = 'white')
    
    })
    
    output$plot5 <- renderPlot({
        x <- rexp(100, 5)
        hist(x, col = 'darkgray', border = 'white')
    
    })
    
################################################################################################################

        
    output$text2 <- renderText('test2')
    output$text3 <- renderText('test3')
    output$text4 <- renderText('test4')
    output$text5 <- renderText('test5')
    output$text6 <- renderText('test6')
  
})
