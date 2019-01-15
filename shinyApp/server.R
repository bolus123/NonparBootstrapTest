shinyServer(function(input, output) {

  
    source('https://raw.githubusercontent.com/bolus123/NonparBootstrapTest/master/shinyApp/head.server.R', local = TRUE)

   
tstat <- function(x, y){

    nA <- length(x)
    nB <- length(y)
    
    s2A <- var(x)
    s2B <- var(y)
        
    (x - y) / sqrt(((nA - 1) * s2A + (nB - 1) * s2B) / (nA + nB - 2))

}   
   
    
   
    
################################################################################################################ 
    
    sampA <- reactive({

        if (input$sampOption == "One Sample test") {
        
            sampA <- rnorm(49)
        
        } else if (input$sampOption == "One Sample") {
    
            req(input$sampA)
            inFile <- input$sampA
            sampA <- as.vector(read_excel(inFile$datapath, 1))
        }
        
        sampA
        
    })
    
    sampB <- reactive({

        if (input$sampOption == "Two Samples test") {
        
            sampB <- rnorm(52)
        
        } else if (input$sampOption == "Two Samples") {
    
            req(input$sampB)
            inFile <- input$sampB
            sampB <- as.vector(read_excel(inFile$datapath, 1))
            
        }

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

    tTest.1samp <- reactive({
    
        x <- sampA()
        
        if (input$alterOption == 'Two-sided') {
            
            alter <- 'two.sided'
        
        } else if (input$alterOption == 'Less') {
        
            alter <- 'less'
        
        } else if (input$alterOption == 'Greater') {
        
            alter <- 'greater'
        
        }
        
        t.test(x = x, mu = input$Mu0, alternative = alter)
        
    })


    tTest.2samp <- reactive({
    
        x <- sampA()
        y <- sampB()
        
        if (input$alterOption == 'Two-sided') {
            
            alter <- 'two.sided'
        
        } else if (input$alterOption == 'Less') {
        
            alter <- 'less'
        
        } else if (input$alterOption == 'Greater') {
        
            alter <- 'greater'
        
        }
        
        t.test(x = x, y = y, alternative = alter, var.equal = TRUE)
        
    })
    
    bootstrapTest.1samp <- reactive({
    
        N <- round(input$bootNum)
        
        sampA <- sampA()
        
        nA <- length(sampA)
        
        tt <- (sampA - input$Mu0) / sqrt(var(sampA) / nA) 

        ref <- unlist(
                    lapply(
                        1:N,
                        function(x) {
                        
                            x1 <- sample(sampA, nA, replace = TRUE)
               
                            (x1 - input$Mu0) / sqrt(var(x1) / nA) 
                            
                        }
                    )
                )
        
        ref
    
    })
    

    
    bootstrapTest.2samp <- reactive({
    
        N <- round(input$bootNum)
    
        ref <- rep(NA, input$bootNum)
        
        x <- sampA()
        y <- sampB()
        
        nA <- length(x)
        nB <- length(y)
        
        tt <- tstat(x, y)
        
        xy <- c(x, y)
        
        ref <- unlist(
                    lapply(
                        1:N,
                        function(x) {
                        
                            x1 <- sample(xy, nA, replace = TRUE)
   
                            y1 <- sample(xy, nB, replace = TRUE)
            
                            tstat(x1, y1)
                            
                        }
                    )
                )
        
        ref
    
    })
    
    bootstrapTest.stat <- reactive({
    
        if (input$sampOption == 'One Sample') {
        
            ref <- bootstrapTest.1samp()
        
        } else if (input$sampOption == 'Two Samples') {
        
            ref <- bootstrapTest.2samp()
        
        }
        
        if (input$alterOption == 'Two-sided') {
        
            p.value <- sum(abs(tt) > abs(ref)) / N 
            
            crit <- quantile(1 - input$signLvl, abs(ref))
        
        } else if (input$alterOption == 'Less') {
        
            p.value <- sum(tt < ref) / N
            
            crit <- quantile(input$signLvl / 2, ref)
        
        } else if (input$alterOption == 'Greater') {
        
            p.value <- sum(tt > ref) / N
            
            crit <- quantile(1 - input$signLvl / 2, ref)
        
        }
        
        result <- list(stat = tt, p.value = p.value, ref = ref, crit = crit)
        
        return(result)
    
    })
    
    
    
################################################################################################################    
        #Tab: Exploration
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
        #Tab: Tests
################################################################################################################
    
    output$testPlot <- renderPlot({
 
        bootstrapTest <- bootstrapTest.stat()
        
        if (input$sampOption == 'One Sample') {
        
            tTest <- tTest.1samp()
        
        } else if (input$sampOption == 'Two Samples') {
        
            tTest <- tTest.2samp()
        
        }
        
        hist(bootstrapTest$ref)
       #curve(dt(x, tTest$parameter), add = TRUE)
       #abline(v = tTest$statistic)
       #
       #if (input$alterOption == 'Two-sided') {
       #
       #    abline(v = -bootstrapTest$crit, lty = 2, col = 'blue')
       #    abline(v = bootstrapTest$crit, lty = 2, col = 'blue')
       #    abline(v = qt(input$signLvl / 2, tTest$parameter), lty = 2, col = 'red')
       #    abline(v = qt(1 - input$signLvl / 2, tTest$parameter), lty = 2, col = 'red')
       #    
       #} else if (input$alterOption == 'Less') {
       #
       #    abline(v = bootstrapTest$crit, lty = 2, col = 'blue')
       #    abline(v = qt(input$signLvl, tTest$parameter), lty = 2, col = 'red')
       #    
       #} else if (input$alterOption == 'Greater') {
       #
       #    abline(v = bootstrapTest$crit, lty = 2, col = 'blue')
       #    abline(v = qt(1 - input$signLvl, tTest$parameter), lty = 2, col = 'red')
       #
       #}

        
    
    })
    

################################################################################################################


################################################################################################################


################################################################################################################
  
})
