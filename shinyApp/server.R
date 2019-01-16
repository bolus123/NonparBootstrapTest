shinyServer(function(input, output) {

  
    source('https://raw.githubusercontent.com/bolus123/NonparBootstrapTest/master/shinyApp/head.server.R', local = TRUE)

   
    tstat <- function(x, y, local = TRUE){

        nA <- length(x)
        nB <- length(y)
    
        s2A <- var(x)
        s2B <- var(y)
        
        x.bar <- mean(x)
        y.bar <- mean(y)
        
        (x.bar - y.bar) / sqrt(var(c(x, y)) * (1 / nA + 1/nB))

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
        x <- sampA()
        result <- Statistics(x)
        
        result 
    })
    
    tbB <-  reactive({
        x <- sampB()
        result <- Statistics(x)
        
        result
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
        
        tt <- (mean(sampA) - input$Mu0) / sqrt(var(sampA) / nA) 

        ref <- unlist(
                    lapply(
                        1:N,
                        function(x) {
                        
                            x1 <- sample(sampA, nA, replace = TRUE)
               
                            (mean(x1) - input$Mu0) / sqrt(var(x1) / nA) 
                            
                        }
                    )
                )
        
        result <- list(ref = ref, tt = tt)
        
        return(result)
    
    })
    

    
    bootstrapTest.2samp <- reactive({
    
        N <- round(input$bootNum)
        
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
        
        result <- list(ref = ref, tt = tt)
        
        return(result)
    
    })
    
    bootstrapTest.stat <- reactive({
    
        if (input$sampOption == 'One Sample') {
        
            ref <- bootstrapTest.1samp()
        
        } else if (input$sampOption == 'Two Samples') {
        
            ref <- bootstrapTest.2samp()
        
        }
        
        if (input$alterOption == 'Two-sided') {
        
            p.value <- sum(abs(ref$tt) > abs(ref$ref)) / N 
            
            crit <- quantile(abs(ref$ref), 1 - input$signLvl)
        
        } else if (input$alterOption == 'Less') {
        
            p.value <- sum(ref$tt < ref$ref) / N
            
            crit <- quantile(ref$ref, input$signLvl / 2)
        
        } else if (input$alterOption == 'Greater') {
        
            p.value <- sum(ref$tt > ref$ref) / N
            
            crit <- quantile(ref$ref, 1 - input$signLvl / 2)
        
        }
        
        result <- list(stat = ref$tt, p.value = p.value, ref = ref$ref, crit = crit)
        
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

    }, digits = 4)
    
    output$SampBStat <- renderTable({

        tb <- tbB()
        
        data.frame(
            'Sample B Metric' = tb$Metric,
            Value = tb$Value,
            stringsAsFactors = FALSE
        )

    }, digits = 4)
    
    
    output$boxPlot <- renderPlot({
        
        x <- sampA()
        
        if (input$sampOption == 'One Sample') {
            boxplot(x, horizontal = TRUE)
        } else if (input$sampOption == 'Two Samples') {
            y <- sampB()
            boxplot(x, y, horizontal = TRUE)
        }
        
    
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
        
       hist(bootstrapTest$ref, freq = FALSE, xlim = c(-3, 3), ylim = c(0, dt(0, tTest$parameter)), col = 'grey')
       curve(dt(x, tTest$parameter), add = TRUE)
       abline(v = tTest$statistic, lty = 2)
       text(x = tTest$statistic, y = dt(0, tTest$parameter) / 2, paste(round(tTest$statistic, 4)), srt = 270, pos = 4, col = 'black')
       
       if (input$alterOption == 'Two-sided') {
       
           abline(v = -bootstrapTest$crit, lty = 2, col = 'blue')
           text(x = -bootstrapTest$crit, y = dt(0, tTest$parameter) / 2, paste(round(-bootstrapTest$crit, 4)), srt = 270, pos = 4, col = 'blue')
           
           abline(v = bootstrapTest$crit, lty = 2, col = 'blue')
           text(x = bootstrapTest$crit, y = dt(0, tTest$parameter) / 2, paste(round(bootstrapTest$crit, 4)), srt = 90, pos = 2, col = 'blue')
           
           abline(v = qt(input$signLvl / 2, tTest$parameter), lty = 2, col = 'red')
           text(x = qt(input$signLvl / 2, tTest$parameter), y = dt(0, tTest$parameter) / 2, paste(round(qt(0.05/ 2, tTest$parameter), 4)), srt = 90, pos = 2, col = 'red')
           
           abline(v = qt(1 - input$signLvl / 2, tTest$parameter), lty = 2, col = 'red')
           text(x = qt(1 - input$signLvl / 2, tTest$parameter), y = dt(0, tTest$parameter) / 2, paste(round(qt(1 - 0.05 / 2, tTest$parameter), 4)), srt = 270, pos = 4, col = 'red')
           
       } else if (input$alterOption == 'Less') {
       
           abline(v = bootstrapTest$crit, lty = 2, col = 'blue')
           text(x = bootstrapTest$crit, y = dt(0, tTest$parameter) / 2, paste(round(bootstrapTest$crit, 4)), srt = 90, pos = 2, col = 'blue')
           
           abline(v = qt(input$signLvl, tTest$parameter), lty = 2, col = 'red')
           text(x = qt(input$signLvl, tTest$parameter), y = dt(0, tTest$parameter) / 2, paste(round(qt(input$signLvl, tTest$parameter), 4)), srt = 270, pos = 4, col = 'red')
           
       } else if (input$alterOption == 'Greater') {
       
           abline(v = bootstrapTest$crit, lty = 2, col = 'blue')
           text(x = bootstrapTest$crit, y = dt(0, tTest$parameter) / 2, paste(round(bootstrapTest$crit, 4)), srt = 270, pos = 4, col = 'blue')
           
           abline(v = qt(1 - input$signLvl, tTest$parameter), lty = 2, col = 'red')
           text(x = qt(1 - input$signLvl, tTest$parameter), y = dt(0, tTest$parameter) / 2, paste(round(qt(1 - input$signLvl, tTest$parameter), 4)), srt = 90, pos = 2, col = 'red')
           
       
       }

        
    
    })
    

################################################################################################################


################################################################################################################


################################################################################################################
  
})
