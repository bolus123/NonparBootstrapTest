server <- shinyServer(function(input, output) {

    source('https://raw.githubusercontent.com/bolus123/NonparBootstrapTest/master/shinyApp/head.server.R', local = TRUE)

        ################################################################################################################ 
             
            
            flag.2samp <- reactive({
                
                #if (input$sampOption == "One Sample") {
            
                #    out <- 1
                    
                #} else if (input$sampOption == 'Two Samples') {
                
                    out <- 2
                
                #} #else if (input$sampOption == "One Sample test") {
                #
                #    out <- 1
                #
                #} else if (input$sampOption == "Two Samples test") {
                #
                #    out <- 2
                #
                #} 
                
                return(out)
            
            })
            
            
        ################################################################################################################    
                #Tab: Data
        ################################################################################################################ 
            
            samp <- reactiveValues()
         
            observe({
               
                if (!is.null(input$hotA)) {
                    samp[["previousA"]] <- isolate(samp[["A"]])
                    A <- hot_to_r(input$hotA)
                } else {
                    if (is.null(samp[["A"]])){
                        A <- A
                    }
                    else {
                        A <- samp[["A"]]
                    }
                }
                
                samp[["A"]] <- A
            
                if (!is.null(input$hotB)) {
                    samp[["previousB"]] <- isolate(samp[["B"]])
                    B <- hot_to_r(input$hotB)
                } else {
                    if (is.null(samp[["B"]])){
                        B <- B
                    }
                    else {
                        B <- samp[["B"]]
                    }
                }
                
                samp[["B"]] <- B
         
            })
            
            
            output$hotA <- renderRHandsontable({
              A <- samp[["A"]]
              if (!is.null(A)) rhandsontable(A, stretchH = "all")
            })
            
            output$hotB <- renderRHandsontable({
              B <- samp[["B"]]
              if (!is.null(B)) rhandsontable(B, stretchH = "all")
            })


        ################################################################################################################    
                #Tab: Exploration
        ################################################################################################################

            
            output$SampStat <- renderTable({
              
                sampA <- samp[["A"]]$Sample.A
                sampB <- samp[["B"]]$Sample.B
                
                tbA <- NA
                tbB <- NA
                
                if (flag.2samp() == 1) {
                
                    tbA <- Statistics(sampA)
                
                } else if (flag.2samp() == 2) {
                
                    tbA <- Statistics(sampA)
                    tbB <- Statistics(sampB)
                
                }
                
                #tb <- list(tbA = tbA, tbB = tbB)
                   
                if (flag.2samp() == 1) {
                
                    out <- data.frame(
                        'Sample Metric' = tbA$Metric,
                        'Value' = tbA$Value,
                        stringsAsFactors = FALSE
                    )
                
                } else if (flag.2samp() == 2) {
                
                    out <- data.frame(
                        'Sample A Metric' = tbA$Metric,
                        'Value A' = tbA$Value,
                        'Sample B Metric' = tbB$Metric,
                        'Value B' = tbB$Value,
                        stringsAsFactors = FALSE
                    )
                
                }
                
                return(out)
                
            }, digits = 4)  
            
            output$boxPlot <- renderPlot({
                
                sampA <- samp[["A"]]$Sample.A
                sampB <- samp[["B"]]$Sample.B
                
                if (flag.2samp() == 1) {
                
                    boxplot(sampA)
                    
                } else if (flag.2samp() == 2) {
         
                    boxplot(sampA, sampB, names = c('A', 'B'))
                
                }
                
            })
          
        ################################################################################################################    
                #Tab: Tests
        ################################################################################################################


            tTest <- reactive({
            
                sampA <- samp[["A"]]$Sample.A
                sampB <- samp[["B"]]$Sample.B    
            
                tTestA <- NA
                tTestB <- NA

                
                if (input$alterOption == 'Two-sided') {
                    
                    alter <- 'two.sided'
                
                } else if (input$alterOption == 'Less') {
                
                    alter <- 'less'
                
                } else if (input$alterOption == 'Greater') {
                
                    alter <- 'greater'
                
                }
                
                if (flag.2samp() == 1) {
                
                    out <- t.test(x = sampA, mu = input$Mu0, alternative = alter)
                
                } else if (flag.2samp() == 2) {
                
                    out <- t.test(x = sampA, y = sampB, alternative = alter, var.equal = TRUE)
                
                }
                
                return(out)
                
            })

            bootstrapTest <- reactive({
            
                N <- round(input$bootNum)
                
                sampA <- samp[["A"]]$Sample.A
                sampB <- samp[["B"]]$Sample.B
                
                nA <- length(sampA)
                nB <- length(sampB)
                
                if (flag.2samp() == 1) {     
                
                    tt <- (mean(sampA) - input$Mu0) / sqrt(var(sampA) / nA) 

                    ref <- unlist(
                            lapply(
                                1:N,
                                function(x) {
                                
                                    sampA1 <- sample(sampA, nA, replace = TRUE)
                       
                                    (mean(sampA1) - input$Mu0) / sqrt(var(sampA1) / nA) 
                                    
                                }
                            )
                        )
                
                
                } else if (flag.2samp() == 2) {
                
               
                        tt <- tstat(sampA, sampB)
                
                        AB <- c(sampA, sampB)
                
                        ref <- unlist(
                            lapply(
                                1:N,
                                function(x) {
                                
                                    sampA1 <- sample(AB, nA, replace = TRUE)
           
                                    sampB1 <- sample(AB, nB, replace = TRUE)
                    
                                    tstat(sampA1, sampB1)
                                    
                                }
                            )
                        )
                
                       
                
                }
                
                out <- list(ref = ref, tt = tt)
                
                return(out)
                
            
            })
            

            
            bootstrapTest.stat <- reactive({
            
                N <- round(input$bootNum)
            
                ref <- bootstrapTest()
                
                if (input$alterOption == 'Two-sided') {
                
                    p.value <- sum(abs(ref$ref) > abs(ref$tt)) / N 
                    
                    crit <- quantile(abs(ref$ref), 1 - input$signLvl)
                
                } else if (input$alterOption == 'Less') {
                
                    p.value <- sum(ref$ref < ref$tt) / N
                    
                    crit <- quantile(ref$ref, input$signLvl )
                
                } else if (input$alterOption == 'Greater') {
                
                    p.value <- sum(ref$ref > ref$tt) / N
                    
                    crit <- quantile(ref$ref, 1 - input$signLvl )
                
                }
                
                out <- list(stat = ref$tt, p.value = p.value, ref = ref$ref, crit = crit)
                
                return(out)
            
            })
            
            
            output$testPlot <- renderPlot({
         
                bootstrapTest <- bootstrapTest.stat()
                
                tTest <- tTest()
                
                x.max <- max(bootstrapTest$crit, -bootstrapTest$crit, qt(input$signLvl / 2, tTest$parameter), qt(1 - input$signLvl / 2, tTest$parameter), 3)
                x.min <- min(bootstrapTest$crit, -bootstrapTest$crit, qt(input$signLvl / 2, tTest$parameter), qt(1 - input$signLvl / 2, tTest$parameter), -3)
                
                hist(bootstrapTest$ref, freq = FALSE, xlim = c(x.min, x.max), ylim = c(0, dt(0, tTest$parameter)), col = 'grey', main = "Comparison between Student's t test and Bootstrap Test", xlab = 't statistic')
                curve(dt(x, tTest$parameter), add = TRUE)
                abline(v = tTest$statistic, lty = 2)
                text(x = tTest$statistic, y = dt(0, tTest$parameter) * 3 / 4, paste(round(tTest$statistic, 4)), srt = 270, pos = 4, col = 'black')
               
                if (input$alterOption == 'Two-sided') {
               
                    abline(v = -bootstrapTest$crit, lty = 2, col = 'blue')
                    abline(v = qt(input$signLvl / 2, tTest$parameter), lty = 2, col = 'red')
                    
                    abline(v = bootstrapTest$crit, lty = 2, col = 'blue')
                    abline(v = qt(1 - input$signLvl / 2, tTest$parameter), lty = 2, col = 'red')
                    
                    if (bootstrapTest$crit > qt(1 - input$signLvl / 2, tTest$parameter)) {
                    
                            text(x = -bootstrapTest$crit, y = dt(0, tTest$parameter) * 3 / 4, paste(round(-bootstrapTest$crit, 4)), srt = 90, pos = 2, col = 'blue')
                            text(x = qt(input$signLvl / 2, tTest$parameter), y = dt(0, tTest$parameter) * 3 / 4, paste(round(qt(input$signLvl / 2, tTest$parameter), 4)), srt = 270, pos = 4, col = 'red')
                            
                            text(x = bootstrapTest$crit, y = dt(0, tTest$parameter) * 3 / 4, paste(round(bootstrapTest$crit, 4)), srt = 270, pos = 4, col = 'blue')
                            text(x = qt(1 - input$signLvl / 2, tTest$parameter), y = dt(0, tTest$parameter) * 3 / 4, paste(round(qt(1 - input$signLvl / 2, tTest$parameter), 4)), srt = 90, pos = 2, col = 'red')
                            
                    } else {
                            text(x = -bootstrapTest$crit, y = dt(0, tTest$parameter) * 3 / 4, paste(round(-bootstrapTest$crit, 4)), srt = 270, pos = 4, col = 'blue')
                            text(x = qt(input$signLvl / 2, tTest$parameter), y = dt(0, tTest$parameter) * 3 / 4, paste(round(qt(input$signLvl/ 2, tTest$parameter), 4)), srt = 90, pos = 2, col = 'red')
                            
                            text(x = bootstrapTest$crit, y = dt(0, tTest$parameter) * 3 / 4, paste(round(bootstrapTest$crit, 4)), srt = 90, pos = 2, col = 'blue')
                            text(x = qt(1 - input$signLvl / 2, tTest$parameter), y = dt(0, tTest$parameter) * 3 / 4, paste(round(qt(1 - input$signLvl / 2, tTest$parameter), 4)), srt = 270, pos = 4, col = 'red')
                    
                    }
                   
                   
                } else if (input$alterOption == 'Less') {
               
                    abline(v = bootstrapTest$crit, lty = 2, col = 'blue')
                    abline(v = qt(input$signLvl, tTest$parameter), lty = 2, col = 'red')
                    
                   
                    if (bootstrapTest$crit > qt(input$signLvl, tTest$parameter)) {
                    
                        text(x = bootstrapTest$crit, y = dt(0, tTest$parameter) * 3 / 4, paste(round(bootstrapTest$crit, 4)), srt = 270, pos = 4, col = 'blue')
                        text(x = qt(input$signLvl, tTest$parameter), y = dt(0, tTest$parameter) * 3 / 4, paste(round(qt(input$signLvl, tTest$parameter), 4)), srt = 90, pos = 2, col = 'red')
                    
                    } else {
                        text(x = bootstrapTest$crit, y = dt(0, tTest$parameter) * 3 / 4, paste(round(bootstrapTest$crit, 4)), srt = 90, pos = 2, col = 'blue')
                        text(x = qt(input$signLvl, tTest$parameter), y = dt(0, tTest$parameter) * 3 / 4, paste(round(qt(input$signLvl, tTest$parameter), 4)), srt = 270, pos = 4, col = 'red')
                    }
                   
                   
                } else if (input$alterOption == 'Greater') {
               
                    abline(v = bootstrapTest$crit, lty = 2, col = 'blue')
                    abline(v = qt(1 - input$signLvl, tTest$parameter), lty = 2, col = 'red')
                   
                    
                    if (bootstrapTest$crit > qt(1 - input$signLvl, tTest$parameter)) {
                        text(x = bootstrapTest$crit, y = dt(0, tTest$parameter) * 3 / 4, paste(round(bootstrapTest$crit, 4)), srt = 270, pos = 4, col = 'blue')
                        text(x = qt(1 - input$signLvl, tTest$parameter), y = dt(0, tTest$parameter) * 3 / 4, paste(round(qt(1 - input$signLvl, tTest$parameter), 4)), srt = 90, pos = 2, col = 'red')     
                    
                    } else {
                        text(x = bootstrapTest$crit, y = dt(0, tTest$parameter) * 3 / 4, paste(round(bootstrapTest$crit, 4)), srt = 90, pos = 2, col = 'blue')
                        text(x = qt(1 - input$signLvl, tTest$parameter), y = dt(0, tTest$parameter) * 3 / 4, paste(round(qt(1 - input$signLvl, tTest$parameter), 4)), srt = 270, pos = 4, col = 'red')     
                    }
                   
               
                }

                
            
            })
            
            output$testPvalue <- renderText({
            
                paste('p-value of t test:', round(tTest()$p.value, 4), 'and p-value of bootstrap test:', round(bootstrapTest.stat()$p.value, 4), '\n')
                
            })
            

          
        })