check.packages <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}


###########################################################################################

packages<-c("shiny", "rhandsontable")
check.packages(packages)

###########################################################################################

guineaPig.App <- function(A, B){

    ## UI
    source('https://raw.githubusercontent.com/bolus123/NonparBootstrapTest/master/shinyApp/ui.R', local = TRUE)
    
    ## Server
    source('https://raw.githubusercontent.com/bolus123/NonparBootstrapTest/master/shinyApp/server.R', local = TRUE)    
      
    ## run app 
    runApp(list(ui = ui, server = server))
    return(invisible())
}

###########################################################################################

options(shiny.port = 38383)

options(shiny.host = "127.0.0.1")

###########################################################################################

A <- data.frame( 'Sample A' = c(
    576
    ,650
    ,216
    ,173
    ,165
    ,608
    ,102
    ,603
    ,725
    ,688
    ,455
    ,87
    ,178
    ,685
    ,421
    ,641
    ,621
    ,120
    ,432
    ,421
    ,367
    ,166
    ,18
    ,167
    ,212
    ,114
    ,278
    ,209
    ,160
    ,86
    ,634
    ,91
    ,463
    ,108
    ,446
    ,663
    ,119
    ,118
    ,590
    ,607
    ,545
    ,167
    ,273
    ,279
    ,341
    ,638
    ,474
    ,52
    ,505
    ,89
    ,569
    ,292
    ,189
    ,735
    ,637
    ,382
    ,36
    ,149
    ,50
    ,380
    ,114
    ,355
    ,546
    ,115
), stringsAsFactors = FALSE)

B <- data.frame('Sample B' = c(
    178
    ,185
    ,119
    ,244
    ,213
    ,139
    ,152
    ,326
    ,168
    ,225
    ,256
    ,373
    ,398
    ,220
    ,216
    ,268
    ,179
    ,93
    ,361
    ,198
    ,406
    ,225
    ,315
    ,598
    ,376
    ,166
    ,194
    ,76
    ,164
    ,136
    ,326
    ,373
    ,97
    ,270
    ,183
    ,311
    ,181
    ,466
    ,283
    ,259
    ,108
    ,291
    ,154
    ,397
    ,138
    ,212
    ,164
    ,181
    ,160
    ,107
    ,253
    ,114
    ,154
    ,459
    ,289
    ,265
    ,113
    ,592
), stringsAsFactors = FALSE)

###########################################################################################

guineaPig.App(A, B)