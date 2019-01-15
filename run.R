check.packages <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}


###########################################################################################

packages<-c("shiny")
check.packages(packages)

###########################################################################################

options(shiny.port = 38383)

options(shiny.host = "127.0.0.1")

runGitHub('bolus123/PH1ANDPH2App', subdir = 'shinyApp')
