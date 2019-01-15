check.packages <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}


#########################################################################

packages<-c("moments")
check.packages(packages)

#########################################################################

Statistics <- function(Data){

    n <- length(Data)
    
    x <- as.vector(Data)
    
    skew <- skewness(x)
    kurt <- kurtosis(x)
    
    quantiles <- quantile(x, c(0, 1, 0.01, 0.05, 0.1, 0.2, 0.25, 0.5, 0.75, 0.8, 0.9, 0.95, 0.99))
    ks.norm <- ks.test(x, "pnorm")$p.value
    
    out <- list(
        Metric = c('n', 'Skewness', 'Kurtosis', 'Max', 'Min', 'Q-0.01', 'Q-0.05', 'Q-0.1', 'Q-0.2', 'Q-0.25', 'Q-0.5'
            , 'Q-0.75', 'Q-0.8', 'Q-0.9', 'Q-0.95', 'Q-0.99', 'KS P-value'),
        Value = c(n, skew, kurt, quantiles, ks.norm)
    )
    
    return(out)

}


#########################################################################
