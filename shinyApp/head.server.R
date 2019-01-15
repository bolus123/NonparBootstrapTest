check.packages <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}


#########################################################################

packages<-c("moments", "devtools")
check.packages(packages)
install_github('bolus123/PH1AND2XBAR')
install_github('Allanvh/XbarChartWithEstimation', subdir = 'XbarChartWithEstimation')

PH1.get.cc <- PH1AND2XBAR::PH1.get.cc
LadjSp <- XbarChartWithEstimation::LadjSp
LadjSpEPC <- XbarChartWithEstimation::LadjSpEPC

#########################################################################

c4.f <- function(nu) sqrt(2 / nu) * 1 / beta(nu / 2, 1 / 2) * sqrt(pi)             #c4.function

#########################################################################

Statistics <- function(Data){

    m <- dim(Data)[1]
    n <- dim(Data)[2]
    
    x <- as.vector(Data)
    
    skew <- skewness(x)
    kurt <- kurtosis(x)
    
    quantiles <- quantile(x, c(0, 1, 0.01, 0.05, 0.1, 0.2, 0.25, 0.5, 0.75, 0.8, 0.9, 0.95, 0.99))
    sw.norm <- shapiro.test(Data)$p.value
    
    out <- list(
        Metric = c('m', 'n', 'Skewness', 'Kurtosis', 'Max', 'Min', 'Q-0.01', 'Q-0.05', 'Q-0.1', 'Q-0.2', 'Q-0.25', 'Q-0.5'
            , 'Q-0.75', 'Q-0.8', 'Q-0.9', 'Q-0.95', 'Q-0.99', 'Shapiro-Wilk P-value'),
        Value = c(m, n, skew, kurt, quantiles, sw.norm)
    )
    
    return(out)

}


#########################################################################

Ph1ChartStatAndPars <- function(Ph1Data) {

    n <- dim(Ph1Data)[2]

    Ph1ChartStat <- rowMeans(Ph1Data)
    Ph1Mu <- mean(Ph1Data)
    Ph1Sigma2 <- mean(diag(var(t(Ph1Data)))) / n
    
    out <- list(Ph1ChartStat = Ph1ChartStat, Ph1Mu = Ph1Mu, Ph1Sigma2 = Ph1Sigma2)
    
    return(out)
    
}

#########################################################################

ControlChartPlot <- function(Ph1ChartStat, Ph2ChartStat, Ph1Mu, Ph1Sigma2, Ph1CC = 3, Ph2CC = 3) {

    m1 <- length(Ph1ChartStat)
    
    m2 <- length(Ph2ChartStat)
    
    Ph1LowerLimit <- Ph1Mu - Ph1CC * sqrt(Ph1Sigma2)
    Ph1UpperLimit <- Ph1Mu + Ph1CC * sqrt(Ph1Sigma2)
    
    Ph2LowerLimit <- Ph1Mu - Ph2CC * sqrt(Ph1Sigma2)
    Ph2UpperLimit <- Ph1Mu + Ph2CC * sqrt(Ph1Sigma2)
    
    if (length(Ph1LowerLimit) == 1) {
    
        Ph1LowerLimit.vec <- rep(Ph1LowerLimit, m1)
    
    } else {
    
        Ph1LowerLimit.vec <- Ph1LowerLimit
    
    }
    
    if (length(Ph1UpperLimit) == 1) {
    
        Ph1UpperLimit.vec <- rep(Ph1UpperLimit, m1)
    
    } else {
    
        Ph1UpperLimit.vec <- Ph1UpperLimit
    
    }
    
    if (length(Ph2LowerLimit) == 1) {
    
        Ph2LowerLimit.vec <- rep(Ph2LowerLimit, m2)
    
    } else {
    
        Ph2LowerLimit.vec <- Ph2LowerLimit
    
    }
    
    if (length(Ph2UpperLimit) == 1) {
    
        Ph2UpperLimit.vec <- rep(Ph2UpperLimit, m2)
    
    } else {
    
        Ph2UpperLimit.vec <- Ph2UpperLimit
    
    }
    
    plot.max <- max(c(Ph1ChartStat, Ph1LowerLimit.vec, Ph1UpperLimit.vec, Ph2LowerLimit.vec, Ph2UpperLimit.vec, Ph1ChartStat, Ph2ChartStat))
    plot.min <- min(c(Ph1ChartStat, Ph1LowerLimit.vec, Ph1UpperLimit.vec, Ph2LowerLimit.vec, Ph2UpperLimit.vec, Ph1ChartStat, Ph2ChartStat))
    
    ####This part will be replaced by ggplot2
    
    plot(c(1, m1 + m2), c(plot.min, plot.max), type = 'n', xlab = 'Batches', ylab = 'Charting Statistics')
    points(seq(1, m1, 1), Ph1ChartStat, type = 'o', lty = 1)
    points(c(seq(1, m1, 1), m1 + 0.5), c(Ph1LowerLimit.vec, Ph1LowerLimit.vec[m1]), type = 'l', lty = 2)
    points(c(seq(1, m1, 1), m1 + 0.5), c(Ph1UpperLimit.vec, Ph1UpperLimit.vec[m1]), type = 'l', lty = 2)
    
    points(seq(m1, m1 + m2, 1), c(Ph1ChartStat[m1], Ph2ChartStat), type = 'o', lty = 1)
    
    points(c(m1 + 0.5, seq(m1 + 1, m1 + m2, 1)), c(Ph2LowerLimit.vec[1], Ph2LowerLimit.vec), type = 'l', lty = 2)
    points(c(m1 + 0.5, seq(m1 + 1, m1 + m2, 1)), c(Ph2UpperLimit.vec[1], Ph2UpperLimit.vec), type = 'l', lty = 2)
    
    abline(v = m1 + 0.5, lty = 2)
}


#########################################################################



#########################################################################

Ph2ChartStat <- function(Ph2Data) {

    Ph2ChartStat <- rowMeans(Ph2Data)
    
    out <- list(Ph2ChartStat = Ph2ChartStat)
    
    return(out)
    
}