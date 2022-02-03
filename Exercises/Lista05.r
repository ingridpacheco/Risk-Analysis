library(MASS)

### Data Preparation ###

# Find log return from company
generateLogReturn <- function(id) {
    csvName <- "PETR4.SA.csv"
    if (id == "VALE") {
        csvName <- "VALE.csv"
    }
    if (id == "ITUB") {
        csvName <- "ITUB.csv"
    }
    
    csvName <- paste("./Stocks/",csvName,sep="")
    values<-read.csv(csvName,sep=';',dec='.')
    plot(values$Close,type='l')
    lnClose<-log(values$Close)
    lnClose
}

# Calculate mean and variance of log return
getStatistics <- function(lnClose) {
    retCC<-diff(lnClose)
    hist(retCC)
    mu<-mean(retCC)
    sigma<-sd(retCC)
    c(mu,sigma)
}

# Return last value and statistics
getInfo <- function(id) {
    lnClose <- generateLogReturn(id)
    statistics <- getStatistics(lnClose)
    len <- length(lnClose)
    c(lnClose[len],statistics)
}

# Calculate Petrobras
petroLog <- generateLogReturn("PETR4")
petroInfo <- getStatistics(petroLog)

# Calculate Vale
valeLog <- generateLogReturn("VALE")
valeInfo <- getStatistics(valeLog)

# Calculate Itau
itauLog <- generateLogReturn("ITUB")
itauInfo <- getStatistics(itauLog)

# Calculate covariance matrix and return matrix of normal distribution
getCovMatrix <- function(Ns = 1000) {
    plot(petroLog,valeLog,main = paste('var(petro)=',(var(petroLog)),'\n',
                                        'var(vale)=',(var(valeLog)),'\n',
                                        'cov(petro,vale)=',(cov(petroLog,valeLog))))
    
    plot(petroLog,itauLog,main = paste('var(petro)=',(var(petroLog)),'\n',
                                        'var(itau)=',(var(itauLog)),'\n',
                                        'cov(petro,itau)=',(cov(petroLog,itauLog))))
    
    plot(itauLog,valeLog,main = paste('var(itau)=',(var(itauLog)),'\n',
                                        'var(vale)=',(var(valeLog)),'\n',
                                        'cov(itau,vale)=',(cov(itauLog,valeLog))))
    
    petr_var <- var(petroLog)
    vale_var<- var(valeLog)
    itub_var<- var(itauLog)
    x <- cov(petroLog,valeLog)
    y <- cov(petroLog,itauLog)
    z <- cov(itauLog,valeLog)
    
    ma <- matrix(c(petr_var,x,y, x,vale_var,z, z, y,itub_var), ncol = 3)

    # Other way to find covariance matrix
    #M <- cbind(petroLog,valeLog,itauLog)
    #ma <- cov(M)

    x<-mvrnorm(Ns,mu=c(0,0,0),Sigma=ma)
    
    if (Ns > 1){
        plot(x[,1],x[,2],xlab='X',ylab='Y',main='Rho')
        abline(lm(x[,2]~x[,1]),col='red')
    }
    x
}

getCovMatrix()

### Result Analysis ###

## First Analysis ##

q1 <- function(id, days, Ns=1000) {
    stock <- getInfo(id)
    today <- stock[1]
    mu <- stock[2]
    sigma <- stock[3]
    retEst<-vector(length = days)
    valEndDay<-vector(length = Ns)
    for(j in 1:Ns){
        retEst[1]<-today
        for ( i in 2:days){
        retEst[i]<-retEst[i-1]+rnorm(1,mu,sigma)
        }
        #in this case, save only last element of path
        valEndDay[j]<-retEst[days]
    }
    
    priceEndDay<-exp(valEndDay)
    hist(priceEndDay)
    plot(ecdf(priceEndDay))
}

# Calculate the probability distribution of the values of Petrobras after 30 days
q1("PETR4",30)

# Calculate the probability distribution of the values of Vale after 30 days
q1("VALE",30)

# Calculate the probability distribution of the values of Itau after 30 days
q1("ITUB",30)

## Second Analysis ##

q2 <- function(qP,qV,qI,days,Ns=1000) {
    petro <- getInfo("PETR4")
    petroValue <- petro[1]
    
    vale <- getInfo("VALE")
    valeValue <- vale[1]
    
    itau <- getInfo("ITUB")
    itauValue <- itau[1]
    
    retEst<-vector(length = days)
    valEndDay<-vector(length = Ns)
    mn <- getCovMatrix(Ns)
    
    for(j in 1:Ns){
        print(j)
        retEst[1] <- qP * petroValue + qV * valeValue + qI * itauValue
        for ( i in 2:days){
        newValue <- qP * mn[j,1] + qV * mn[j,2] + qI * mn[j,3]
        retEst[i]<-retEst[i-1]+newValue
        }
        #in this case, save only last element of path
        valEndDay[j]<-retEst[days]
    }
    
    priceEndDay<-exp(valEndDay)
    hist(priceEndDay)
    plot(ecdf(priceEndDay))
}

# Calculate the probability distribution of the values of the wallet after 30 days
q2(1,1,1,30)