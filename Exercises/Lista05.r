generateInfo <- function(id) {
    csvName <- "PETR4.SA.csv"
    if (id == "VALE") {
        csvName <- "VALE.csv"
    }
    if (id == "ITUB") {
        csvName <- "ITUB.csv"
    }
    csvName <- paste("./Stocks/",csvName,sep="")
    values<-read.csv(csvName,sep=';',dec=',')
    plot(values$Close,type='l')
    lnClose<-log(values$Close)
    retCC<-diff(lnClose)
    hist(retCC)
    mu<-mean(retCC)
    sigma<-sd(retCC)
    c(lnClose[1],mu,sigma)
}

# Calculate Petrobras
petro <- generateInfo("PETR4")

# Calculate Vale
vale <- generateInfo("VALE")

# Calculate Itau
Itau <- generateInfo("ITUB")

q1 <- function(id, day, Ns=1000) {
    stock <- generateInfo(id)
    today <- stock[1]
    mu <- stock[2]
    sigma <- stock[3]
    print(mu)
    print(sigma)
    retEst<-vector(length = day)
    valEndDay<-vector(length = Ns)
    for(j in 1:Ns){
        retEst[1]<-today
        for ( i in 2:day){
        retEst[i]<-retEst[i-1]+rnorm(1,mu,sigma)
        }
        #in this case, save only last element of path
        valEndDay[j]<-retEst[day]
    }
    
    priceEndDay<-exp(valEndDay)
    hist(priceEndDay)
    plot(ecdf(priceEndDay))
}