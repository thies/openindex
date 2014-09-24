    # Generate Testdata
    
    # Settings
    # Start and end Date
    from <- as.Date("2000-01-01")
    to <- as.Date("2012-12-31")
    # Total number of homes in market
    populationSize <- 100000
    # Trend in market (e. g. 0.1 = 10% linear growth per year)
    trend <- 0
    # Time between sales, in years
    avTimeBetweenSales <- 10
    # lenght of cycle, in years
    cycleLength <- 20
    # Amplitude of cycle
    cycleAmplitude <- 0.5
    
    # Generate sequence of days
    days <- seq(from, to, 1)
    d <- as.data.frame(days)
    colnames(d)<-"date"
    d$n <- 1:nrow(d)
    d$trend<-(1+trend)^(1/365)-1
    d$trend<-cumsum(d$trend)
    d$trend<-100*(d$trend+1)
    d$cycle <- sin(d$n/(365*cycleLength)*2*pi)
    d$market <- 100 + d$trend + 100*d$cycle*cycleAmplitude
    
  # generate a list of house ids
  houses <- as.data.frame(1:populationSize)
  colnames(houses)<-"id"
    
  # generate sales for each day in the period.
  # the number of sales depends on "avTimeBetweenSales"
  numSalesPerDay <- round(rnorm(nrow(d),(populationSize/365/avTimeBetweenSales),(populationSize/365/avTimeBetweenSales)/3))
  numSalesPerDay[numSalesPerDay < 0] <- 0 
  sales <- list()
  for (i in 1:nrow(d)){
    # draw a sample of houses that are sold
    sales[[i]] <- as.data.frame(houses[sample(1:nrow(houses), numSalesPerDay[i], replace=FALSE),'id'])
    # assume prices to be fair market value plus some random noise
    colnames(sales[[i]])<-"id"
    sales[[i]]$price <- rnorm( numSalesPerDay[i], d$market[i], 20)
    sales[[i]]$date <- rep(d$date[i], numSalesPerDay[i])
  }
  testsales <- do.call("rbind", sales)
  
  plot(d$date,d$market, type="n", xlab="Date", ylab=paste(from,"= 100%"))
  points(testsales$date,testsales$price, cex=0.1, pch=19, col="grey")
  lines(d$date, d$market, col="red")
  write.csv(testsales, file="~/research/openindex/data/testsales.csv", row.names=FALSE)



