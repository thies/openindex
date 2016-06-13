#========================================================
#
# REPEAT SALES INDEX ESTIMATION
# 
#
# © Thies Lindenthal
# thilin@mit.edu
# <all rights reserved>  
#
# 2016-05-17
# 
# This software is work in progress and 
# comes without any warranty.
# 
# --- As a background, the following papers are helpful ---
#  - Martin J. Bailey, Richard F. Muth, Hugh O. Nourse (1963). "A Regression Method for Real Estate Price Index Construction". Journal of the American Statistical Association, Vol. 58, Iss. 304.
#  - Case, K., & Shiller, R. (1987). "Prices of single family homes since 1970: New indexes for four cities". New England Economic Review: 45–56, Sept./Oct.
#  - Bokhari, S., & Geltner, D. (2012). "Estimating real estate price movements for high frequency tradable indexes in a scarce data environment". The Journal of Real Estate Finance and Economics, 1-22.
#  - Bryan, T., & Colwell, P. (1982). "Housing price indices". In C. F. Sirmans (Ed.), Research in real estate (Vol. 2). Greenwich: JAI Press.
#========================================================

# ==== Load libraries
library(MASS) 


# Index estimation
RepeatSalesIndex <- function( sales,indexFrequency=1, conversionBaseFrequency=NA, method="CaseShiller", baseMethod = 'BaileyMuthNourse', dateMin=NA, dateMax=NA, minDaysBetweenSales=0, maxReturn=NA, minReturn=NA, diagnostics=FALSE, confidence=0.95 ){ 

  # Currently, three estimation methods are supported.
  if(! method %in% c("CaseShiller","BaileyMuthNourse","BokhariGeltner")){
    stop( paste("Estimation Method not supported:",method ) ) 
  }

  Index <- list()
  
  sales$date<-as.Date(sales[,3])

  # apply date filters
  if(!is.na(dateMin)){
    sales<-subset(sales, date >= dateMin)
  }	
  if(!is.na(dateMax)){
    sales<-subset(sales, date <= dateMax)
  }
  # Set the end of each interval period
  months<-seq(from=dateMin, to=(dateMax+27), by="month")
  intervals <-months[seq((1+indexFrequency), length(months), indexFrequency)]-1
  Index[["date"]]<-intervals
  if(intervals[length(intervals)] < dateMax){
    intervals <- c(intervals, dateMax)
  }
  Index[["date"]]<-intervals
  # set the interval for each sale
  sales$interval<-1
  for(i in length(intervals):1){
    sales$interval[sales$date <= intervals[i]]<-i
  }  
    
  # ======== SUMMARY STATISTICS SALES =============
  if(diagnostics){
    # summary statistics for the sales
    Index[["salesSumstats"]]<-summary(sales[,2:3])    
    print("SUMMARY STATISTICS FOR SALES")
    print(Index[["salesSumstats"]])
  }
  Index[['sales']]<-sales
    
  #=========== FIND REPEAT SALES ===============
  # match sales by ID
    
  #pairs<-merge(sales, sales, by="id")
  #pairs<-subset(pairs,date.x < date.y)
  #calculate return between sales
  #pairs$return <- pairs$price.y/pairs$price.x
  #pairs <- subset(pairs, ! is.na( return ) )
  
  pairs <- sales[order(sales$id, sales$date),]
  pairs$id <- as.integer(factor(pairs$id))

  pairs <- pairs[order(pairs$id, pairs$date),]
  
  pairs$price.y[2:nrow(pairs)] <- pairs$price[1:(nrow(pairs)-1)]  
  pairs$id.y[2:nrow(pairs)] <- pairs$id[1:(nrow(pairs)-1)]  
  pairs$interval.y[2:nrow(pairs)] <- pairs$interval[1:(nrow(pairs)-1)]  
  pairs$date.y <- pairs$date
  pairs$date.y[2:nrow(pairs)] <- pairs$date[1:(nrow(pairs)-1)]  
  pairs <- subset(pairs, id == id.y)
  
  pairs$id.y <- NULL
  colnames(pairs) <- c("id", "price.y", "date.y","interval.y","price.x","interval.x","date.x")
    
  # calculate return between sales
  pairs$return<-pairs$price.y/pairs$price.x
  pairs$ln_return <- log(pairs$return)

  print( summary(pairs) )
  
  if(diagnostics){
    topreturns<-subset(pairs,return > quantile(pairs$return, 0.99))
    topreturns<-topreturns[rev(order(topreturns$return)),]
    lowreturns<-subset(pairs,return < quantile(pairs$return, 0.01))
    lowreturns<-lowreturns[order(lowreturns$return),]
    # write lists of top-/low-returns to output
    Index[["lowReturns"]]<-lowreturns
    Index[["highReturns"]]<-topreturns
    
    #print(topreturns)
  }
  # Exclude repeat sales with unreasonably high/low returns (cut-offs defined in settings )
  if(!is.na(maxReturn)){
    pairs<-subset(pairs, return <= maxReturn)
  }
  if(!is.na(minReturn)){
    pairs<-subset(pairs, return >= minReturn)
  }
  
  # Exclude sales where houses were resold very quickly
  if(!is.na(minDaysBetweenSales)){
    pairs<-subset(pairs, as.numeric(date.y-date.x) > minDaysBetweenSales)
  }
  Index[["pairs"]]<- pairs
  

  
  if(diagnostics){
    # Overview of pairs per interval
    tab1<-as.data.frame(table(pairs$interval.x), stringsAsFactors=FALSE)
    tab2<-as.data.frame(table(pairs$interval.y), stringsAsFactors=FALSE)
    tab<-merge(tab1,tab2,by="Var1", all=TRUE)
    colnames(tab)<-c("interval","first_sale","repeat_sale")
    tab<-tab[order(as.numeric(tab$interval)),]
    # summary statistics of
    summary(tab)
    # counts per month for pairs
    Index[["pairsPerInterval"]]<-tab
  }  
  
  
  # ======= BASIC OVERVIEW
  # compare straight index to mean sales prices and median sales prices
  if( method %in% c("CaseShiller","BaileyMuthNourse") ){
    repsales <- estimateIndex( pairs, c( ( dateMin-1 ), intervals ), diagnostics, method, confidence )

    
    index <- as.data.frame( exp( repsales$fit )*100 )
    colnames( index ) <- "estimate"
    index$date <- c(dateMin, intervals)  
    Index[["index"]] <- index


    Index[["index_lower"]] <- exp( repsales$lwr )*100
    Index[["index_upper"]] <- exp( repsales$upr )*100
  }
    
  Index[["mean"]]   <- as.matrix( tapply( sales$price, sales$interval, mean ) )
  Index[["median"]] <- as.matrix( tapply( sales$price, sales$interval, median ) )  
  
  
  # ============== FREQUENCY CONVERSION / LOW TO HIGH FREQUENCY ==================
  # See Bokhari and Geltner (2011) for background on two-stage estimation
  if(method=="BokhariGeltner"){
    
    indices<-list()
    conversion_ratio<- conversionBaseFrequency/indexFrequency
        
    for(i in 1:conversion_ratio){
      # shift dates by one frequency
      intervals_base_shifted <- months[seq((i-1)*indexFrequency+1, length(months), conversionBaseFrequency)]
      # and estimate shifted index
      indices[[i]]<-estimateIndex(pairs, intervals_base_shifted, method=baseMethod, diagnostics, confidence)
    }
    
    if( diagnostics ){
      # visualize the shifted indics (for debugging)
      plot( range( c( dateMin,dateMax )),c(0.5,1.8), type="n", xlab="Time", ylab="Raw base index" )
      annualplots<-matrix(NA, conversion_ratio,(length(indices[[1]]$fit)*conversion_ratio))
      
      for (i in 1:conversion_ratio){
        index <- indices[[ i ]]$fit 
        
        # shift 
        intervals_base_shifted <- months[seq((i-1)*indexFrequency+1, length(months), conversionBaseFrequency)]     
        #index <- exp(index) + (d$market[ d$date == intervals_base_shifted[1] ]/200-1)

        index <- exp(index)        
        
        k <- i
        for ( j in 1:length( index )){
          annualplots[i,k] <- index[ j ]
          k <- k+conversion_ratio
        }
        line<-as.data.frame(rbind(1:ncol( annualplots ), annualplots[i,]))
        line<-line[,!is.na(line[2,])]
        lines(intervals_base_shifted,  index , col="grey")        
        points( intervals_base_shifted , index, col="black")
      }
    }
    # create matrix of stacked returns
    # for second stage of frequency conversion
    stacked_ret <- matrix( 0, nrow=length( indices[[1]]$fit )*conversion_ratio, ncol=(( length( indices[[1]]$fit +1 ))*conversion_ratio) )
    for(i in 2:length( indices[[1]]$fit )){
      for (j in 1:conversion_ratio){
        ii <- (i-2)*conversion_ratio + j
        stacked_ret[ ii, 1 ] <- indices[[j]]$fit[i]-indices[[j]]$fit[i-1]
        stacked_ret[ ii, (ii+1):(ii+conversion_ratio) ] <-rep(1, conversion_ratio)
      }		
    }
    
    stacked_ret<-stacked_ret[ stacked_ret[,1] != 0,]
    stacked_ret<-stacked_ret[ ! is.na(stacked_ret[,1]),]
    stacked_ret <- stacked_ret[ , colSums( abs( stacked_ret ) ) != 0]
        
    #write.csv( stacked_ret, file="~/Downloads/stacked_ret.csv" )
        
    X <- stacked_ret[,2:ncol(stacked_ret)]
        
    # calculate generalized inverse of matrix
    Xginv <- ginv(X)
    # calculate the monthly return coefficients (b)
    b <- Xginv%*%stacked_ret[,1]
    # convert monthly returns to cumulative returns
    b_added <- cumsum( b )
    
    index <- as.data.frame( exp( c( 0, b_added ) )*100 )
    colnames(index) <- "estimate"

    index$date <- c( dateMin, intervals )[1:nrow(index)]
    Index[["index"]] <- index    
  }
  # return index
  return(Index)
}

# ==========================================================
# procedure to estimate an index
estimateIndex <- function( p, int, diagnostics=FALSE, method="CaseShiller", confidence=0.95 ){
  # Currently, only two estimation methods are supported.
  if(! method %in% c("CaseShiller","BaileyMuthNourse")){
    stop( paste("Estimation Method not supported:",method ) ) 
  } 
  
  # only include observations along intervals
  p <- subset(p, date.x >=int[1])
  p <- subset(p, date.y < int[ length(int) ])
  
  
  # Create matrix of yearly time dummies
	dummies<-matrix( 0 , nrow(p) , (length(int)-1) )
	colnames(dummies)<-paste("d", format(int[2:length(int)], format="%Y%m"),sep="")
  
  # loop through the intervals, set dummy variables
	for(m in 1:ncol(dummies)){
    # interval start date
    s <- int[m]
    # interval end date
    e <- int[m+1]
    # interval length, in days
    curr_int_length <-as.numeric( e - s )
    
		# set dummies 1 for any year between first sale and repeat sale
		dummies[ p$date.x <= s , m  ] <- 1
		dummies[ p$date.y <  e , m  ] <- 0	
    
		# set fraction of first and last interval
		# this makes sure we have end of interval returns
		# see Bryon and Colwell (1982) for more information
		dummies[  p$date.x <= e & p$date.x > s  , m ]   <- as.numeric( p$date.x[ p$date.x <= e & p$date.x > s ] - e)*-1/curr_int_length
    dummies[  p$date.y <= e & p$date.y > s  , m ]   <- as.numeric( p$date.y[ p$date.y <= e & p$date.y > s ] - s)/curr_int_length
    dummies[  p$date.x > s & p$date.y <= e  , m ]   <- as.numeric( p$date.y[ p$date.x > s & p$date.y <= e ] - p$date.x[ p$date.x > s & p$date.y <= e ] )/curr_int_length    
	}
  p$days_between_sales <- as.numeric(p$date.y - p$date.x)
  
	p    <- as.data.frame( cbind( subset( p, select=c("ln_return","days_between_sales") ),dummies[,1:ncol( dummies )] ) )
  deps <- paste( colnames( p )[3:ncol( p )], collapse="+" )
	# estimate index with OLS
	reg  <- lm( as.formula( paste( "ln_return~-1+",deps,sep='' ) ), data=p, na.action=na.exclude )  
  
  if(method=="CaseShiller"){    
    # Three step weighted (generalized) least squares
    #  1. BMN regression (already done above)
    #  2. Regression of squared residuals from 1 on constant and time between sales
    #  3. Weight each observation by dividing each obs in 1 by the square root of fitted value in 2 
    #     and run regression again (weighted).

    p$resid_from_first <- residuals( reg )
    p$resid_from_first_sq <- p$resid_from_first*p$resid_from_first
    cs_second_stage <- lm( resid_from_first_sq ~ 1 + days_between_sales , data=p, na.action=na.exclude )
    p$fitted_second_stage <- predict( cs_second_stage )
    p$fitted_second_stage_sqrt <- p$fitted_second_stage^0.5
    p$weights <- 1/p$fitted_second_stage_sqrt
    # estimate index with weighted least squares
    reg  <- lm( as.formula( paste( "ln_return~-1+",deps,sep='' ) ), data=p, weights=p$weights )
    
  }

	# evaluate index  
	mat  <- matrix( 1, ncol( dummies ) , ncol( dummies ) )
	mat[ upper.tri( mat ) ] <- 0
  
	evaluate <- as.data.frame( mat )
	colnames( evaluate )<-colnames( dummies )
	pred <-  as.data.frame( predict.lm( reg, evaluate, interval="confidence", level=confidence ) )
  
  # add base year back 
  pred <- rbind(c(0,0,0), pred)
  
	return( pred )
}
#===================

