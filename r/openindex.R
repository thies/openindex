#========================================================
#
# REPEAT SALES INDEX ESTIMATION
# 
#
# © Thies Lindenthal
# htl24@cam.ac.uk	
#
# 2016-05-17
# 
# This software is work in progress and 
# comes without ANY warranty.
# 
# --- As a background, the following papers are helpful ---
#  - Martin J. Bailey, Richard F. Muth, Hugh O. Nourse (1963). "A Regression Method for Real Estate Price Index Construction". Journal of the American Statistical Association, Vol. 58, Iss. 304.
#  - Case, K., & Shiller, R. (1987). "Prices of single family homes since 1970: New indexes for four cities". New England Economic Review: 45–56, Sept./Oct.
#  - Bokhari, S., & Geltner, D. (2012). "Estimating real estate price movements for high frequency tradable indexes in a scarce data environment". The Journal of Real Estate Finance and Economics, 1-22.
#  - Bryan, T., & Colwell, P. (1982). "Housing price indices". In C. F. Sirmans (Ed.), Research in real estate (Vol. 2). Greenwich: JAI Press.
#========================================================


setwd("~/git/openindex/")

# ==== LOAD DATA
# Expected input: CSV sheet containing information on:
#     - Property identifier. ID can be a number or a set of strings or a combination of both.
#     - Sales price
#     - Date of sale. Format: yyyy-mm-dd
# (Script to generate test data provided in repository)
sales <- read.csv("data/testsales.csv",as.is=TRUE)
colnames(sales)<-c("id","price","date")
# order by date, ascending
sales$date<-as.Date(sales$date)
sales<-sales[order(sales$date),]


# load the index estimation script
source("r/openindex_includes.R")

# estimate Index
BMN <- RepeatSalesIndex(sales,indexFrequency=1, method="BaileyMuthNourse", dateMin=as.Date("2000-01-01"), dateMax=as.Date("2017-10-31"), minDaysBetweenSales=1, maxReturn=NA, minReturn=NA, diagnostics=TRUE)
BG <- RepeatSalesIndex(sales,indexFrequency=1, conversionBaseFrequency=12, method="BokhariGeltner", dateMin=as.Date("2000-01-01"), dateMax=as.Date("2017-10-31"), minDaysBetweenSales=1, maxReturn=NA, minReturn=NA, diagnostics=TRUE)

plot(BG$index$date, BG$index$estimate, type="n", xlab="Time", ylab="Index")
lines(BG$index$date, BG$index$estimate, col="blue", lwd=3)
lines(BMN$index$date, BMN$index$estimate, col="red", lwd=3)
legend("bottomleft",c("Frequency conversion, 12:1 (BG)","Repeat sales (BMN)"), lwd=3, col=c("blue","red"))


# CS <- RepeatSalesIndex(sales,indexFrequency=3, method="CaseShiller", dateMin=as.Date("2000-01-01"), dateMax=as.Date("2012-12-31"), minDaysBetweenSales=1, maxReturn=NA, minReturn=NA, diagnostics=TRUE)




check <- as.data.frame(as.Date(myIndex$date))




colnames(check)<-"date"
check$mean <- myIndex$mean
check$index <- myIndex$RepeatSalesIndexUnsmoothed
check$upper <- myIndex$repsales_upper
check$lower <- myIndex$repsales_lower
plot(range(check$date), range(c(check$lower, check$upper)), type="n")
lines(check$date, check$index, col="blue")
lines(check$date, check$upper, col="green")
lines(check$date, check$lower, col="red")




c<-merge(check, d, by="date",all=FALSE)
#c$market <- c$market/c$market[1]
c$index <- c$index - c$index[1] +  c$market[1]
c$upper <- c$upper- c$index[1] +  c$market[1]
c$lower <- c$lower- c$index[1] +  c$market[1]

plot(range(c$date), range(c(c$market,c$upper, c$lower)), type="n")
lines(c$date,c$market, col="green", lwd=3)
lines(c$date,c$upper, col="blue", lwd=1)
lines(c$date,c$index, col="red", lwd=1)
lines(c$date,c$lower, col="blue", lwd=1)

c$outside <- 0
c$outside[c$market > c$upper]<-1
c$outside[c$market < c$lower]<-1






# Display index diagnostics
outputDirectory <- "/home/thies/Ubuntu One/research/openindex/output/"

write.csv(myIndex[["salesSumstats"]],file=paste(outputDirectory,"sumstats.csv", sep="/"), quote=FALSE, row.names=FALSE)


# plot histogram of prices and sales dates
png(file=paste(outputDirectory,'hist_sales_prices.png', sep=""), bg="white")
hist(sales$price, breaks=100, main="Histogram Sales Prices", xlab="Price", col="lightblue")
box()
dev.off()
png(file=paste(outputDirectory,'hist_sales_date.png', sep=""), bg="white")
hist(sales$date, breaks=length(myIndex[["intervals"]]), main="Histogram Sales Prices", xlab="Month", col="lightblue")
box()
dev.off()

# write CSV files of top returns/lowest returns
write.csv(myIndex[['highReturns']],file=paste(outputDirectory,"pairs_return_high.csv", sep="/"), quote=FALSE, row.names=FALSE)
write.csv(myIndex[['lowReturns']],file=paste(outputDirectory,"pairs_return_low.csv", sep="/"), quote=FALSE, row.names=FALSE)
# plot histogram of returns
png(file=paste(outputDirectory,'hist_repeats_returns.png', sep=""), bg="white")
hist(myIndex[["pairs"]]$return, breaks=100, main="Histogram Repeat Sales Returns (uncensored)", xlab="Return", col="lightblue", border=FALSE)
box()
dev.off()

# Distribution of repeat sales pairs per interval
write.csv(myIndex[["pairsPerInterval"]],file=paste(outputDirectory,"pairs_per_interval.csv", sep=""), quote=FALSE, row.names=FALSE)
png(file=paste(outputDirectory,'pairs_per_interval.png', sep=""), bg="white", width=1024)
barplot(t(myIndex[["pairsPerInterval"]][,2:3]), beside=TRUE, col=c("blue","green"), border=FALSE)
legend("topleft",c("first sale","repeat sale"),fil=c("blue","green"))
box()
dev.off()



# Make graph of comparison: Unsmoothed Index vs Median vs Mean
png(file=paste(outputDirectory,'repeatsalesindex_mean_median.png', sep=""), bg="white", width=1024)
plot(myIndex[["intervals"]],myIndex[["RepeatSalesIndexUnsmoothed"]], main="Direct repeat sales index vs Means vs Median", ylab=paste(format(myIndex[["intervals"]][1], format="%Y-%m"),"= 100%"), xlab="Time", type="n")
lines(myIndex[["intervals"]],myIndex[["RepeatSalesIndexUnsmoothed"]])
lines(myIndex[['intervals']][1:length(myIndex[["mean"]])],myIndex[["mean"]],col="red")
lines(myIndex[['intervals']][1:length(myIndex[["median"]])],myIndex[["median"]],col="blue")
box()
legend("topleft",c("repeat sales index","mean","median"), lwd=1, col=c("black","red","blue"))
dev.off()
#==================



# create plot
png(file=paste(outputDirectory,'repeatsalesindex.png', sep=""), bg="white", width=1024)
plot(myIndex[['intervals']],myIndex[["index"]]$estimate,xlab="",ylab=paste(myIndex[['intervals']][1],"= 100%"), type="n")
lines(myIndex[['intervals']], myIndex[["index"]]$estimate)
box()
dev.off()
write.csv(myIndex[["index"]],file=paste(outputDirectory,"index.csv", sep="/"), quote=FALSE, row.names=FALSE)

