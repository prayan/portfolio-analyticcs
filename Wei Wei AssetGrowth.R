# Wei Wei 104688156
# 237M Behavioral Finance Project
# Cooper, Gulen, Schill (2008). Asset Growth and the Cross-Section of Stock Returns

setwd("/Users/ciciww/Documents/R")
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)

crsp = read.csv("CRSP.csv")
compustat = read.csv("compustat.csv")

## Input CRSP ##
# change the crsp colnames to lower case
colnames(crsp) <- tolower(colnames(crsp))

# get rid of the read.csv producing levels problem
crsp[,'cusip'] <- (as.character(crsp[,'cusip']))
crsp[,'ret'] <- as.numeric(as.character(crsp[,'ret']))
crsp[,'dlret'] <- as.numeric(as.character(crsp[,'dlret']))

# extract year and month from date
crsp$date <- ymd(crsp$date)
crsp$year <- year(crsp$date)
crsp$month <- month(crsp$date)

## Input Compustat ##
#
# Old Description New
# 6 Assets - Total : AT
# 25 Common Shares Outstanding : CSHO
# 199 Price Close - Annual - Fiscal : PRCC_F

###############################################################

## clean compustat data ##
# select NYSE/AMEX/NASDAQ stocks
# 11 - NYSE, 12 - AMEX, 14 - NASDAQ 
# compustat <- filter(compustat, (exchg == 11) | (exchg == 12) | (exchg == 14))

# reshape cusip to match crsp
compustat$cusip <- substr(compustat$cusip, 1, 8)
compustat$datadate <- ymd(compustat$datadate)
compustat$year <- year(compustat$datadate) 
compustat$month <- month(compustat$datadate)

compustat <- mutate(group_by(compustat, cusip), lagAT = lag(at), lagAT2 = lag(at,2))

# keep only meaningful values
compustat <- filter(compustat, !is.na(at), !is.na(lagAT), !is.na(lagAT2),
                    !is.na(csho), !is.na(prcc_f),
                    at > 0, lagAT > 0, lagAT2 > 0, csho > 0, prcc_f > 0)

# calculate fundamentals, meanings of variables see above chart and 
compustat <- mutate(compustat,
                    assetG = lagAT / lagAT2 - 1,
                    mktCap = prcc_f * csho)

## merge compustat with crsp
# select NYSE/AMEX/NASDAQ stocks: 1 - NYSE, 2 - AMEX, 3 - NASDAQ 
crspmerge <- filter(crsp, (exchcd == 1) | (exchcd == 2) | (exchcd == 3))
crspmerge[!is.na(crspmerge$dlret),]$ret <- crspmerge[!is.na(crspmerge$dlret),]$dlret
## crspmerge is used to merge with compustat ##
crspmerge <- select(crsp, permno, date, year, month, ret)
crspmerge <- filter(crspmerge, ret > -1)

## sort crspmerge by permno year month ##
crspmerge <- arrange(crspmerge, permno, year, month)

## prepare compustat for merge, compustatmerge is used to merge with crspmerge ##
## move month of report date forward 4 month, the finnancial data will be used ##
## since this month and onward, as assumed in page 23 ##
compustatmerge <- compustat

compustatmerge <- mutate(compustatmerge, month = ifelse(month >= 1 & month <= 8, month + 4, month - 8))
compustatmerge <- mutate(compustatmerge, year = ifelse(month >= 1 & month <= 4, fyear + 1, fyear + 0))
compustatmerge <- select(compustatmerge, gvkey, datadate, fyear, year, fmonth = month, cusip, assetG, mktCap)

## Add permnos to compustat by matching cusips ##
cusips <- filter(crsp, month == 1)
cusips <- select(cusips, permno, cusip, year)

## sort by cusip year
cusips <- arrange(cusips, cusip, year)
compustatmerge <- arrange(compustatmerge, cusip, year)

compustatmerge <- inner_join(compustatmerge, cusips, by = c('cusip', 'year'))

## merge crspmerge and compustatmerge by permno ##
## we think the reason not use cusip to merge is that cusip is not perfectly number ##
## fin_ret contains financial data and returns ##

## sort data
compustatmerge <- arrange(compustatmerge, permno, year)
finRet <- inner_join(compustatmerge, crspmerge, by = c('permno', 'year'))

remove(compustatmerge, crspmerge, cusips)

# delete data that does no have valid permno (cannot be identified) ##
finRet <- filter(finRet, !is.na(permno))

## adjust financial data. when merge, all physical year has same financial data
## however, investors only use the financial data after fmonth (when financial data
## of that year come out, before that month, invesor use last year financial data ##
finRet <- arrange(finRet, permno, year, month)

for (i in 1:11) {
  # finRet <- mutate(group_by(finRet, permno), 
  #                  assetG = ifelse((fmonth ==  (i + 1)) & (month < fmonth), lag(assetG, i), assetG))
  finRet <- mutate(group_by(finRet, permno), 
                   mktCap = ifelse((fmonth ==  (i + 1)) & (month < fmonth), lag(mktCap, i), mktCap))
}

## delete the data that is not correct ##
finRet = filter(finRet, !is.na(mktCap), !is.na(assetG), mktCap > 0)

## rank Asset Growth Rate for each month
finRet <- arrange(finRet, year, month)
finRet <- transform(finRet, assetgRank = ave(assetG, year, month, FUN = function(x) ntile(x, 10)))

## calculate cumulative returns from t-2 to t-12 month ##
finRet <- arrange(finRet, permno, year, month)
finRet <- mutate(group_by(finRet, permno), indx = row_number(-permno), 
                 lagRet = 0)
## use indx to control the calculation of PR1YR ##

## This loop is used to calculate cumulative returns from t-2 to t-12 month ##
for (i in 2:11) {
  finRet <- mutate(group_by(finRet, permno), lagRet = (1 + lagRet) * (1 + lag(ret, i)) - 1)
  ## cumulative return from t-2 to t-12 month ##
}

# the first 12 lines are calculated wrong ##
finRet <- filter(finRet, indx > 12)
finRet <- select(finRet, -indx)

# ## some groups only have one stocks, it is excluded ##
# finRet <- filter(finRet, freq > 1)
# finRet <- select(finRet, -sumCap, -freq)
# 
# remove(retComp_EW, retComp_VW)
################################################################
## calculate raw return and adjust return of each assetG decile ##
## seperate each assetG decile in to each sub database and calculate return ##
ret_VW_ASSETG = list()
ret_EW_ASSETG = list()

for (i in 1:10) {
  ASSETG <- filter(finRet, assetgRank == i)
  ASSETG <- arrange(ASSETG, year, month)
  temp <- summarise(group_by(ASSETG, year, month),
                    sumCap = sum(mktCap))
  ASSETG <- inner_join(ASSETG, temp, by = c('year', 'month'))
  
  ## value-weighted
  retComp_VW <- mutate(ASSETG, weights = mktCap / sumCap)
  ## Calculate benchmark weighted returns ##
  retComp_VW <- arrange(retComp_VW, year, month)
  
  ret_VW_ASSETG[[i]] <- summarise(group_by(retComp_VW, year, month),
                               meanLagRet = sum(lagRet * weights))

  ret_VW_ASSETG[[i]]$meanLagRet[is.na(ret_VW_ASSETG[[i]]$meanLagRet)] <- 0

  ret_VW_ASSETG[[i]] <- mutate(ret_VW_ASSETG[[i]], rank = i)
  
  ## equal-weighted
  ## Calculate benchmark weighted returns ##
  retComp_EW <- arrange(ASSETG, year, month)
  
  ret_EW_ASSETG[[i]] <- summarise(group_by(retComp_EW, year, month),
                               meanLagRet = mean(lagRet))
  
  ret_EW_ASSETG[[i]]$meanLagRet[is.na(ret_EW_ASSETG[[i]]$meanLagRet)] <- 0
  
  ret_EW_ASSETG[[i]] <- mutate(ret_EW_ASSETG[[i]], rank = i)
}

################################################################

## vertical merge return data ##
ASSETGCap <- do.call('rbind', ret_VW_ASSETG)
ASSETGEq <- do.call('rbind', ret_EW_ASSETG)

## calculate the average weighted returns of ASSETG Decile 10 (High growth), Decile 1 (Low growth)##
ASSETGCap = filter(ASSETGCap, month == 7)
ASSETGCap = select(ASSETGCap, -month)
VW_dec10 = filter(ASSETGCap, rank == 10)
VW_dec1 = filter(ASSETGCap, rank == 1)
VW_plot = left_join(VW_dec10, VW_dec1, by = 'year')
names(VW_plot)[names(VW_plot)=="meanLagRet.x"] <- "Decile 10"
names(VW_plot)[names(VW_plot)=="meanLagRet.y"] <- "Decile 1"
VW_plot = select(VW_plot, -rank.x, -rank.y)
VW_plot$spread = VW_plot$`Decile 1` - VW_plot$`Decile 10`
meltdf = melt(VW_plot, id = 'year')
ggplot(meltdf,aes(x=year,y=value,colour=variable,group=variable)) + geom_line() + labs(list(title = "Panel B: Value-weighted portfolios", x = "", y = "Annual return"))

ASSETGEq = filter(ASSETGEq, month == 7)
ASSETGEq = select(ASSETGEq, -month)
EW_dec10 = filter(ASSETGEq, rank == 10)
EW_dec1 = filter(ASSETGEq, rank == 1)
EW_plot = left_join(EW_dec10, EW_dec1, by = 'year')
names(EW_plot)[names(EW_plot)=="meanLagRet.x"] <- "Decile 10"
names(EW_plot)[names(EW_plot)=="meanLagRet.y"] <- "Decile 1"
EW_plot = select(EW_plot, -rank.x, -rank.y)
EW_plot$spread = EW_plot$`Decile 1` - EW_plot$`Decile 10`
meltdf = melt(EW_plot, id = 'year')
ggplot(meltdf,aes(x=year,y=value,colour=variable,group=variable)) + geom_line() + labs(list(title = "Panel A: Equal-weighted portfolios", x = "", y = "Annual return"))

remove(temp)
remove(VW_dec1)
remove(VW_dec10)
remove(EW_dec1)
remove(EW_dec10)
