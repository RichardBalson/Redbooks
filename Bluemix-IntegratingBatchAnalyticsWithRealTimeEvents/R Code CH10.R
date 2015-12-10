library(ibmdbR)

##---------------------------------------------Connect to database
mycon <- idaConnect("BLUDB", "", "")
idaInit(mycon)

##---------------------------------------------R Script variables

view_data = FALSE
check_types= FALSE
check_convert_types = FALSE
check_aggregated_date=FALSE
plot_time_series=FALSE
plot_relationship=FALSE
check_correlation=FALSE
plot_rba_model=FALSE


##---------------------------------------------Import Data into R

housing_prices <- as.data.frame(ida.data.frame('"DASH100087"."HOUSES_BY_SUBURB"')[ ,c('LOCALITY', 'P_2004', 'P_2005', 'P_2006', 'P_2007', 'P_2008', 'P_2009', 'P_2010', 'P_2011', 'P_2012', 'P_2013', 'P_2014', 'P_2015')])

rba_cash_rate <- as.data.frame(ida.data.frame('"DASH100087"."RBA_CASH_RATE"')[ ,c('CASH_RATE_TARGET', 'DATE')])

exchange_rate <- as.data.frame(ida.data.frame('"DASH100087"."EXCHANGERATE_V0"')[ ,c('CNY', 'DATE', 'EUR', 'GBP', 'NZD', 'USD')])

##-------------------------------------------View Data

if(view_data) {
##      head(housing_prices)
##     head(rba_cash_rate)
     head(exchange_rate)
}

if(check_types){
##     sapply(housing_prices,class)
##     sapply(rba_cash_rate,class)
    sapply(exchange_rate,class)
}

## -------------------------------------------Convert character data to numeric or date type where necessary

rba_cash_rate[,"CASH_RATE_TARGET"]<-as.numeric(rba_cash_rate[,"CASH_RATE_TARGET"])

rba_cash_rate[,'DATE']<-as.Date(rba_cash_rate[,'DATE'],"%d/%m/%Y")

for(i in 1:6){
   if(i==2){
         exchange_rate[,i] <- as.Date(exchange_rate[,2],)
    } else{
         exchange_rate[, i] <- as.numeric(exchange_rate[,i])
   }
}

##--------------------------------------------Check if types have been converted correctly

if(check_convert_types){
##     sapply(rba_cash_rate,class)
    sapply(exchange_rate,class)
}


##------------Housing prices are yearly, exchange rates and interest rates are monthly. We will average the monthly rates to a yearly average rate

rba_cash_rate$Year <- format(rba_cash_rate$DATE,"%Y")

rba_cash_rate_yearly<-aggregate(CASH_RATE_TARGET~Year, rba_cash_rate,mean)

exchange_rate$Year <- format(exchange_rate$DATE,"%Y")

exch_year_avg<-aggregate(cbind(CNY,  EUR, GBP, NZD, USD)~Year, exchange_rate,mean)

##---------------------------------------------------------------------------Compare rates to average housing price for all suburbs in a given year
df_house_sum <- sapply(housing_prices[,-1], function(x) ifelse(x==0,NA,x))

avg_house_price <- data.frame('Year'=2004:2015,'AveragePrice'=apply(df_house_sum,2,function(x) mean(x, na.rm=TRUE)))

remove(df_house_sum)

##--------------------------------------Check Aggregated data

if(check_aggregated_date){
  ##   avg_house_price
      rba_cash_rate_yearly
##      exch_year_avg
}


##---------------------------------------------------------------------Plot time series data

if(plot_time_series) {
       plot(avg_house_price$Year,avg_house_price$AveragePrice,main="Average House Price in Victoria per Year",xlab="Year",ylab="Average Price",type="l")

 plot(rba_cash_rate_yearly$Year,rba_cash_rate_yearly$CASH_RATE_TARGET,main="Average Yearly RBA Cash Rate Target",xlab="Year",ylab="Average RBA Cash Rate Target",type="l")

 plot(exch_year_avg$Year,exch_year_avg$CNY,main="Average Yearly Exchange Rate ($1AUD)",xlab="Year",ylab="Average Chinese Exchange Rate",type="l")
 plot(exch_year_avg$Year,exch_year_avg$EUR,main="Average Yearly Exchange Rate ($1AUD)",xlab="Year",ylab="Average Euro Exchange Rate",type="l")
 plot(exch_year_avg$Year,exch_year_avg$GBP,main="Average Yearly Exchange Rate ($1AUD)",xlab="Year",ylab="Average Britain Exchange Rate",type="l")
 plot(exch_year_avg$Year,exch_year_avg$NZD,main="Average Yearly Exchange Rate ($1AUD)",xlab="Year",ylab="Average New Zealand Exchange Rate",type="l")
 plot(exch_year_avg$Year,exch_year_avg$USD,main="Average Yearly Exchange Rate ($1AUD)",xlab="Year",ylab="Average United States Exchange Rate",type="l")

}

##--------------------------------------------------------------------------Do basic graphing to see if a relationship exists

exch_year_tmp <- exch_year_avg[exch_year_avg$Year>2003,]

rba_cash_tmp <- rba_cash_rate_yearly[rba_cash_rate_yearly$Year>2003,]

if(plot_relationship){
plot(rba_cash_tmp$CASH_RATE_TARGET,avg_house_price$AveragePrice,main="Cash Rate Target compared to the Average House Price in Victoria",xlab="RBA Cash Rate Target",ylab="Average Price")

plot(exch_year_tmp$CNY,avg_house_price$AveragePrice,main="Average Victorian House Price to the AUD to CNY Exchange Rate",xlab="AUD to CNY Exchange Rate",ylab="Average Price")

plot(exch_year_tmp$EUR,avg_house_price$AveragePrice,main="Average Victorian House Price to the AUD to EURO Exchange Rate",xlab="AUD to Euro Exchange Rate",ylab="Average Price")

plot(exch_year_tmp$GBP,avg_house_price$AveragePrice,main="Average Victorian House Price to the AUD to GBP Exchange Rate",xlab="AUD to GBP Exchange Rate",ylab="Average Price")

plot(exch_year_tmp$NZD,avg_house_price$AveragePrice,main="Average Victorian House Price to the AUD to NZD Exchange Rate",xlab="AUD to NZD Exchange Rate",ylab="Average Price")

plot(exch_year_tmp$USD,avg_house_price$AveragePrice,main="Average Victorian House Price to the AUD to USD Exchange Rate",xlab="AUD to USD Exchange Rate",ylab="Average Price")

}

##----------------------------------------------Check predictive variable correlation

if(check_correlation){
cor(cbind(rba_cash_tmp$CASH_RATE_TARGET,exch_year_tmp[,-1]))
}

##---------------------------------------------------------------------Develop model based on interest rate

housing_model <- lm(avg_house_price$AveragePrice~rba_cash_tmp$CASH_RATE_TARGET)

summary(housing_model)


##---------------------------------------------------------------------Check how linear mode fits the data

if(plot_rba_model){
plot(rba_cash_tmp$CASH_RATE_TARGET,avg_house_price$AveragePrice,main="Model of housing Prices",xlab="RBA Cash Rate",ylab="Average House Price in Victoria")
abline(housing_model,col='red')
}

##---------------------------------------------------------------------Develop model based on interest rate and British Pound AUD exchange rate

housing_model_2 <- lm(avg_house_price$AveragePrice~rba_cash_tmp$CASH_RATE_TARGET+exch_year_tmp$GBP)

summary(housing_model_2)

