#Import S$P 500, Dow Jones, Energy Sector, IT Sector, Real Estate Sector, Banking Sector Data
library(quantmod)
library(fGarch)
library(forecast)

par(mfrow=c(2,2))
#Get S&P 500 overall market index

sp500=getSymbols('^GSPC',from='2000-01-01',to='2016-11-30',auto.assign=FALSE)

#Get S&P 500 sector indices, based on indivdiual SPDRs in S&P 500

Energy.500=getSymbols('XLE', from='2000-01-01', to='2016-11-30', auto.assign=FALSE)
IT.500=getSymbols('XLK', from='2000-01-01', to='2016-11-30', auto.assign=FALSE)
HealthCare.500=getSymbols('XLV', from='2000-01-01', to='2016-11-30', auto.assign=FALSE)
Financials.500=getSymbols('XLF', from='2000-01-01', to='2016-11-30', auto.assign=FALSE )

#Plot Data
plot(sp500$GSPC.Adjusted, main="S&P 500 Adjusted", ylab="Price", xlab="Time")
plot(Energy.500$XLE.Adjusted, main="Energy Sector Adjusted", ylab="Price", xlab="Time")
plot(IT.500$XLK.Adjusted, main="Technology Sector Adjusted", ylab="Price", xlab="Time")
plot(HealthCare.500$XLV.Adjusted, main="Health Care Sector Adjusted", ylab="Price", xlab="Time")
plot(Financials.500$XLF.Adjusted, main="Financials Sector Adjusted", ylab="Price", xlab="Time")

#Get Daily Log Return Data of all indices to stabilize mean and variance
sp500.Daily=dailyReturn(sp500$GSPC.Adjusted, type='log')
Energy.500.Daily=dailyReturn(Energy.500$XLE.Adjusted, type='log')
IT.500.Daily=dailyReturn(IT.500$XLK.Adjusted, type='log')
HealthCare.500.Daily=dailyReturn(HealthCare.500$XLV.Adjusted, type='log')
Financials.500.Daily=dailyReturn(Financials.500$XLF.Adjusted, type='log')

#Plot Differenced Data
plot(sp500.Daily, main="S&P 500 Daily Log Return", ylab="Price (Log)", xlab="Time")
plot(Energy.500.Daily, main="Energy Sector Daily Log Return", ylab="Price (Log)", xlab="Time")
plot(IT.500.Daily, main="Technology Sector Daily Log Return", ylab="Price (Log)", xlab="Time")
plot(HealthCare.500.Daily, main="Health Care Sector Daily Log Return", ylab="Price (Log)", xlab="Time")
plot(Financials.500.Daily, main="Financials Sector Daily Log Return", ylab="Price (Log)", xlab="Time")

#Perform Residual Testing
Box.test(sp500.Daily,lag=10,type="Ljung")
Box.test(Energy.500.Daily,lag=10,type="Ljung")
Box.test(IT.500.Daily,lag=10,type="Ljung")
Box.test(HealthCare.500.Daily,lag=10,type="Ljung")
Box.test(Financials.500.Daily,lag=10,type="Ljung")


#ARMA Modeling (ACF/PACF plotting, Arima(), etc.), followed by GARCH Modeling 

#S&P 500 Modeling
acf(sp500.Daily, main="S&P 500 Daily Log Return ACF")
pacf(sp500.Daily,main="S&P 500 Daily Log Return PACF")


sp500.fit=Arima(sp500.Daily, order=c(2,0,0))
sp500.fit
Box.test(sp500.fit$residuals, lag=10, type="Ljung")
Box.test(sp500.fit$residuals^2, lag=10, type="Ljung")

sp500.garchFit=garchFit(~arma(1,0) + garch(1,1), sp500.Daily, trace=F)
sp500.garchFit
summary(sp500.garchFit)

#Residuals and volatility spikes for S&P500 
residual=residuals(sp500.garchFit, standardize=T)
par(mfcol=c(2,1))
plot(residual,xlab='Time',ylab='Residuals',type='l')
par(mfcol=c(2,2))
acf(residual, lag=24, main="S&P 500 GARCH Residuals ACF")
pacf(residual, lag=24, main="S&P 500 GARCH Residuals PACF")
acf(residual^2, lag=24, main="S&P 500 GARCH Squared Residuals ACF")
pacf(residual^2, lag=24, main="S&P 500 GARCH Squared Residuals PACF") 

v1=volatility(sp500.garchFit)
plot(v1,type="l", main="S&P 500 Volatility")
mean(v1)
median(v1)
#Volatility Mean is 1.1%, Volatility Median is 0.9% 

#Spike1 424-480, Corresponding Data: September 17, 2001-November 2, 2001
#Big event: 9/11 terror attacks and lasting impact
v2=v1[424:480]
mean(v2)
median(v2)
plot(v2,type="l", main="S&P 500 Volatility Spike 1")
#Mean volatility=1.4%, Median volatility=1.3%

#Spike2 2215-2226, Corresponding Data: September 29, 2008-October 22, 2008 
#Big event: Lehman Brothers bankruptcy, House rejects 7 billion dollar bailout, slumping oil prices/weak earnings fear of recession 
v3=v1[2215:2226]
mean(v3)
median(v3)
plot(v3,type="l", main="S&P 500 Volatility Spike 2")
#Mean volatility=4.5%, Median volatility=4.5%

#Spike3 2592-2610, Corresponding Data: April 27, 2010-May 20, 2010
#Big event: Flash Crash, European Debt Crisis, slump in Euro lowers Greece/Portugal debt rating
v4=v1[2592:2610]
mean(v4)
median(v4)
plot(v4, type="l", main="S&P 500 Volatility Spike 3")
#Mean volatility=1.4%, Median Volatility=1.3%

#Spike4 2915-2926, Corresponding Data: August 4, 2011-August 18, 2011
#Big event: Global Panic (Aug 4th), Black Monday (Aug 8th), Cutting ties with Syrian Government (Aug 18th)
v5=v1[2915:2926]
mean(v5)
median(v5)
plot(v5, type="l", main="S&P 500 Volatility Spike 4")
#Mean volatility=2.5%, Median volatility= 2.8%

#Spike5 2974-3005, Corresponding Data: October 27th, 2011-December 9th, 2011
#Big event: Penn State football coach fired over sexual abuse scandal (November 9th, 2011)
v6=v1[2974:3005]
mean(v6)
median(v6)
plot(v6, type="l", main="S&P 500 Volatility Spike 5")
#Mean volatility= 1.7%, Median volatility=1.7%

#Spike6 3339-3351, Corresponding Data: April 12, 2013-April 30, 2013
#Big event: Boston marathon bombings (April 15, 2013)
v7=v1[3339:3351]
mean(v7)
median(v7)
plot(v7, type="l", main="S&P 500 Volatility Spike 6")
#Mean volatility=.94%, Median volatility=1.0%

#Energy Sector Modeling
acf(Energy.500.Daily, main="Energy Sector Daily Log Return ACF")
pacf(Energy.500.Daily, main="Energy Sector Daily Log Return PACF")

Energy.fit=Arima(Energy.500.Daily, order=c(4,0,1))
Energy.fit

Box.test(Energy.fit$residuals, lag=10, type="Ljung")
Box.test(Energy.fit$residuals^2, lag=10, type="Ljung")

Energy.garchFit=garchFit(~arma(1,1)+garch(1,1), Energy.500.Daily, trace=F)
Energy.garchFit

#Residuals and volatility spikes for Energy Sector
residual_eng=residuals(Energy.garchFit, standardize=T)
par(mfcol=c(2,1))
plot(residual_eng,xlab='Time',ylab='Residuals',type='l')
par(mfcol=c(2,2))
acf(residual_eng, lag=24, main="Energy Sector GARCH Residuals ACF")
pacf(residual_eng, lag=24, main="Energy Sector GARCH Residuals PACF")
acf(residual_eng^2, lag=24, main="Energy Sector GARCH Squared Residuals ACF")
pacf(residual_eng^2, lag=24, main="Energy Sector GARCH Squared Residuals PACF") 
#The model is adeqaute

par(mfrow=c(2,1))

#When volatility spikes occur  
v_eng=volatility(Energy.garchFit)
plot(v_eng,type="l", main="Energy Sector Volatility")
mean(v_eng)
median(v_eng)
#Volatility mean is 1.6%, volatility median is 1.4% however, for some specific time span

#Spike1 424-480, Corresponding Data: September 17, 2001-November 2, 2001
#Big event: 9/11 terror attacks and lasting impact
v2_eng=v_eng[424:480]
mean(v2_eng)
median(v2_eng)
plot(v2_eng,type="l", main="Energy Sector Volatility Spike 1")
#Mean volatility=2.0%, Median volatility=2.0%

#Spike2 2215-2226, Corresponding Data: September 29, 2008-October 22, 2008 
#Big event: Lehman Brothers bankruptcy, House rejects 7 billion dollar bailout, slumping oil prices/weak earnings fear of recession 
v3_eng=v_eng[2215:2226]
mean(v3_eng)
median(v3_eng)
plot(v3_eng,type="l", main="Energy Sector Volatility Spike 2")
#Mean volatility=7.2%, Median volatility=7.3%

#Spike3 2592-2610, Corresponding Data: April 27, 2010-May 20, 2010
#Big event: Flash Crash, European Debt Crisis, slump in Euro lowers Greece/Portugal debt rating
v4_eng=v_eng[2592:2610]
mean(v4_eng)
median(v4_eng)
plot(v4_eng, type="l", main="Energy Sector Volatility Spike 3")
#Mean volatility=1.6%, Median Volatility=1.5%

#Spike4 2915-2926, Corresponding Data: August 4, 2011-August 18, 2011
#Big event: Global Panic (Aug 4th), Black Monday (Aug 8th), Cutting ties with Syrian Government (Aug 18th)
v5_eng=v_eng[2915:2926]
mean(v5_eng)
median(v5_eng)
plot(v5_eng, type="l", main="Energy Sector Volatility Spike 4")
#Mean volatility=2.9%, Median volatility= 3.3%

#Spike5 2974-3005, Corresponding Data: October 27th, 2011-December 9th, 2011
#Big event: Penn State football coach fired over sexual abuse scandal (November 9th, 2011)
v6_eng=v_eng[2974:3005]
mean(v6_eng)
median(v6_eng)
plot(v6_eng, type="l",main="Energy Sector Volatility Spike 5")
#Mean volatility= 2.4%, Median volatility=2.4%

#Spike6 3339-3351, Corresponding Data: April 12, 2013-April 30, 2013
#Big event: Boston marathon bombings (April 15, 2013)
v7_eng=v_eng[3339:3351]
mean(v7_eng)
median(v7_eng)
plot(v7_eng, type="l", main="Energy Sector Volatility Spike 6")
#Mean volatility=1.3%, Median volatility=1.4%

#IT Sector Modeling
acf(IT.500.Daily, main="Technology Sector Daily Log Return ACF")
pacf(IT.500.Daily, main="Technology Sector Daily Log Return PACF")

IT.fit=Arima(IT.500.Daily, order=c(2,0,0))
IT.fit
Box.test(IT.fit$residuals, lag=10, type="Ljung")
Box.test(IT.fit$residuals^2, lag=10, type="Ljung")

IT.garchFit=garchFit(~garch(1,1), IT.500.Daily, trace=F)
IT.garchFit

#Residuals and volatility spikes for IT Sector
residual_IT=residuals(IT.garchFit, standardize=T)
par(mfcol=c(2,1))
plot(residual_IT,xlab='Time',ylab='Residuals',type='l')
par(mfcol=c(2,2))
acf(residual_IT, lag=24, main="Technology Sector GARCH Residuals ACF")
pacf(residual_IT, lag=24, main="Technology Sector GARCH Residuals PACF")
acf(residual_IT^2, lag=24, main="Technology Sector GARCH Squared Residuals ACF")
pacf(residual_IT^2, lag=24, main="Technology Sector GARCH Squared Residuals PACF") 
#The model is adequate

par(mfrow=c(2,1))

#When volatility spikes occur  
v_IT=volatility(IT.garchFit)
plot(v_IT,type="l",main="Technology Sector Volatility")
mean(v_IT)
median(v_IT)
#Volatility mean is 1.4%, Volatility median is 1.1% 

#Spike 1: 59-71, Corresponding Data: March 27, 2000-April 12, 2000
#Big event: Dot com bubble burst (Climax March 10, 2000)
v1_IT=v_IT[59:71]
mean(v1_IT)
median(v1_IT)
plot(v1_IT, type="l", main="Technology Sector Volatility Spike 1")
#Mean volatility= 2.1%, Median volatility=2.1%

#Spike2 424-480, Corresponding Data: September 17, 2001-November 2, 2001
#Big event: 9/11 terror attacks and lasting impact
v2_IT=v_IT[424:480]
mean(v2_IT)
median(v2_IT)
plot(v2_IT,type="l", main="Technology Sector Volatility Spike 2")
#Mean volatility=2.5%, Median volatility=2.5%

#Spike3 2215-2226, Corresponding Data: September 29, 2008-October 22, 2008 
#Big event: Lehman Brothers bankruptcy, House rejects 7 billion dollar bailout, slumping oil prices/weak earnings fear of recession 
v3_IT=v_IT[2215:2226]
mean(v3_IT)
median(v3_IT)
plot(v3_IT,type="l",main="Technology Sector Volatility Spike 3")
#Mean volatility=4.8%, Median volatility=4.7%

#Spike4 2592-2610, Corresponding Data: April 27, 2010-May 20, 2010
#Big event: Flash Crash, European Debt Crisis, slump in Euro lowers Greece/Portugal debt rating
v4_IT=v_IT[2592:2610]
mean(v4_IT)
median(v4_IT)
plot(v4_IT, type="l", main="Technology Sector Volatility Spike 4")
#Mean volatility=1.4%, Median Volatility=1.3%

#Spike5 2915-2926, Corresponding Data: August 4, 2011-August 18, 2011
#Big event: Global Panic (Aug 4th), Black Monday (Aug 8th), Cutting ties with Syrian Government (Aug 18th)
v5_IT=v_IT[2915:2926]
mean(v5_IT)
median(v5_IT)
plot(v5_IT, type="l",main="Technology Sector Volatility Spike 5")
#Mean volatility=2.2%, Median volatility= 2.4%

#Spike6 2974-3005, Corresponding Data: October 27th, 2011-December 9th, 2011
#Big event: Penn State football coach fired over sexual abuse scandal (November 9th, 2011)
v6_IT=v_IT[2974:3005]
mean(v6_IT)
median(v6_IT)
plot(v6_IT, type="l", main="Technology Sector Volatility Spike 6")
#Mean volatility= 1.7%, Median volatility=1.7%

#Spike7 3339-3351, Corresponding Data: April 12, 2013-April 30, 2013
#Big event: Boston marathon bombings (April 15, 2013)
v7_IT=v_IT[3339:3351]
mean(v7_IT)
median(v7_IT)
plot(v7_IT, type="l", main="Technology Sector Volatility Spike 7")
#Mean volatility=.98%, Median volatility=.99%

#HealthCare Sector Modeling
acf(HealthCare.500.Daily, main="Health Care Sector Daily Log Return ACF")
pacf(HealthCare.500.Daily, main="Health Care Sector Daily Log Return PACF")

HealthCare.fit=Arima(HealthCare.500.Daily, order=c(0,0,5))
HealthCare.fit
Box.test(HealthCare.fit$residuals, lag=10, type="Ljung")
Box.test(HealthCare.fit$residuals^2, lag=10, type="Ljung")

HealthCare.garchFit=garchFit(~arma(0,4) + garch(1,1), HealthCare.500.Daily, trace=F)
HealthCare.garchFit

#Residuals and Volatility spikes for IT Sector
residual_HealthCare=residuals(HealthCare.garchFit, standardize=T)
par(mfcol=c(2,1))
plot(residual_HealthCare,xlab='Time',ylab='Residuals',type='l')
par(mfcol=c(2,2))
acf(residual_HealthCare, lag=24, main="Health Care Sector GARCH Residuals ACF")
pacf(residual_HealthCare, lag=24, main="Health Care Sector GARCH Residuals PACF")
acf(residual_HealthCare^2, lag=24, main="Health Care Sector GARCH Squared Residuals ACF")
pacf(residual_HealthCare^2, lag=24, main="Health Care Sector GARCH Squared Residuals PACF") 
#The model is adequate

par(mfrow=c(2,1))

#When volatility spikes occur
v_HealthCare=volatility(HealthCare.garchFit)
plot(v_HealthCare,type="l",main="Health Care Sector Volatility")
mean(v_HealthCare)
median(v_HealthCare)
#Volatility mean is 1.1%, Volatility median is 0.9% however, for some specific time span

#Spike1 424-480, Corresponding Data: September 17, 2001-November 2, 2001
#Big event: 9/11 terror attacks and lasting impact
v2_HealthCare=v_HealthCare[424:480]
mean(v2_HealthCare)
median(v2_HealthCare)
plot(v2_HealthCare,type="l", main="Health Care Sector Volatility Spike 1")
#Mean volatility=1.8%, Median volatility=1.7%

#Spike2 2215-2226, Corresponding Data: September 29, 2008-October 22, 2008 
#Big event: Lehman Brothers bankruptcy, House rejects 7 billion dollar bailout, slumping oil prices/weak earnings fear of recession 
v3_HealthCare=v_HealthCare[2215:2226]
mean(v3_HealthCare)
median(v3_HealthCare)
plot(v3_HealthCare,type="l", main="Health Care Sector Volatility Spike 2")
#Mean volatility=3.7%, Median volatility=3.8%

#Spike3 2592-2610, Corresponding Data: April 27, 2010-May 20, 2010
#Big event: Flash Crash, European Debt Crisis, slump in Euro lowers Greece/Portugal debt rating
v4_HealthCare=v_HealthCare[2592:2610]
mean(v4_HealthCare)
median(v4_HealthCare)
plot(v4_HealthCare, type="l",main="Health Care Sector Volatility Spike 3")
#Mean volatility=1.1%, Median Volatility=.96%

#Spike4 2915-2926, Corresponding Data: August 4, 2011-August 18, 2011
#Big event: Global Panic (Aug 4th), Black Monday (Aug 8th), Cutting ties with Syrian Government (Aug 18th)
v5_HealthCare=v_HealthCare[2915:2926]
mean(v5_HealthCare)
median(v5_HealthCare)
plot(v5_HealthCare, type="l", main="Health Care Sector Volatility Spike 4")
#Mean volatility=2.1%, Median volatility= 2.3%

#Spike5 2974-3005, Corresponding Data: October 27th, 2011-December 9th, 2011
#Big event: Penn State football coach fired over sexual abuse scandal (November 9th, 2011)
v6_HealthCare=v_HealthCare[2974:3005]
mean(v6_HealthCare)
median(v6_HealthCare)
plot(v6_HealthCare, type="l", main="Health Care Sector Volatility Spike 5")
#Mean volatility= 1.4%, Median volatility=1.4%

#Spike6 3339-3351, Corresponding Data: April 12, 2013-April 30, 2013
#Big event: Boston marathon bombings (April 15, 2013)
v7_HealthCare=v_HealthCare[3339:3351]
mean(v7_HealthCare)
median(v7_HealthCare)
plot(v7_HealthCare, type="l", main="Health Care Sector Volatility Spike 6")
#Mean volatility=.96%, Median volatility=.98%

#Spike7 3784-3796, Corresponding Data: January 20, 2015-February 5, 2015
#Big event: Obamacare implementation
v8_HealthCare=v_HealthCare[3784:3796]
mean(v8_HealthCare)
median(v8_HealthCare)
plot(v8_HealthCare, type="l", main="Health Care Sector Volatility Spike 7")
#Mean volatility=1.0%, Median volatility=1.0%

#Financials Sector Modeling
acf(Financials.500.Daily, main="Financials Sector Daily Log Return ACF")
pacf(Financials.500.Daily, main="Financials Sector Daily Log Return PACF")

Financials.fit=Arima(Financials.500.Daily, order=c(2,0,4))
Financials.fit
Box.test(Financials.fit$residuals, lag=10, type="Ljung")
Box.test(Financials.fit$residuals^2, lag=10, type="Ljung")

Financials.garchFit=garchFit(~arma(2,2) + garch(1,1), Financials.500.Daily, trace=F)
Financials.garchFit

#Residuals and volatility spikes for IT Sector
residual_Financials=residuals(Financials.garchFit, standardize=T)
par(mfcol=c(2,1))
plot(residual_Financials,xlab='Time',ylab='Residuals',type='l')
par(mfcol=c(2,2))
acf(residual_Financials, lag=24,main="Financials Sector GARCH Residuals ACF")
pacf(residual_Financials, lag=24,main="Financials Sector GARCH Residuals PACF")
acf(residual_Financials^2, lag=24,main="Financials Sector GARCH Squared Residuals ACF")
pacf(residual_Financials^2, lag=24, main="Financials Sector GARCH Squared Residuals PACF") 

#The model is adequate

par(mfrow=c(2,1))

#when volatility spikes occur
v_Financials=volatility(Financials.garchFit)
plot(v_Financials,type="l", main="Financials Sector Volatility")
mean(v_Financials)
median(v_Financials)

#Volatility mean is 1.4%, volatility median is 1.1% however, for some specific time span

#Spike 1: 59-71, Corresponding Data: March 27, 2000-April 12, 2000
#Big event: Dot com bubble burst (Climax March 10, 2000)
v1_Financials=v_Financials[59:71]
mean(v1_Financials)
median(v1_Financials)
plot(v1_Financials, type="l", main="Financials Sector Volatility Spike 1")
#Mean volatility= 5.8%, Median volatility=5.6%

#Spike2 424-480, Corresponding Data: September 17, 2001-November 2, 2001
#Big event: 9/11 terror attacks and lasting impact
v2_Financials=v_Financials[424:480]
mean(v2_Financials)
median(v2_Financials)
plot(v2_Financials,type="l", main="Financials Sector Volatility Spike 2")
#Mean volatility=1.8%, Median volatility=1.7%

#Spike3 2215-2226, Corresponding Data: September 29, 2008-October 22, 2008 
#Big event: Lehman Brothers bankruptcy, House rejects 7 billion dollar bailout, slumping oil prices/weak earnings fear of recession 
v3_Financials=v_Financials[2215:2226]
mean(v3_Financials)
median(v3_Financials)
plot(v3_Financials,type="l", main="Financials Sector Volatility Spike 3")
#Mean volatility=6.6%, Median volatility=6.6%

#Spike4 2592-2610, Corresponding Data: April 27, 2010-May 20, 2010
#Big event: Flash Crash, European Debt Crisis, slump in Euro lowers Greece/Portugal debt rating
v4_Financials=v_Financials[2592:2610]
mean(v4_Financials)
median(v4_Financials)
plot(v4_Financials, type="l", main="Financials Sector Volatility Spike 4")
#Mean volatility=2.1%, Median Volatility=2.0%

#Spike5 2915-2926, Corresponding Data: August 4, 2011-August 18, 2011
#Big event: Global Panic (Aug 4th), Black Monday (Aug 8th), Cutting ties with Syrian Government (Aug 18th)
v5_Financials=v_Financials[2915:2926]
mean(v5_Financials)
median(v5_Financials)
plot(v5_Financials, type="l", main="Financials Sector Volatility Spike 5")
#Mean volatility=3.8%, Median volatility= 4.4%

#Spike6 2974-3005, Corresponding Data: October 27th, 2011-December 9th, 2011
#Big event: Penn State football coach fired over sexual abuse scandal (November 9th, 2011)
v6_Financials=v_Financials[2974:3005]
mean(v6_Financials)
median(v6_Financials)
plot(v6_Financials, type="l", main="Financials Sector Volatility Spike 6")
#Mean volatility= 2.7%, Median volatility=2.7%

#Spike7 3339-3351, Corresponding Data: April 12, 2013-April 30, 2013
#Big event: Boston marathon bombings (April 15, 2013)
v7_Financials=v_Financials[3339:3351]
mean(v7_Financials)
median(v7_Financials)
plot(v7_Financials, type="l", main="Financials Sector Volatility Spike 7")
#Mean volatility=.1.2%, Median volatility=1.2%


