
install.packages("ggfortify")
install.packages("forecast")
library(readxl)
SectorData<-`Energy_Consumption_by_Sector`
SectorDataTS<-ts(SectorData, start=1973, frequency=12)

#1 Draw time plot to show monthly energy consumption by each sector 
library(ggplot2)
library(ggfortify)
library(forecast)
autoplot(SectorDataTS,ts.colour='blue',xlab="Month",ylab="Trillion Btu")

# Check seasonality
ggseasonplot(SectorDataTS[,"Total Energy Consumed by the Commercial Sector"], season.labels=NULL,year.labels=FALSE,year.labels.left=FALSE)

# Polar version of seasonal plots 
ggseasonplot(SectorDataTS[,"Total Energy Consumed by the Commercial Sector"], polar=TRUE, season.labels=NULL,year.labels=FALSE,year.labels.left=FALSE)

# Seasonal subseries plots 
ggsubseriesplot(SectorDataTS[,"Total Energy Consumed by the Commercial Sector"])

#2. Draw lag plot of energy consumption by the commercial sector 
gglagplot(SectorDataTS[,"Total Energy Consumed by the Commercial Sector"],lags=40,nrow=NULL,ncol=NULL)

#3. Decompose time series into trend component, seasonal component, and random component 
components.ts=decompose(SectorDataTS[,"Total Energy Consumed by the Commercial Sector"])
plot(components.ts)

#4. Remove seasonal data from original time series
seasonal<-components.ts$seasonal
withoutSeasonal<-SectorDataTS[,"Total Energy Consumed by the Commercial Sector"] - seasonal 
plot(withoutSeasonal)

# Differentiation of data until time series is stationary
ndiffs(withoutSeasonal)
diff(withoutSeasonal)
withoutSeasonalAfterDiff<-diff(withoutSeasonal,lag=12)
plot(withoutSeasonalAfterDiff)

#5 ACF of data.
Acf(withoutSeasonalAfterDiff,plot=FALSE)
AIC(arima(withoutSeasonalAfterDiff, order=c(4,0,1)),arima(withoutSeasonalAfterDiff,order=c(4,0,5)))

#6. Train model
trainingModel<-auto.arima(SectorDataTS[1:541,"Total Energy Consumed by the Commercial Sector"],stationary=FALSE,seasonal=TRUE)

#7. Test model 
testModel<-Arima(SectorDataTS[542:561,"Total Energy Consumed by the Commercial Sector"],model=trainingModel)
accuracy(testModel)

#8 Forecast energy consumption by the Commercial Sector
trainingModel<-auto.arima(SectorDataTS[,"Total Energy Consumed by the Commercial Sector"],stationary=FALSE,seasonal=TRUE)
forecast<-forecast(trainingModel,h=12)
plot(forecast)

