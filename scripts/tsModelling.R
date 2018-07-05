# Some time series forecasting ----

# Find break points from visualization using plotly (Hacky way) ----
# Formal method exists but won't go for that
library(plotly)
pb <- plotly_build(smoothAggrAirAllStores)
for(k in 1:length(pb$x$data))
  pb$x$data[[k]] <- modifyList(pb$x$data[[k]],
                               list(text = paste0('Date and time: ', 
                                                  as.character(combPlotData$visit_date), 
                                                  '<br>',
                                                  'Measure: ', 
                                                  as.character(pb$x$data[[k]]$y))))

sp = "2016-01-01" # start point
b1 = "2016-07-25" #(break)
b2 = "2016-10-27" #(break)
ep = "2017-04-22" # end point

# Identify sesaonality (another Hacky way) ---- 
sesonalFreq <- apply(combPlotData[,-1], 2, findfrequency)
sesonalFreq # Rough estimate of seasonality. Will use 7 days frequency for all series

# Idea! forecast + backcast weighted average imputation -----
# Define univariate sereis 
arvTS <- ts(combPlotData$airReservedVis, s = 1, f = 7)
hrvTS <- ts(combPlotData$hpgReservedVis, s = 1, f = 7)

# Clean the series
arvTSc <- tsclean(log1p(arvTS))
hrvTSc <- tsclean(log1p(hrvTS))
# autoplot(arvTSc) + theme_bw()
# autoplot(hrvTSc) + theme_bw()

combPlotData$arvTSc <- round(exp(arvTSc)-1)
combPlotData$hrvTSc <- round(exp(hrvTSc)-1)


# Plot raw vs cleaned data
combPlotData %>%
  ggplot(aes(visit_date, airReservedVis)) +
  geom_line() +
  geom_smooth(aes(visit_date, airReservedVis),
              method = "loess", color = "darkgreen", span = 0.075) +
  geom_line(aes(y=arvTSc), color = "green") +
  geom_smooth(aes(visit_date, arvTSc),
              method = "loess", color = "green", span = 0.075) +
  geom_line(aes(y=hpgReservedVis)) +
  geom_smooth(aes(visit_date, hpgReservedVis),
              method = "loess", color = "maroon", span = 0.075)+
  geom_line(aes(y=hrvTSc), color = "tomato") +
  geom_smooth(aes(visit_date, hrvTSc),
              method = "loess", color = "tomato", span = 0.075) +
  labs(y = "Total visitors all AIR stores", x = "Date") +
  theme_bw() +
  labs(title = 
         "Total guests reserving using AIR and HPG for AIR stores over time") -> rawVsCleanesTS
rawVsCleanesTS + scale_y_continuous(trans = "log1p")


# Auto arima setup ----
tvTS <- ts(log1p(na.omit(combPlotData$airVisitor)), s = 1, f = 7)
xRegsFit <- log1p(combPlotData[-which(is.na(combPlotData$airVisitor) == TRUE), 
                               c("arvTSc", "hrvTSc")])
xRegsFore <- log1p(combPlotData[which(is.na(combPlotData$airVisitor) == TRUE),
                                c("arvTSc", "hrvTSc")])

arimaFitYK <- auto.arima(tvTS, xreg = xRegsFit)
aaFore <- forecast(arimaFitYK, xreg = xRegsFore)

combPlotData$Fitted <- combPlotData$Forecast <- 
  combPlotData$ForecastLo <- combPlotData$ForecastHi <- NA
combPlotData$Fitted[1:(min(which(is.na(combPlotData$airVisitor)))-1)] <- exp(c(aaFore$fitted))-1
combPlotData$Forecast[which(is.na(combPlotData$airVisitor) == TRUE)] <- exp(c(aaFore$mean))-1
combPlotData$ForecastLo[which(is.na(combPlotData$airVisitor) == TRUE)] <- exp(c(aaFore$lower[,1]))-1
combPlotData$ForecastHi[which(is.na(combPlotData$airVisitor) == TRUE)] <- exp(c(aaFore$upper[,1]))-1



# Plot raw vs cleaned data
combPlotData %>%
  ggplot(aes(visit_date, airVisitor)) +
  geom_line() +
  geom_smooth(aes(visit_date, airVisitor),
              method = "loess", color = "blue", span = 0.075) +
  geom_line(aes(y=Fitted), color = "firebrick") +
  geom_line(aes(y=Forecast), size = 1.2, color = "firebrick") +
  geom_line(aes(y=ForecastLo), size = 1.2, color = "lightblue") +
  geom_line(aes(y=ForecastHi), size = 1.2, color = "lightblue") +
  geom_line(aes(y=airReservedVis)) +
  geom_smooth(aes(visit_date, airReservedVis),
              method = "loess", color = "darkgreen", span = 0.075) +
  geom_line(aes(y=arvTSc), color = "green") +
  geom_smooth(aes(visit_date, arvTSc),
              method = "loess", color = "green", span = 0.075) +
  geom_line(aes(y=hpgReservedVis)) +
  geom_smooth(aes(visit_date, hpgReservedVis),
              method = "loess", color = "maroon", span = 0.075)+
  geom_line(aes(y=hrvTSc), color = "tomato") +
  geom_smooth(aes(visit_date, hrvTSc),
              method = "loess", color = "tomato", span = 0.075) +
  labs(y = "Total visitors all AIR stores", x = "Date") +
  theme_bw() +
  labs(title = 
         "Total guests reserving using AIR and HPG for AIR stores over time") -> ForerawVsCleanesTS
ForerawVsCleanesTS + scale_y_continuous(trans = "log1p")


pb2 <- plotly_build(ForerawVsCleanesTS)
pb2
