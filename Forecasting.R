library(stats)
install.packages("tseries")
install.packages("forecast")
getwd()
dir <- "/Users/laavanyaganesh/Desktop"
setwd(dir)
# Import Data
ts <- read.table("c02.txt", skip = 3, header = F, sep = "")
View(ts)
# Remove exraneous featurs
ts <- ts$V5
# Plot original series
plot.ts(ts, ylab = "Atmospheric Carbon Dioxide (PPM)",
        main = "Atmospheric Carbon Dioxide vs. Time")
# Copy series to new variable
orig <- ts
# Reserve last 10 observations
next10 <- orig[697:706]
# Convert vector to time series object
ts <- as.ts(ts, start=c(1958,3), frequency=12)
ts <- ts[1:696]
# Plot with regression line
linMod <- lm(ts~as.numeric(1:length(ts)))
plot.ts(ts, ylab = "Atmospheric Carbon Dioxide (PPM)",
        main = "Atmospheric Carbon Dioxide vs. Time")
abline(linMod, col = "red")
# Find lambda for Box-Cox transformation
library(MASS)
bcTransform <- boxcox(ts~as.numeric(1:length(ts)), lambda = seq(0,1.5,1/10))
trans <- bcTransform$x[which.max(bcTransform$y)]
# Transform data, plot with regression line
ts.log <- log(ts)
linMod2 <- lm(ts.log~as.numeric(1:length(ts.log)))
plot.ts(ts.log, ylab = "Atmospheric Carbon Dioxide (LogPPM)",
        main = "Log Atmospheric Carbon Dioxide vs. Time")
abline(linMod2, col = "red")
# Difference at lag1, compare variance
ts.log.1 <- diff(ts.log,1)
plot.ts(ts.log.1, ylab = "Diff. Atmospheric Carbon Dioxide (LogPPM)",
        main = "Diff. Log Atmospheric Carbon Dioxide vs. Time")
var.ts.log <- var(ts.log, na.rm = T)
var.ts.log.1 <- var(ts.log.1, na.rm =T)
# Difference at lag12, compare variance
ts.log.1.12 <- diff(ts.log.1,12)
plot.ts(ts.log.1.12, ylab = "Twice Diff. Atmospheric Carbon Dioxide (LogPPM)",
        main = "Twice Diff. Log Atmospheric Carbon Dioxide vs. Time")
abline(h=mean(ts.log.1.12, na.rm = T), col = "red")
var.ts.log.1.12 <- var(ts.log.1.12, na.rm =T)
# KPSS Test for stationarity
library(tseries)
kpss.test(ts.log.1.12)
# Plot ACF, PACF
acf <- acf(ts.log.1.12, type = "correlation",
           plot = T, na.action = na.pass,
           lag.max=12*5, main ="ACF")
pacf <- acf(ts.log.1.12, type = "partial",
            plot = T, na.action = na.pass,
            lag.max=12*5, main = "PACF")
# Build two appropriate models
library(forecast)
model1 <- Arima(ts.log, order = c(0,1,1),
                seasonal = list(order = c(0,1,1), period = 12))
model2 <- Arima(ts.log, order = c(4,1,0),
                seasonal = list(order = c(0,1,1), period = 12))
# Model 1 Diagnostics
plot(model1$residuals, main="Model 1 Residuals")
shapiro.test(model1$residuals)
shapiro.test(model1$residuals[14:706])
hist(model1$residuals, xlim = c(-0.005,0.005),
     main = "Model 1 Residuals", breaks = 50)
qqnorm(model1$residuals)
qqline(model1$residuals)
paste("Box-Pierce")
Box.test(model1$residuals, lag = 26, type = "Box-Pierce", fitdf=2)
paste("Ljung-Box")
Box.test(model1$residuals, lag = 26, type = "Ljung-Box", fitdf=2)
paste("McLeod-Li")
Box.test((model1$residuals)^2, lag=26-2, type="Ljung-Box")
acf(model1$residuals, na.action=na.pass, main = "ACF of Residuals")
pacf(model1$residuals, na.action = na.pass, main = "PACF of Residuals")
# Model 2 Diagnostics
plot(model2$residuals, main="Model 2 Residuals")
shapiro.test(model2$residuals)
shapiro.test(model2$residuals[14:706])
hist(model1$residuals, xlim = c(-0.005,0.005),
     main = "Model 2 Residuals", breaks = 50)
qqnorm(model2$residuals)
qqline(model2$residuals)
paste("Box-Pierce")
Box.test(model2$residuals, lag = 26, type = "Box-Pierce", fitdf=5)
paste("Ljung-Box")
Box.test(model2$residuals, lag = 26, type = "Ljung-Box", fitdf=5)
paste("McLeod-Li")
Box.test((model2$residuals)^2, lag=26-5, type="Ljung-Box")
acf(model2$residuals, na.action=na.pass,
    main = "ACF of Residuals")
pacf(model2$residuals, na.action = na.pass,
     main = "PACF of Residuals")
# Forecasting with Model 1
pred <- predict(model1, n.ahead = 10)
pred.orig <- exp(pred$pred)
pred.se <- exp(pred$pred)*pred$pred*pred$se
plot.ts(orig, xlim = c(680,length(orig)+10), ylim = c(380,420),
        ylab = "Atmospheric CO2 Concentration",
        main = "Atmospheric CO2 Concentration with Forecasts")
points((length(orig)+1):(length(orig)+10),pred.orig, col="red")
points((length(orig)+1):(length(orig)+10),next10, pch = "*")
lines((length(orig)+1):(length(orig)+10),pred.orig+1.96*pred.se,lty=2, col="blue")
lines((length(orig)+1):(length(orig)+10),pred.orig-1.96*pred.se,lty=2, col="blue")