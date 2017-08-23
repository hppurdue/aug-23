rain <- read.csv(file.choose(),header = TRUE)
#For reproducibility
set.seed(120)
# pick random 70% observations
index <- sample(1:nrow(rain),size = 0.7*nrow(rain))
#subset rain to include only the elements in the index
train <- rain[index,]
#subset rain to include only the elements all but not in index
test <- rain[-index,]

group <- rep(NA,366)
group <- ifelse(seq(1,366) %in% index,"Train","Test")
data_frame <- data.frame(date=rain$Date,rain=rain$RISK_MM,group)


mo1 <- lm(log(rain$RISK_MM+1)~rain$MinTemp + rain$MaxTemp + rain$Rainfall
          + rain$Evaporation + rain$Sunshine + rain$WindGustDir + rain$WindGustSpeed
          + rain$WindDir9am + rain$WindDir3pm + rain$WindSpeed9am + rain$WindSpeed3pm
          + rain$Humidity9am + rain$Humidity3pm + rain$Pressure9am + rain$Pressure3pm
          + rain$Cloud9am + rain$Cloud3pm + rain$Temp3pm + rain$RainToday, data=train)
# removed maxtemp as pvalue= 0.94906

mo2 <- lm(log(rain$RISK_MM+1)~rain$MinTemp + rain$Rainfall
          + rain$Evaporation + rain$Sunshine + rain$WindGustDir + rain$WindGustSpeed
          + rain$WindDir9am + rain$WindDir3pm + rain$WindSpeed9am + rain$WindSpeed3pm
          + rain$Humidity9am + rain$Humidity3pm + rain$Pressure9am + rain$Pressure3pm
          + rain$Cloud9am + rain$Cloud3pm + rain$Temp3pm + rain$RainToday, data=train)
# removed temp3pm

mo3 <- lm(log(rain$RISK_MM+1)~rain$MinTemp + rain$Rainfall
          + rain$Evaporation + rain$Sunshine + rain$WindGustDir + rain$WindGustSpeed
          + rain$WindDir9am + rain$WindDir3pm + rain$WindSpeed9am + rain$WindSpeed3pm
          + rain$Humidity9am + rain$Humidity3pm + rain$Pressure9am + rain$Pressure3pm
          + rain$Cloud9am + rain$Cloud3pm + rain$RainToday, data=train)

# removed RainToday

mo4 <- lm(log(rain$RISK_MM+1)~rain$MinTemp + rain$Rainfall
          + rain$Evaporation + rain$Sunshine + rain$WindGustDir + rain$WindGustSpeed
          + rain$WindDir9am + rain$WindDir3pm + rain$WindSpeed9am + rain$WindSpeed3pm
          + rain$Humidity9am + rain$Humidity3pm + rain$Pressure9am + rain$Pressure3pm
          + rain$Cloud9am + rain$Cloud3pm, data=train )

# removed WindDir3pm

mo5 <- lm(log(rain$RISK_MM+1)~rain$MinTemp + rain$Rainfall
          + rain$Evaporation + rain$Sunshine + rain$WindGustDir + rain$WindGustSpeed
          + rain$WindDir9am + rain$WindSpeed9am + rain$WindSpeed3pm
          + rain$Humidity9am + rain$Humidity3pm + rain$Pressure9am + rain$Pressure3pm
          + rain$Cloud9am + rain$Cloud3pm, data=train )

# removed cloud3pm

mo6 <- lm(log(rain$RISK_MM+1)~rain$MinTemp + rain$Rainfall
          + rain$Evaporation + rain$Sunshine + rain$WindGustDir + rain$WindGustSpeed
          + rain$WindDir9am + rain$WindSpeed9am + rain$WindSpeed3pm
          + rain$Humidity9am + rain$Humidity3pm + rain$Pressure9am + rain$Pressure3pm
          + rain$Cloud9am, data=train)

# removed rainfall

mo7 <- lm(log(rain$RISK_MM+1)~rain$MinTemp
          + rain$Evaporation + rain$Sunshine + rain$WindGustDir + rain$WindGustSpeed
          + rain$WindDir9am + rain$WindSpeed9am + rain$WindSpeed3pm
          + rain$Humidity9am + rain$Humidity3pm + rain$Pressure9am + rain$Pressure3pm
          + rain$Cloud9am, data=train)


# removed windspeed9am

mo8 <- lm(log(rain$RISK_MM+1)~rain$MinTemp
          + rain$Evaporation + rain$Sunshine + rain$WindGustDir + rain$WindGustSpeed
          + rain$WindDir9am + rain$WindSpeed3pm
          + rain$Humidity9am + rain$Humidity3pm + rain$Pressure9am + rain$Pressure3pm
          + rain$Cloud9am, data=train)

# removed humidity9am

mo9 <- lm(log(rain$RISK_MM+1)~rain$MinTemp
          + rain$Evaporation + rain$Sunshine + rain$WindGustDir + rain$WindGustSpeed
          + rain$WindDir9am + rain$WindSpeed3pm
          + rain$Humidity3pm + rain$Pressure9am + rain$Pressure3pm
          + rain$Cloud9am, data=train)

# removed cloud9am

mo10 <- lm(log(rain$RISK_MM+1)~rain$MinTemp
           + rain$Evaporation + rain$Sunshine + rain$WindGustDir + rain$WindGustSpeed
           + rain$WindDir9am + rain$WindSpeed3pm
           + rain$Humidity3pm + rain$Pressure9am + rain$Pressure3pm
           , data=train)

# removed evaporation

mo11 <- lm(log(rain$RISK_MM+1)~rain$MinTemp + rain$Sunshine + rain$WindGustDir
           + rain$WindGustSpeed + rain$WindDir9am + rain$WindSpeed3pm + rain$Humidity3pm
           + rain$Pressure9am + rain$Pressure3pm, data=train)

plot(predict(mo11),resid(mo11),xlab="predicted",ylab="residual")




