

#Loading air quality data.


air = read.csv('newdelhiair2.csv')

library(ggplot2)

aqi = air[,'AQI']

day = air[,'Day']


hour = air[,'Hour']
month = air[, 'Month']

#Plotting a frequency polygon.
ggplot(data = air, aes(x = month, y = aqi), span = 14, col = 'red')+geom_polygon()

qplot(hour, aqi, geom = 'smooth', span = 0.75, lwd = 2)


shapiro.test(aqi)

hist(aqi, xlab = 'AQI', border = 'gray', col = 'orange', main = 'AQI')

summary(aqi)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#66.0   167.0   184.0   202.0   219.2   574.0 




#Vectorizing raw concentration.

raw_conc = air[,'Raw.Conc.']

#Histogram of raw concentration.

hist(raw_conc, border = 'gray', col = 'red', xlab = 'Raw Concentration')



#Plot between time and raw concentration. 

qplot(hour, raw_conc, geom = 'smooth', span = 0.75, lwd = 2, col = 'green', xlab = 'Time in 24 hrs format', ylab = 'Raw Concentration')





