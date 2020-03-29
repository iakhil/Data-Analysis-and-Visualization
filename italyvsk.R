
library('ggplot2')

#Studying COVID-19 cases of Italy, India and South Korea.
skorea = read.csv('southkorea.csv')

#Loading Italy's dataset.
italy = read.csv('italy.csv')

#Loading India's dataset.
india = read.csv('india.csv')

#Loading South Korea's dataset.
sday = skorea[,'S.No']

iday = italy[,'S.No']

inday = india[,'S.No']

incases = india[, 'Cases']
scases = skorea[, 'Cases']

icases = italy[, 'Cases']

#qplot(iday, icases, geom = 'smooth', col = 'red', lwd = 1.2, span = 0.2, xlab = 'Day', ylab = 'Active Cases')

#qplot(sday, scases, geom = 'smooth', col = 'blue', lwd = 1.2, span = 0.2, xlab = 'Day', ylab = 'Active Cases')

plot(iday, icases, xlab = 'Day', ylab = 'Active Cases', xlim = c(1,42))

lines(iday, icases, col = 'red', lwd = 6, xlim = c(1,42))

lines(sday, scases, col = 'blue', lwd = 6, add = TRUE)

lines(inday, incases, col = 'orange', lwd = 6, add = TRUE)

legend(1,39493, legend=c("Italy", "South Korea", "India"),
       col=c("red", "blue", "orange"), lty=6:10, cex=0.8, bg = 'Light Blue')

italy = read.csv('italy.csv')

scatter.smooth(x = italy$Cases, y = italy$Difference, main = 'Total Cases vs New Cases', col = 'red', lwd = 4)
x = italy$Cases
y = italy$Difference
linearMod <- lm(y ~ x, data = italy)

print(linearMod)

summary(linearMod)

set.seed(100)

trainingRow <- sample(1:nrow(italy), 0.8*nrow(italy))

trainingData <- italy[trainingRow, ]

testData <- italy[-trainingRow,]
lmMod <- lm(y ~ x, data = italy)

cases_pred <- predict(lmMod, testData)

summary(lmMod)

AIC(lmMod)

actual_preds <- data.frame(cbind(actuals = y, predicteds = x))

correlation_accuracy <- cor(actual_preds)

print(correlation_accuracy)

head(actual_preds)
plot(x,y, xlab = 'Total Cases', ylab = 'New Cases')

abline(lm(y ~ x), col = 'red', lwd = 4)     

modelSummary = summary(linearMod)

#Finding model coefficients:
coeffs = modelSummary$coefficients 
#Estimate   Std. Error   t value     Pr(>|t|)
#(Intercept) 499.33352901 157.47416692  3.170892 2.914801e-03
#x             0.07873591   0.00487697 16.144434 4.053316e-19

#Finding beta estimate:
beta_estimate = coeffs["x", "Estimate"]
# 0.07873591

#Finding f statistic:
f = modelSummary$fstatistic
#value    numdf    dendf 
#260.6427   1.0000  40.0000 

#Finding AIC and BIC:

aic = AIC(linearMod)
#686.2199

bic = BIC(linearMod)
#691.4329

#p-value = 2.2*10^-16
#Adjusted R-squared value = 0.8636

