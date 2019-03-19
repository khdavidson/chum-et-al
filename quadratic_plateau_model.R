###
# Fit quadratic plateau model 
###
fit.lm = lm(current ~ discharge, data=cd[cd$bay=="B2",])

a.ini = fit.lm$coefficients[1]
b.ini = fit.lm$coefficients[2]
c.ini = 0.01
dis.ini = mean(cd[cd$bay=="B2",]$discharge)

# Define quadratic plateau function
expon = function(x, a, b, c) {
                 a + b * exp(-1 * c * (x - D))}

# Find best fit parameters
model = nls(current ~ expon(discharge, a, b, c), 
            data = cd[cd$bay=="B2",], 
            start = list(a = a.ini, 
                         b = b.ini, 
                         c = c.ini),
             trace = FALSE,
             nls.control(maxiter = 10000))
summary(model)

# Define null model
nullfunct = function(x, m){m}
m.ini = mean(cd[cd$bay=="B2",]$current, na.rm=T)
null = nls(current ~ nullfunct(discharge, m), 
           data = cd[cd$bay=="B2",], 
           start = list(m = m.ini),
           trace = FALSE,
           nls.control(maxiter = 1000))

# Find p-value and pseudo R-squared
library(rcompanion)
nagelkerke(model, null)

#Determine confidence intervals for parameter estimates
library(nlstools)
confint2(model,
         level = 0.95)

plotPredy(data  = cd[cd$bay=="B2",],
          x     = discharge,
          y     = current,
          model = model,
          xlab  = "discharge",
          ylab  = "current")

x = residuals(model)
plotNormalHistogram(x)
plot(fitted(model), residuals(model))

