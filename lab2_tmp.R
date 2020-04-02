# Load and init log transformation
load('Data/Pb_mossa.rda')
Pb_mossa$nyear = I(Pb_mossa$year-1975)
summary(Pb_mossa)
Pb_log = Pb_mossa
Pb_log$Pb <- log(Pb_mossa$Pb)
Pb_log$nyear <- Pb_mossa$nyear

# Fit model and extract p
(Pb_log.model <- lm(Pb ~ nyear, data = Pb_log))

summary(Pb_log.model)

# p-value: < 2.2e-16 which is much lower than 0.05 which means H1 is the correct hypothesis => we have a relationship

library(ggplot2)
# Plot values with locations colored and split
ggplot(data =Pb_mossa, 
       aes(x = nyear, y = Pb, color = loc)) + 
  geom_point() +
  facet_wrap(~ loc)

# Plots look exponential with minor discrepancies

# Plot log(values) with locations colored and split
ggplot(data =Pb_log, 
       aes(x = nyear, y = Pb, color = loc)) + 
  geom_point() +
  facet_wrap(~ loc) 
# They now look mostly linear with minor discrepancies

Pb_log.locmodel <- lm(Pb ~ nyear + loc, data = Pb_log) 
summary(Pb_log.locmodel)
# Örebro was used as a reference, I don't know if this is a good idea but it contains few measurements which is bad.

Pb_log$loc <- relevel(Pb_log$loc, "Norrbotten")

ggplot(data =Pb_mossa, 
       aes(x = nyear, y = Pb, color = loc)) + 
  geom_point() +
  facet_wrap(~ loc)

ggplot(data =Pb_log, 
       aes(x = nyear, y = Pb, color = loc)) + 
  geom_point() +
  facet_wrap(~ loc) 

Pb_log.locmodel <- lm(Pb ~ nyear + loc, data = Pb_log) 

summary(Pb_log.locmodel)

###                  Beta       Std. Error
# Norrbotten         2.905491   0.031725  91.584  < 2e-16 ***
# locOrebro          0.736667   14.964
# locJamtland       -0.300911   -8.875
# locVasternorrland  0.225392   5.464
# locVastraGotaland  0.734661   23.068

confint(Pb_log.locmodel)
# Confidence intervals
#                       2.5 %      97.5 %
# Norrbotten         2.84324926  2.96773182
# locOrebro          0.64008329  0.83325018
# locJamtland       -0.36743107 -0.23439106
# locVasternorrland  0.14446405  0.30631969
# locVastraGotaland  0.67217929  0.79714223

exp(confint(Pb_log.locmodel))
# Exponential confidence intervals
#                     2.5 %     97.5 %
# Norrbotten         17.1714695 19.4477586
# locOrebro          1.8966388  2.3007846
# locJamtland        0.6925111  0.7910524
# locVasternorrland  1.1554202  1.3584165
# locVastraGotaland  1.9585008  2.2191899

summary(Pb_log.locmodel)

# beta0 for intercept = 2.9 which is the average?

(Pb_log.x0 <- data.frame(nyear = c(0), loc = "Orebro"))

(
  Pb_log.y0.pred <- 
    cbind(Pb_log.x0, 
          predict(Pb_log.locmodel, Pb_log.x0, se.fit = TRUE),
          conf = predict(Pb_log.locmodel, Pb_log.x0, 
                         interval = "confidence"))
)

# Prediction for Örebro with year = 1975 in log
# predicted value: 3.642157
# confidence interval [3.537919, 3.746396]

exp(Pb_log.y0.pred[,c('fit', 'conf.lwr', 'conf.upr')])
# Prediction for Örebro with year = 1975 in normal transformation
# predicted value: 38.1741
# confidence interval [34.39526, 42.36811]

exp(Pb_log.locmodel$coefficients)
# Ratios between locations, Jämtland seems to have the lowest average levels
# Norrbotten          locOrebro       locJamtland locVasternorrland locVastraGotaland 
# 18.2742057          2.0889608         0.7401436         1.2528136         2.0847746 

summary(Pb_log.locmodel)
#                   p-value
# Norrbotten        < 2e-16
# locOrebro         < 2e-16
# locJamtland       < 2e-16
# locVasternorrland 5.63e-08
# locVastraGotaland < 2e-16

# There seem to be a significant relationship between time and log(Pb). The largest p-value is for Västernorrland
# but it is still much lower than 0.05 (using 95% confidence interval)

anova(Pb_log.locmodel, Pb_log.model)
# p-value < 2.2e-16 indicates that there is no significant difference between starting values
(
  plot.data <- 
    ggplot(data =Pb_log, 
           aes(x = nyear, y = Pb, color = loc)) + 
    geom_point()  +
    facet_wrap(~ loc)
)

Pb_log.pred <- 
  cbind(Pb_log,
        fit = predict(Pb_log.locmodel),
        conf = predict(Pb_log.locmodel,
                       interval = "confidence"),
        pred = predict(Pb_log.locmodel,
                       interval = "prediction"))

(
  plot.conf <- plot.data + 
    geom_smooth(method = lm) +
    labs(caption = "data, fitted line and 95% confidence interval")
)

(
  plot.conf +
    geom_line(data = Pb_log.pred, aes(y = pred.lwr),
              color = "red", linetype = "dashed", size = 1) +
    geom_line(data = Pb_log.pred, aes(y = pred.upr),
              color = "red", linetype = "dashed", size = 1) +
    labs(caption = "data, fitted line, 95% confidence and prediction intervals")
)

# The transformed plots have resonable intervals and fits

(
  plot.data <- 
    ggplot(data =Pb_mossa, 
           aes(x = nyear, y = Pb, color = loc)) + 
    geom_point()  +
    facet_wrap(~ loc)
)

(
  plot.data +
    geom_line(data = Pb_log.pred,
              aes(y = exp(fit)),
              color = "blue", size = 1) +
    geom_ribbon(data = Pb_log.pred,
                aes(ymin = exp(conf.lwr), 
                    ymax = exp(conf.upr)),
                alpha = 0.2) +
    geom_line(data = Pb_log.pred, aes(y = exp(pred.lwr)),
              color = "red", linetype = "dashed", size = 1) +
    geom_line(data = Pb_log.pred, aes(y = exp(pred.upr)),
              color = "red", linetype = "dashed", size = 1) +
    labs(caption = "data, fitted line, 95% confidence and prediction intervals")
)

# Same goes for the original plots

Pb_log.pred$e <- Pb_log.locmodel$residuals
head(Pb_log.pred)

(max.e <- max(abs(Pb_log.pred$e)))
(Pb_log.elims <- c(-max.e, max.e))

ggplot(data = Pb_log.pred, 
       aes(x = nyear, y = e, color = loc)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ loc) +
  expand_limits(y = Pb_log.elims) +
  xlab("Time (years)") +
  ylab("Residual") +
  labs(tag = "A") +
  labs(title = "Residuals vs x-values") +
  theme(text = element_text(size = 18))

ggplot(data = Pb_log.pred, 
       aes(x = pred.fit, y = e, color = loc)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ loc) +
  expand_limits(y = Pb_log.elims) +
  xlab("Time (years)") +
  ylab("Residual") +
  labs(tag = "A") +
  labs(title = "Residuals vs y-hat") +
  theme(text = element_text(size = 18))


ggplot(data = Pb_log.pred, 
       aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))  

# yes, adding locations did improve the residuals