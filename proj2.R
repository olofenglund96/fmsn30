load('Data/weather.rda')
library(ggplot2)

(
  plot.data <- 
    ggplot(data = weather, 
           aes(x = temp, y = log(rain)))+ 
    geom_point() +
    labs(title = "Rain vs temp")
)

model.log_rt <- lm(log(rain) ~ temp, data = weather)

(
  plot.data <- plot.data +
    geom_smooth(method = lm)
)

## a)
summary(weather.log_model)

# Pr(<|t|) < 2e-16 show that it is significantly different from 0. 

## b)
(
  plot.tr <- 
    ggplot(data = weather, 
           aes(x = temp, y = rain))+ 
    geom_point() +
    labs(title = "Rain vs temp")
)

(
  plot.tp <- 
    ggplot(data = weather, 
           aes(x = temp, y = pressure))+ 
    geom_point() +
    labs(title = "Temp vs pressure")
)

(
  plot.rp <- 
    ggplot(data = weather, 
           aes(x = log(rain), y = pressure))+ 
    geom_point() +
    labs(title = "Rain vs pressure")
)

## c)

model.log_rtp <- lm(log(rain) ~ temp + pressure, data = weather)

summary(model.log_rtp)

(model.log_rtp.conf <- confint(model.log_rtp))

(
  model.log_rtp.pred <-
    cbind(weather,
          fit = predict(model.log_rtp),
          conf = predict(model.log_rtp,
                         interval = "confidence"),
          pred = predict(model.log_rtp,
                         interval = "prediction"))
)

# Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 61.902608   3.558113   17.40   <2e-16 ***
#  temp         0.040443   0.002603   15.54   <2e-16 ***
#  pressure    -0.057873   0.003518  -16.45   <2e-16 ***

# confidence intervals:
# (Intercept) 54.92106690 68.88414831
# temp         0.03533587  0.04555006
# pressure    -0.06477596 -0.05097015

# the addition is relevant since the t-test results in Pr < 2e-16

## d)

model.log_rtp.pred$e <- model.log_rtp$residuals

(max.e <- max(abs(model.log_rtp.pred$e)))
(weather.elims <- c(-max.e, max.e))

# residuals vs temp
ggplot(data = model.log_rtp.pred, 
       aes(x = temp, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = weather.elims) +
  xlab("Temp") +
  ylab("Residual") +
  labs(tag = "A") +
  labs(title = "Residuals vs temp") +
  theme(text = element_text(size = 18))

# residuals vs pressure
ggplot(data = model.log_rtp.pred, 
       aes(x = pressure, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = weather.elims) +
  xlab("Temp") +
  ylab("Residual") +
  labs(tag = "A") +
  labs(title = "Residuals vs temp") +
  theme(text = element_text(size = 18))

# residuals vs yhat
ggplot(data = model.log_rtp.pred, 
       aes(x = pred.fit, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = weather.elims) +
  xlab("Rain") +
  ylab("Residual") +
  labs(tag = "B") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

# qq
ggplot(data = model.log_rtp.pred, 
       aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

# the residuals have a improved a tiny bit

## e)
exp(model.log_rtp$coefficients['temp']*1) # changed by a factor 1.041272
# now increases with 4-5% / degree, it is substantially higher, yes

## f)
exp(model.log_rtp$coefficients['pressure']*20) # changed by a factor 0.31430

## g)
(weather.x0 <- data.frame(temp = c(5), pressure=c(1000, 1020)))

(
  weather.y0.pred <- 
    cbind(weather.x0, 
          predict(model.log_rtp, weather.x0, se.fit = TRUE),
          pred = predict(model.log_rtp, weather.x0, 
                         interval = "prediction"))
)

(exp(weather.y0.pred$pred.lwr))
(exp(weather.y0.pred$pred.upr))
  
# t = 5, p = 1000: 18.79460 252.13442
# t = 5, p = 1020: 5.91542 79.12645
# when pressure increased, the average precipitation decreased, this is according to f)

## h)
model.log_rtp_i <- lm(log(rain) ~ temp*pressure, data = weather)

summary(model.log_rtp_i)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   61.0413685  3.4905278  17.488  < 2e-16 ***
#   temp           3.2717976  0.4813345   6.797 1.75e-11 ***
#   pressure      -0.0570093  0.0034513 -16.518  < 2e-16 ***
#   temp:pressure -0.0031910  0.0004753  -6.713 3.06e-11 ***

confint(model.log_rtp_i)

#                 2.5 %       97.5 %
# (Intercept)   54.192433567 67.890303441
# temp           2.327347658  4.216247530
# pressure      -0.063781200 -0.050237355
# temp:pressure -0.004123633 -0.002258353

## i)

model.log_rtp_i <- lm(log(rain) ~ temp*I(pressure-1012), data = weather)

## j)

(
  model.log_rtp_i.pred <-
    cbind(weather,
          fit = predict(model.log_rtp_i),
          conf = predict(model.log_rtp_i,
                         interval = "confidence"),
          pred = predict(model.log_rtp_i,
                         interval = "prediction"))
)

model.log_rtp_i.pred$e <- model.log_rtp_i$residuals

(max.e <- max(abs(model.log_rtp_i.pred$e)))
(weather.elims <- c(-max.e, max.e))

# residuals vs temp
ggplot(data = model.log_rtp_i.pred, 
       aes(x = temp, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = weather.elims) +
  xlab("Temp") +
  ylab("Residual") +
  labs(tag = "A") +
  labs(title = "Residuals vs temp") +
  theme(text = element_text(size = 18))

# residuals vs pressure
ggplot(data = model.log_rtp_i.pred, 
       aes(x = pressure, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = weather.elims) +
  xlab("Temp") +
  ylab("Residual") +
  labs(tag = "A") +
  labs(title = "Residuals vs temp") +
  theme(text = element_text(size = 18))

# residuals vs yhat
ggplot(data = model.log_rtp_i.pred, 
       aes(x = pred.fit, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = weather.elims) +
  xlab("Rain") +
  ylab("Residual") +
  labs(tag = "B") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

# qq
ggplot(data = model.log_rtp_i.pred, 
       aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

# the residuals have a improved a tiny bit more

summary(model.log_rtp_i)
## k)
exp(model.log_rtp_i$coefficients['temp:I(pressure - 1012)'] + model.log_rtp_i$coefficients['temp'])
# changed by a factor 1.04

exp((model.log_rtp_i$coefficients['temp:I(pressure - 1012)']+model.log_rtp_i$coefficients['temp:I(pressure - 1012)'])*c(-12, 8) + model.log_rtp_i$coefficients['temp'])
# changes by 1.12 for p = 1000 and 0.99 for p = 1020

(weather.x0 <- data.frame(temp = c(-10, -10, 10, 10), pressure=c(1000, 1020, 1000, 1020)))

(
  weather.y0.pred <- 
    cbind(weather.x0, 
          predict(model.log_rtp_i, weather.x0, se.fit = TRUE),
          pred = predict(model.log_rtp_i, weather.x0, 
                         interval = "prediction"))
)

(exp(weather.y0.pred$fit))

(exp(weather.y0.pred$pred.lwr))
(exp(weather.y0.pred$pred.upr))

# > (exp(weather.y0.pred$fit))
# [1]  25.12955  15.21159 126.48684  21.36500
# > (exp(weather.y0.pred$pred.lwr))
# [1]  6.990012  4.245308 35.214375  5.982919
# > (exp(weather.y0.pred$pred.upr))
# [1]  90.34240  54.50546 454.32929  76.29442

## m)

(table(weather$location))

weather$location <- relevel(weather$loc, "Uppsala")

model.log_rtp_i_loc <- lm(log(rain) ~ temp*I(pressure-1012) + location, data = weather)

summary(model.log_rtp_i_loc)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              3.3913013  0.0264193 128.364  < 2e-16 ***
#   temp                     0.0346847  0.0025092  13.823  < 2e-16 ***
#   I(pressure - 1012)      -0.0658602  0.0033244 -19.811  < 2e-16 ***
#   locationLund             0.3406855  0.0486733   6.999 4.49e-12 ***
#   locationAbisko          -0.5112770  0.0584571  -8.746  < 2e-16 ***
#   temp:I(pressure - 1012) -0.0029452  0.0004537  -6.492 1.29e-10 ***

confint(model.log_rtp_i_loc)
# 2.5 %       97.5 %
#   (Intercept)              3.339462535  3.443140134
# temp                     0.029761372  0.039608100
# I(pressure - 1012)      -0.072383230 -0.059337074
# locationLund             0.245181167  0.436189923
# locationAbisko          -0.625978812 -0.396575285
# temp:I(pressure - 1012) -0.003835445 -0.002055026

# p-test results in p-values as above which means that they are statistically significant

## o)
# The temperature coefficient changed alot by adding locations

(
  model.log_rtp_i_loc.pred <-
    cbind(weather,
          fit = predict(model.log_rtp_i),
          conf = predict(model.log_rtp_i,
                         interval = "confidence"),
          pred = predict(model.log_rtp_i,
                         interval = "prediction"))
)

model.log_rtp_i_loc.pred$e <- model.log_rtp_i_loc$residuals

(max.e <- max(abs(model.log_rtp_i_loc.pred$e)))
(weather.elims <- c(-max.e, max.e))

# residuals vs temp
ggplot(data = model.log_rtp_i_loc.pred, 
       aes(x = temp, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = weather.elims) +
  xlab("Temp") +
  ylab("Residual") +
  labs(tag = "A") +
  labs(title = "Residuals vs temp") +
  theme(text = element_text(size = 18))

# residuals vs pressure
ggplot(data = model.log_rtp_i_loc.pred, 
       aes(x = pressure, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = weather.elims) +
  xlab("Temp") +
  ylab("Residual") +
  labs(tag = "A") +
  labs(title = "Residuals vs temp") +
  theme(text = element_text(size = 18))

# residuals vs yhat
ggplot(data = model.log_rtp_i_loc.pred, 
       aes(x = pred.fit, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = weather.elims) +
  xlab("Rain") +
  ylab("Residual") +
  labs(tag = "B") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

# qq
ggplot(data = model.log_rtp_i_loc.pred, 
       aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

# the residuals have a improved more, especially in the top right part of the plot

## q)
# since lund has the largest positive coefficients (always increasing), lund should also have the highest amount of
# precipitation

max(weather[(weather$location == "Uppsala"), "rain"])
max(weather[(weather$location == "Lund"), "rain"])
max(weather[(weather$location == "Abisko"), "rain"])
