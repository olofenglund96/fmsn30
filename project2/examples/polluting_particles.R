# Lecture 7, 27/4-20:
# Logistic regresssion: estimates and confidence intervals

library(ggplot2)

# Introductory example####
load("Data/exampledata.rda")
head(exampledata)

# plot log-odds against x####
ggplot(exampledata, aes(x, logodds, color = model)) +
  geom_line(aes(linetype = model), size = 1.5) +
  labs(title = "log-odds as a function of x",
       color = "log-odds", linetype = "log-odds") +
  xlab("x") + ylab("log-odds = ln p/(1-p)") +
  theme(text = element_text(size = 18))

# plot odds against x####
# Note: lims() cuts the y-axis at 21 (giving warning
# messages for points left out of the plot).
# Try plotting without it and see what happens.

ggplot(exampledata, aes(x, odds, color = model)) +
  geom_line(aes(linetype = model), size = 1.5) +
  labs(title = "odds as a function of x",
       color = "log-odds", linetype = "log-odds") +
  lims(y = c(0, 21)) +
  xlab("x") + ylab("odds = p/(1-p) (odds < 20)") +
  theme(text = element_text(size = 18))

# plot p against x####
ggplot(exampledata, aes(x, p, color = model)) +
  geom_line(aes(linetype = model), size = 1.5) +
  labs(title = "Pr(Y = 1) as a function of x",
       color = "log-odds", linetype = "log-odds") +
  xlab("x") + ylab("p") +
  theme(text = element_text(size = 18))

#particles in Olso####

load("Data/pm10.rda")
head(pm10)
# Some extra variables used in lecture 8.

# highpm10 against number of cars
ggplot(pm10, aes(cars, highpm10)) +
  geom_point() +
  xlab("number of cars") +
  ylab("High PM10") +
  labs(title = "High PM10 (=1) or Not high PM10 (=0) vs number of cars") +
  theme(text = element_text(size = 14))

ggplot(pm10, aes(cars, highpm10)) +
  geom_point() +
  geom_smooth() +
  xlab("number of cars") +
  ylab("High PM10") +
  labs(title = "High PM10 (=1) or Not high PM10 (=0) vs number of cars") +
  theme(text = element_text(size = 14))

#estimate the model####
# Note: I am using cars/1000 as x-variable instead of cars.
# This is both for numerical reasons and for interpretation 
# of parameters.

glm(highpm10 ~ cars, family = "binomial", data = pm10)
(model.1 <- glm(highpm10 ~ I(cars/1000), family = "binomial", data = pm10))
# Note that beta for I(cars/1000) = 1000*beta for cars

summary(model.1)
# Look! our friend AIC.
# But no R2, residual standard error or F-test.
# Instead we have something called "deviance".
# Deviance = -2*loglikelihood. More on this next lecture.

# beta: log-odds(ratio) with c.i.:
model.1$coefficients
(ci.beta <- confint(model.1))

# Odds (exp(beta0)) and OR, odds ratio, exp(beta1)
exp(model.1$coefficients)
(ci.or <- exp(ci.beta))

# predict for plotting####
# phat = estimated probabilities p
pm10.pred <- cbind(
  pm10,
  phat = predict(model.1, type = "response"))

ggplot(pm10.pred, aes(cars, highpm10)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_line(aes(y = phat), color = "red", size = 1) +
  xlab("number of cars") +
  ylab("High PM10") +
  labs(title = "High PM10 (=1) or Not high PM10 (=0) vs number of cars",
       caption = "red = fitted line, blue dashed = moving average") +
  theme(text = element_text(size = 14))

# logit = logodds with s.e. for constructing C.I.
pm10.pred <- cbind(
  pm10.pred,
  logit = predict(model.1, se.fit = TRUE))
head(pm10.pred)
# An unnecessary variable:
pm10.pred$logit.residual.scale <- NULL

# Calculate confidence intervals for the log odds####
# standard normal quantile:
(lambda <- qnorm(1 - 0.05/2))
pm10.pred$logit.lwr <- pm10.pred$logit.fit - lambda*pm10.pred$logit.se.fit
pm10.pred$logit.upr <- pm10.pred$logit.fit + lambda*pm10.pred$logit.se.fit
head(pm10.pred)

# transform the log-odds intervals into C.I. for odds####
pm10.pred$odds.lwr <- exp(pm10.pred$logit.lwr)
pm10.pred$odds.upr <- exp(pm10.pred$logit.upr)
head(pm10.pred)

# transform the odds intervals into C.I. for p####
pm10.pred$p.lwr <- pm10.pred$odds.lwr/(1 + pm10.pred$odds.lwr)
pm10.pred$p.upr <- pm10.pred$odds.upr/(1 + pm10.pred$odds.upr)
head(pm10.pred)

# plot the intervals:
ggplot(pm10.pred, aes(cars, highpm10)) +
  geom_point() +
  geom_line(aes(y = phat), color = "red", size = 1) +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
  xlab("number of cars") +
  ylab("High PM10") +
  labs(title = "High PM10 (=1) or Not high PM10 (=0) vs number of cars",
       caption = "red = fitted line, with 95% confidence interval") +
  theme(text = element_text(size = 14))

# Wald test:
summary(model.1)$coefficients
# Since |4.92| > lambda_0.025 = 1.96 we can reject
# H0: beta_1 = 0
# Alt. Since
# P(|N(0,1)| > 4.92) = 2*P(N(0,1) > 4.92) = 8.7*10^(-7) < 0.05
# we can reject H0.
# The number of cars (or, rather, the number of thousands of cars)
# has a significant impact on the probability of a high
# concentration of PM10-particles.
