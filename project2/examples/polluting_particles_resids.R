# Lecture 8, 28/4-20:
# Logistic regresssion: chi2-test, residuals, pseudo R2

library(ggplot2)

load("Data/pm10.rda")
head(pm10)
summary(pm10)

# Null model####
(model.0 <- glm(highpm10 ~ 1, family = "binomial", data = pm10))
# Model 1: cars####
(model.1 <- glm(highpm10 ~ I(cars/1000), family = "binomial", data = pm10))
# Model.oslo: cars*windspeed + tempdiff####
(model.oslo <- glm(highpm10 ~ I(cars/1000)*windspeed + tempdiff,
                   family = "binomial", data = pm10))
(sum.oslo <- summary(model.oslo))
confint(model.oslo)
# Average Y:
mean(pm10$highpm10)
# or prediction in the null model;
predict(model.0, data.frame(x = NA), type = "response")

# compare with Null model using the summary output:
(D_diff <- sum.oslo$null.deviance - sum.oslo$deviance)
(f_diff <- sum.oslo$df.null - sum.oslo$df.residual)

# compare with Null model using the anova funktion:
(anova.0.oslo <- anova(model.0, model.oslo))
(D_diff <- anova.0.oslo$Deviance[2])
(f_diff <- anova.0.oslo$Df[2])

#chi2-quantile to compare D_diff with:
qchisq(1 - 0.05, f_diff)
# or P-value:
pchisq(D_diff, f_diff, lower.tail = FALSE)

# Compare with model with only tempdiff:
(model.red <- glm(highpm10 ~ tempdiff, family = "binomial", data = pm10))
(sum.red <- summary(model.red))

(D_diff <- sum.red$deviance - sum.oslo$deviance)
(f_diff <- sum.red$df.residual - sum.oslo$df.residual)
# or
(anova.red.oslo <- anova(model.red, model.oslo))
(D_diff <- anova.red.oslo$Deviance[2])
(f_diff <- anova.red.oslo$Df[2])

qchisq(1 - 0.05, f_diff)
pchisq(D_diff, f_diff, lower.tail = FALSE)

# Leverage####
infl.oslo <- influence(model.oslo)
pm10.pred <- cbind(pm10,
                   xbeta = predict(model.oslo),
                   v = infl.oslo$hat)
head(pm10.pred)

(plot.v <- ggplot(pm10.pred, aes(xbeta, v)) + 
    geom_point() +
    geom_hline(yintercept = 2*6/500, color = "red", size = 1) +
    facet_wrap(~ highpm10) +
    labs(title = "Leverage vs linear predictor, by Y=0 or Y=1") +
    theme(text = element_text(size = 14)))

#highlight unusually large ones (arbitrary choice):
I_highv <- which(pm10.pred$v > 0.045)
plot.v +
  geom_point(data = pm10.pred[I_highv, ], size = 3, 
             color = "red", shape = 24) +
  geom_hline(yintercept = 0.045, linetype = "dotted", size = 1) +
  labs(title = "Leverage vs linear predictor, by Y=0 or Y=1",
       caption = "red = 2(p+1)/n, black = 0.045")

ggplot(pm10.pred, aes(cars, v)) + 
  geom_point() +
  geom_point(data = pm10.pred[I_highv, ], size = 3, 
             color = "red", shape = 24) +
  facet_wrap(~ tempdiff) +
  labs(title = "Leverage vs cars, by temp diff",
       caption = "red = 2(p+1)/n, black = 0.045") +
  geom_hline(yintercept = 2*6/500, color = "red", size = 1) +
  geom_hline(yintercept = 0.045, linetype = "dotted", size = 1) +
  theme(text = element_text(size = 14))

ggplot(pm10.pred, aes(windspeed, v)) +
  geom_point() + 
  geom_point(data = pm10.pred[I_highv, ], size = 3, 
             color = "red", shape = 24) +
  facet_wrap(~ tempdiff) +
  geom_hline(yintercept = 2*6/500, color = "red", size = 1) +
  geom_hline(yintercept = 0.045, linetype = "dotted", size = 1) +
  labs(title = "Leverage vs wind speed, by temp diff",
       caption = "red = 2(p+1)/n, black = 0.045") +
  theme(text = element_text(size = 14))

# Use facet_grid to split rows and columns by different variables:
ggplot(pm10.pred, aes(cars, windspeed)) +
  geom_point() +
  geom_point(data = pm10.pred[I_highv, ], color = "red",
             shape = 24, size = 3) +
  facet_grid(rows = vars(highpm10), cols = vars(tempdiff)) +
  labs(title = "wind speed vs cars, by Y=0 or Y=1 and by temp diff",
       caption = "rows: Y = 0 or 1, columns: tempdiff") +
  theme(text = element_text(size = 14))

# Residuals####
# Add pearson residuals and standardize them:
pm10.pred$pearson <- infl.oslo$pear.res
pm10.pred$stdres <- pm10.pred$pearson/sqrt(1 - pm10.pred$v)
head(pm10.pred)

ggplot(pm10.pred, aes(sample = stdres)) +
  geom_qq() + geom_qq_line() +
  labs(title = "Q-Q-plot standardized residuals") +
  theme(text = element_text(size = 14))

# The as.factor(highpm10) prevents ggplot from using a 
# spectrum and instead use default color number 1 and 2.
ggplot(pm10.pred, aes(xbeta, stdres, 
                      color = as.factor(highpm10))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
             size = 1) +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
             size = 1) +
  labs(title = "Standardized residuals vs linear predictor",
       color = "Y") +
  theme(text = element_text(size = 14))

ggplot(pm10.pred, aes(xbeta, stdres^2, color = as.factor(highpm10))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 4, color = "red", linetype = "dashed",
             size = 1) +
  labs(title = "Squared standardized residuals vs linear predictor",
       color = "Y") +
  theme(text = element_text(size = 14))

# deviance residuals, standardised####
pm10.pred$devres <- infl.oslo$dev.res
pm10.pred$devstd <- pm10.pred$devres/sqrt(1 - pm10.pred$v)
head(pm10.pred)

ggplot(pm10.pred, aes(xbeta, devstd, color = as.factor(highpm10))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
             size = 1) +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
             size = 1) +
  labs(title = "Standardized deviance residuals vs linear predictor",
       color = "Y") +
  theme(text = element_text(size = 14))

ggplot(pm10.pred, aes(cars, devstd, color = as.factor(highpm10))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
             size = 1) +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
             size = 1) +
  labs(title = "Standardized deviance residuals vs cars, by temp diff",
       color = "Y") +
  theme(text = element_text(size = 14)) +
  facet_wrap(~ tempdiff)

ggplot(pm10.pred, aes(windspeed, devstd, color = as.factor(highpm10))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed",
             size = 1) +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dotted",
             size = 1) +
  labs(title = "Standardized deviance residuals vs wind speed, by temp diff",
       color = "Y") +
  theme(text = element_text(size = 14)) +
  facet_wrap(~ tempdiff)

ggplot(pm10.pred, aes(sample = devstd)) +
  geom_qq() + geom_qq_line()

# Cook's distance####
pm10.pred$Dcook <- cooks.distance(model.oslo)
head(pm10.pred)

ggplot(pm10.pred, aes(xbeta, Dcook, color = as.factor(highpm10))) +
  geom_point() +
  geom_point(data = pm10.pred[I_highv, ], color = "black",
             shape = 24, size = 3) +
  geom_hline(yintercept = 0) +
  #  geom_hline(yintercept = 1, color = "red", linetype = "dashed",
  #             size = 1) +
  geom_hline(yintercept = 4/500, color = "red", linetype = "dotted",
             size = 1) +
  labs(title = "Cook's distance vs linear predictor, by temp diff",
       color = "Y") +
  theme(text = element_text(size = 14)) +
  facet_grid(rows = vars(highpm10), cols = vars(tempdiff))

#other models####
# Since tempdiff = pos and neg are small groups, both with
# beta-parameters larger than zero, we might want to put them
# together into one group. This is done in the variable zerodiff:
table(pm10$tempdiff, pm10$zerodiff)
# We will try using this versions in some models.
# It might reduce the influence problem.
#
# p = 0:
# Null: model.0
# p = 1:
# cars: model.1
# p = 2:
model.2 <- glm(highpm10 ~ I(cars/1000) + windspeed, family = "binomial", data = pm10)
# tempdiff: model.red
model.3 <- glm(highpm10 ~ I(cars/100) + zerodiff, family = "binomial", data = pm10)
# p = 3:
model.4 <- glm(highpm10 ~ I(cars/1000)*windspeed, family = "binomial", data = pm10)
# backward reduction with BIC:
model.5 <- step(model.oslo, k = log(nrow(pm10)))
# p = 4:
model.6 <- glm(highpm10 ~ I(cars/100)*windspeed + zerodiff, family = "binomial", data = pm10)

bic <- BIC(model.0, model.1, model.2, model.red, model.3, 
           model.4, model.5, model.6, model.oslo)
aic <- AIC(model.0, model.1, model.2, model.red, model.3, 
           model.4, model.5, model.6, model.oslo)
(collect_AIC <- data.frame(aic, bic))

# model 3: with cars and zerodiff is the best (BIC)

# pseudo R2####+
logLik(model.0)
(lnL0 <- logLik(model.0)[1])
(R2CS_max <- 1 - (exp(lnL0))^(2/500))
# Collect the log likelihoods L(betahat)
collect_AIC$loglik <- 
  c(logLik(model.0)[1],
    logLik(model.1)[1],
    logLik(model.2)[1],
    logLik(model.red)[1],
    logLik(model.3)[1],
    logLik(model.4)[1],
    logLik(model.5)[1],
    logLik(model.6)[1],
    logLik(model.oslo)[1])
# calculate R2_CS:
collect_AIC$R2CS <- 1 - (exp(lnL0 - collect_AIC$loglik))^(2/500)
# Canculate R2_N:
collect_AIC$R2N <- collect_AIC$R2CS/R2CS_max

# Show them as % with one decimal value:
round(100*collect_AIC[, c("R2CS", "R2N")], digits = 1)


