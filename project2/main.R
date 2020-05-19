load("weather.rda")

## 1a)
weather$lowrain <- as.numeric(weather$rain < 25)
plow = mean(weather$lowrain)
phigh = 1 - plow

odds = plow / phigh
# 0.42

## 1b)
(model.1 <- glm(lowrain ~ 1, family = "binomial", data = weather))

# b0 = -0.9105
confint(model.1)
#    2.5 %    97.5 % 
#-1.042988 -0.780509

odds_new = exp(model.1$coefficients)
# 0.42, same
model.1.b0.confint = exp(confint(model.1))
#     2.5 %    97.5 % 
# 0.3524002 0.4581728

plow_new = exp(-0.9105) / (1 + exp(-0.9105))
# same as before

(plow_confint = model.1.b0.confint / (1 + model.1.b0.confint))
#    2.5 %    97.5 % 
# 0.2605739 0.3142102

## 2a)
library(ggplot2)
ggplot(weather, aes(temp, lowrain)) +
  geom_point() +
  geom_smooth(method = loess) +
  xlab("temp") +
  ylab("lowrain") +
  labs(title = "Lowrain vs temp") +
  theme(text = element_text(size = 14))
# seems to look s-shaped which is good

## 2b)
(model.2 <- glm(lowrain ~ temp, family = "binomial", data = weather))

#Coefficients:
# (Intercept)         temp  
#    -0.58860     -0.07386

confint(model.2)
#                  2.5 %      97.5 %
#(Intercept) -0.73897322 -0.43949978
#temp        -0.09218264 -0.05599002

exp(model.2$coefficients)
#(Intercept)        temp 
#0.5551015   0.9288000

exp(confint(model.2))
#                2.5 %    97.5 %
#(Intercept) 0.4776041 0.6443587
#temp        0.9119386 0.9455486

summary(model.2)
# pr < 1.18e-15, significant

# +1 degree changes with exp(b1) = 0.9288000 (decrease in about 7%)
# -1 degree changes with exp(-b1)
exp(-(-0.07386)) # = 1.076656 (increase in about 8%)


## 2c)
(weather.x0 <- data.frame(temp = c(-10, -9, 9, 10)))

(
  weather.y0.pred <- 
    cbind(weather.x0,
          predict(model.2, weather.x0, se.fit = TRUE)
          )
)

(pred_prob = cbind(weather.x0, fit = exp(weather.y0.pred$fit) / (1 + exp(weather.y0.pred$fit))))

(pred_prob.conf <- 
  cbind(pred_prob,
        data.frame(conf.lwr = exp(weather.y0.pred$fit - 1.96*weather.y0.pred$se.fit) / (1 + exp(weather.y0.pred$fit - 1.96*weather.y0.pred$se.fit)), 
                   conf.upr = exp(weather.y0.pred$fit + 1.96*weather.y0.pred$se.fit) / (1 + exp(weather.y0.pred$fit + 1.96*weather.y0.pred$se.fit))) 
        ))

# temp       fit  conf.lwr  conf.upr
# 1  -10 0.5374333 0.4677144 0.6057185
# 2   -9 0.5190288 0.4532568 0.5841479
# 3    9 0.2221189 0.1944462 0.2524955
# 4   10 0.2096192 0.1814557 0.2408675
# the derivative is higher in the center of the s-curve


## 2d)
weather.pred <- 
  cbind(weather,
        predict(model.2, se.fit = TRUE)
  )

weather.pred_prob = cbind(weather, fit = exp(weather.pred$fit) / (1 + exp(weather.pred$fit)))

weather.pred_prob.conf <- 
    cbind(weather.pred_prob,
          data.frame(conf.lwr = exp(weather.pred$fit - 1.96*weather.pred$se.fit) / (1 + exp(weather.pred$fit - 1.96*weather.pred$se.fit)), 
                     conf.upr = exp(weather.pred$fit + 1.96*weather.pred$se.fit) / (1 + exp(weather.pred$fit + 1.96*weather.pred$se.fit))) 
    )

ggplot(weather.pred, aes(temp, lowrain)) +
  geom_point() +
  geom_smooth(method = loess) +
  geom_line(data = weather.pred_prob, aes(y = fit, x = temp)) +
  geom_ribbon(data = weather.pred_prob.conf, aes(ymin = conf.lwr, ymax = conf.upr, fill = "confidence"), alpha = 0.3) +
  xlab("temp") +
  ylab("lowrain") +
  labs(title = "Lowrain vs temp (blue) with predictions and confidence intervals (red)") +
  theme(text = element_text(size = 14))


## 2e)
weather.pred$v <- influence(model.2)$hat

ggplot(data = weather.pred, aes(x = temp, y = v)) +
  geom_jitter(width = 1) +
  geom_line(data = weather.pred, aes(y = 1/nrow(weather)), color = "black") +
  geom_line(data = weather.pred, aes(y = 2*3/nrow(weather)), color = "red") +
  geom_line(data = weather.pred, aes(y = 0), color = "white", alpha = 0) +
  labs(caption = "y = 1/n (black) and 2(p+1)/n (red)", title = "Leverage vs temp") +
  theme(text = element_text(size = 18)) +
  geom_hline(yintercept = 0, color = NA)
# observations in the beginning have high leverage

## 2f)
weather.pred$dr <- influence(model.2)$dev.res / sqrt(1-weather.pred$v)
ggplot(data = weather.pred, aes(x = temp, y = dr)) +
  geom_jitter(width = 1) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red") +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dashed") +
  labs(caption = "Residuals with lines at +-2,4", title = "Deviance residuals vs temp") +
  theme(text = element_text(size = 18)) +
  geom_hline(yintercept = 0, color = NA)
# no alarming residuals

## 2g)
weather.pred$cd <- cooks.distance(model.2)

ggplot(weather.pred, aes(x = temp, y = cd, color = as.factor(lowrain))) + 
  geom_point() +
  geom_hline(yintercept = 4/nrow(weather), color = "red") +
  xlab("temp") +
  ylab("D_i") +
  labs(title = "Cook's D vs temp", color = 'Lowrain') +
  labs(caption = "y = 4/n") +
  theme(text = element_text(size = 18))
# ones with high temperature have high cooks d
# no, no the same

## 3a)
ggplot(weather, aes(pressure, lowrain)) +
  geom_point() +
  geom_smooth(method = loess) +
  xlab("pressure") +
  ylab("lowrain") +
  labs(title = "Lowrain vs pressure") +
  theme(text = element_text(size = 14))

(model.3 <- glm(lowrain ~ I(pressure-1012), family = "binomial", data = weather))

# (Intercept)  I(pressure - 1012)  
# -1.0229              0.1312
confint(model.3)
# 2.5 %     97.5 %
#   (Intercept)        -1.1692893 -0.8807596
# I(pressure - 1012)  0.1035733  0.1600229

exp(model.3$coefficients)
# (Intercept) I(pressure - 1012) 
# 0.3595352          1.1401855

exp(confint(model.3))
#                     2.5 %   97.5 %
#   (Intercept)        0.3105876 0.414468
# I(pressure - 1012) 1.1091271 1.173538

summary(model.3)
# pr <2e-16, significant

weather.pred.3 <- 
  cbind(weather,
        predict(model.3, se.fit = TRUE)
  )

weather.pred_prob.3 = cbind(weather, fit = exp(weather.pred.3$fit) / (1 + exp(weather.pred.3$fit)))

weather.pred_prob.conf.3 <- 
  cbind(weather.pred_prob.3,
        data.frame(conf.lwr = exp(weather.pred.3$fit - 1.96*weather.pred.3$se.fit) / (1 + exp(weather.pred.3$fit - 1.96*weather.pred.3$se.fit)), 
                   conf.upr = exp(weather.pred.3$fit + 1.96*weather.pred.3$se.fit) / (1 + exp(weather.pred.3$fit + 1.96*weather.pred.3$se.fit))) 
  )

ggplot(weather.pred.3, aes(pressure, lowrain)) +
  geom_point() +
  geom_smooth(method = loess) +
  geom_line(data = weather.pred_prob.3, aes(y = fit, x = pressure)) +
  geom_ribbon(data = weather.pred_prob.conf.3, aes(ymin = conf.lwr, ymax = conf.upr, fill = "confidence"), alpha = 0.3) +
  xlab("temp") +
  ylab("lowrain") +
  labs(title = "Lowrain vs pressure (blue) with predictions and confidence interval (red)") +
  theme(text = element_text(size = 14))

## 3b)
weather.pred.3$v <- influence(model.3)$hat

ggplot(data = weather.pred.3, aes(x = pressure, y = v)) +
  geom_jitter(width = 1) +
  geom_line(data = weather.pred.3, aes(y = 1/nrow(weather)), color = "black") +
  geom_line(data = weather.pred.3, aes(y = 2*3/nrow(weather)), color = "red") +
  geom_line(data = weather.pred.3, aes(y = 0), color = "white", alpha = 0) +
  labs(caption = "y = 1/n (black) and 2(p+1)/n (red)", title = "Leverage vs pressure") +
  theme(text = element_text(size = 18)) +
  geom_hline(yintercept = 0, color = NA)
# observations with large pressure have high leverages

## 3c)
weather.pred.3$dr <- influence(model.3)$dev.res / sqrt(1-weather.pred.3$v)
ggplot(data = weather.pred.3, aes(x = pressure, y = dr)) +
  geom_jitter(width = 1) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red") +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dashed") +
  labs(caption = "Residuals with lines at +-2,4", title = "Deviance residuals vs pressure") +
  theme(text = element_text(size = 18)) +
  geom_hline(yintercept = 0, color = NA)
# no alarming residuals, but somewhat large for small x

## 3d)
weather.pred.3$cd <- cooks.distance(model.3)

ggplot(weather.pred.3, aes(x = pressure, y = cd, color = as.factor(lowrain))) + 
  geom_point() +
  geom_hline(yintercept = 4/nrow(weather), color = "red") +
  xlab("pressure") +
  ylab("D_i") +
  labs(title = "Cook's D vs pressure",  color = 'Lowrain') +
  labs(caption = "y = 4/n") +
  theme(text = element_text(size = 18))
# ones with low pressure have high Cooks d
# no, no the same

## 3e)
# temp seems best since it has the lowest standardized deviances and similar cooks d and leverages

## 3f)
(lnL0 <- logLik(model.1)[1])
(R2CS_max <- 1 - (exp(lnL0))^(2/nrow(weather)))
(loglik = data.frame(model2=logLik(model.2)[1], model3=logLik(model.3)[1]))

(R2CS <- 1 - (exp(lnL0 - loglik))^(2/nrow(weather)))
# model2     model3
# 1 0.06143153 0.08749883
(R2N <- R2CS/R2CS_max)
# model2    model3
# 1 0.08795892 0.1252826

## 3g)
(aic.2 = AIC(model.2))
(aic.3 = AIC(model.3))

(bic.2 = BIC(model.2))
(bic.3 = BIC(model.3))

# model.3 was best

## 4a)
weather$location <- relevel(weather$location, "Uppsala")
(model.4 <- glm(lowrain ~ I(pressure-1012)*temp + location, family = "binomial", data = weather))

anova.2.4 <- anova(model.2, model.4)
anova.3.4 <- anova(model.3, model.4)
(D_diff <- anova.2.4$Deviance[2])
(f_diff <- anova.2.4$Df[2])

#chi2-quantile to compare D_diff with:
qchisq(1 - 0.05, f_diff)
# or P-value:
pchisq(D_diff, f_diff, lower.tail = FALSE)
# P = 3.166005e-49 < 0.05

(D_diff <- anova.3.4$Deviance[2])
(f_diff <- anova.3.4$Df[2])

#chi2-quantile to compare D_diff with:
qchisq(1 - 0.05, f_diff)
# or P-value:
pchisq(D_diff, f_diff, lower.tail = FALSE)
# P = 1.295569e-42 < 0.05

## 4b)
weather.pred.4 <- 
  cbind(weather,
        predict(model.4, se.fit = TRUE)
  )
weather.pred_prob.4 = cbind(weather, fit = exp(weather.pred.4$fit) / (1 + exp(weather.pred.4$fit)))

ggplot(weather.pred_prob.4, aes(temp, lowrain, location)) +
  geom_point() +
  #geom_smooth(method = loess) +
  geom_point(data = weather.pred_prob.4, aes(y = fit, x = temp)) +
  #geom_ribbon(data = weather.pred_prob.conf.3, aes(ymin = conf.lwr, ymax = conf.upr, fill = "band"), alpha = 0.3) +
  facet_wrap( ~location ) +
  geom_point(aes(y = fit, color = pressure)) +
  scale_color_viridis_c() +
  xlab("temp") +
  ylab("lowrain") +
  labs(title = "Lowrain vs temp for different locations") +
  theme(text = element_text(size = 14))

ggplot(weather.pred_prob.4, aes(pressure, lowrain, location)) +
  geom_point() +
  #geom_smooth(method = loess) +
  geom_point(data = weather.pred_prob.4, aes(y = fit, x = pressure)) +
  #geom_ribbon(data = weather.pred_prob.conf.3, aes(ymin = conf.lwr, ymax = conf.upr, fill = "band"), alpha = 0.3) +
  facet_wrap( ~location ) +
  geom_point(aes(y = fit, color = temp)) +
  scale_color_viridis_c() +
  xlab("pressure") +
  ylab("lowrain") +
  labs(title = "Lowrain vs pressure for different locations") +
  theme(text = element_text(size = 14))

# temperature seems to add the largest variability
# abisko

## 4c)
# pressure seems best to predict

weather.lund <- weather[(weather$location == 'Lund'),]

(model.lund <- glm(lowrain ~ I(pressure-1012)*temp, family = "binomial", data = weather.lund))

# Coefficients:
#   (Intercept)       I(pressure - 1012)                     temp  I(pressure - 1012):temp  
# -1.95350                  0.27579                 -0.15036                  0.01793 
confint(model.lund)
#                               2.5 %      97.5 %
#   (Intercept)             -2.90102713 -1.19169302
# I(pressure - 1012)       0.15599432  0.42524011
# temp                    -0.28713294 -0.02788756
# I(pressure - 1012):temp -0.00357026  0.04072037

exp(model.lund$coefficients)
# (Intercept)      I(pressure - 1012)                    temp I(pressure - 1012):temp 
# 0.1417772               1.3175741               0.8603977               1.0180886 

exp(confint(model.lund))
#                             2.5 %    97.5 %
#   (Intercept)             0.05496673 0.3037066
# I(pressure - 1012)      1.16881956 1.5299577
# temp                    0.75041197 0.9724977
# I(pressure - 1012):temp 0.99643611 1.0415608

summary(model.lund)
# pressure seems significant while temp seems not very significant but P = 0.0221 < 0.05 and the interaction
# seems not very significant (P = 0.1096 < 0.05)

step(model.lund, k = log(nrow(weather.lund)))
# Final model coeffs agree with us:
#   (Intercept)  I(pressure - 1012)  
# -2.8129              0.3501

## 4d)
weather.pred.4$v <- influence(model.4)$hat
weather.pred.4$dr <- influence(model.4)$dev.res / sqrt(1-weather.pred.4$v)

ggplot(weather.pred.4, aes(fit, dr)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red") +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dashed") +
  labs(caption = "Residuals with lines at +-2,4", title = "Deviance residuals vs predicted values") +
  theme(text = element_text(size = 14))

ggplot(weather.pred.4, aes(temp, dr, location)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red") +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dashed") +
  facet_wrap( ~location ) +
  geom_point(aes(y = dr, color = pressure)) +
  scale_color_viridis_c() +
  xlab("temp") +
  ylab("lowrain") +
  labs(caption = "Residuals with lines at +-2,4", title = "Deviance residuals vs temperature for different locations") +
  theme(text = element_text(size = 14))

ggplot(weather.pred.4, aes(pressure, dr, location)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red") +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dashed") +
  facet_wrap( ~location ) +
  geom_point(aes(y = dr, color = temp)) +
  scale_color_viridis_c() +
  xlab("pressure") +
  ylab("lowrain") +
  labs(caption = "Residuals with lines at +-2,4", title = "Deviance residuals vs pressure for different locations") +
  theme(text = element_text(size = 14))

# no problematic residuals
# they have improved a bit

## 4e)
weather.pred.4$cd <- cooks.distance(model.4)

ggplot(weather.pred.4, aes(fit, cd)) +
  geom_point() +
  geom_hline(yintercept = 4/nrow(weather), color = "red") +
  xlab("fit") +
  ylab("D_i") +
  labs(title = "Cook's D vs predictions") +
  labs(caption = "y = 4/n") +
  theme(text = element_text(size = 14))

ggplot(weather.pred.4, aes(temp, cd, location)) +
  geom_hline(yintercept = 4/nrow(weather), color = "red") +
  facet_wrap( ~location ) +
  geom_point(aes(y = cd, color = pressure)) +
  scale_color_viridis_c() +
  xlab("temp") +
  ylab("cooks d") +
  labs(title = "Cook's D vs temperature for different locations") +
  labs(caption = "y = 4/n") +
  theme(text = element_text(size = 14))

ggplot(weather.pred.4, aes(pressure, cd, location)) +
  geom_hline(yintercept = 4/nrow(weather), color = "red") +
  facet_wrap( ~location ) +
  geom_point(aes(y = cd, color = temp)) +
  scale_color_viridis_c() +
  xlab("pressure") +
  ylab("cooks d") +
  labs(title = "Cook's D vs pressure for different locations") +
  labs(caption = "y = 4/n") +
  theme(text = element_text(size = 14))

# cooks D have somewhat improved with fewer extreme points
# however abisko seems to have som problematic points

## 4f)
(lnL0 <- logLik(model.1)[1])
(R2CS_max <- 1 - (exp(lnL0))^(2/nrow(weather)))
(loglik.4 = logLik(model.4)[1])

(R2CS <- 1 - (exp(lnL0 - loglik.4))^(2/nrow(weather)))
# 0.2418358
(R2N <- R2CS/R2CS_max)
# 0.3462654

# they have improved alot

## 4g)
(aic.4 = AIC(model.4))

(bic.4 = BIC(model.4))

# they have also improved

## 5a)
weather.pred_prob$expect_lowrain <- as.numeric(weather.pred_prob$fit > 0.5)
weather.pred_prob.3$expect_lowrain <- as.numeric(weather.pred_prob.3$fit > 0.5)
weather.pred_prob.4$expect_lowrain <- as.numeric(weather.pred_prob.4$fit > 0.5)

confusion_params <- function(fit, actual, ...) {
  tp = sum((fit + actual) == 2)
  tn = sum((fit + actual) == 0)
  fp = sum((fit - actual) == 1)
  fn = sum((fit - actual) == -1)
  
  return(c(tp, tn, fp, fn))
}

tp = sum((weather.pred_prob$expect_lowrain + weather.pred_prob$lowrain) == 2)
tn = sum((weather.pred_prob$expect_lowrain + weather.pred_prob$lowrain) == 0)
fp = sum((weather.pred_prob$expect_lowrain - weather.pred_prob$lowrain) == 1)
fn = sum((weather.pred_prob$expect_lowrain - weather.pred_prob$lowrain) == -1)

params.2 = confusion_params(weather.pred_prob$expect_lowrain, weather.pred_prob$lowrain)
params.3 = confusion_params(weather.pred_prob.3$expect_lowrain, weather.pred_prob.3$lowrain)
params.4 = confusion_params(weather.pred_prob.4$expect_lowrain, weather.pred_prob.4$lowrain)

(sensitivity.2 <- params.2[1] / (params.2[4] + params.2[1]))
(sensitivity.3 <- params.3[1] / (params.3[4] + params.3[1]))
(sensitivity.4 <- params.4[1] / (params.4[4] + params.4[1]))

(specificity.2 <- params.2[2] / (params.2[2] + params.2[3]))
(specificity.3 <- params.3[2] / (params.3[2] + params.3[3]))
(specificity.4 <- params.4[2] / (params.4[2] + params.4[3]))

(accuracy.2 <- (params.2[1] + params.2[2]) / (sum(params.2)))
(accuracy.3 <- (params.3[1] + params.3[2]) / (sum(params.3)))
(accuracy.4 <- (params.4[1] + params.4[2]) / (sum(params.4)))

(precision.2 <- params.2[1] / (params.2[3] + params.2[1]))
(precision.3 <- params.3[1] / (params.3[3] + params.3[1]))
(precision.4 <- params.4[1] / (params.4[3] + params.4[1]))

(sensitivities = cbind(sensitivity.2, sensitivity.3, sensitivity.4))
(specificities = cbind(specificity.2, specificity.3, specificity.4))
(accuracies = cbind(accuracy.2, accuracy.3, accuracy.4))
(precisions = cbind(precision.2, precision.3, precision.4))

# no model is outperforming in all of the measurements but 4 seems to be the one with the overall best scores

## 5b)
library(pROC)
roc.2 <- roc(lowrain ~ fit, data = weather.pred_prob)
roc.3 <- roc(lowrain ~ fit, data = weather.pred_prob.3)
roc.4 <- roc(lowrain ~ fit, data = weather.pred_prob.4)
ggroc(list(temp=roc.2, pressure=roc.3, all=roc.4))
ggroc(roc.3)
ggroc(roc.4)

auc(roc.2) # 0.654
auc(roc.3) # 0.6984
auc(roc.4) # 0.8143

ci(roc.2) # 0.6177-0.6903
ci(roc.3) # 0.6619-0.735
ci(roc.4) # 0.7873-0.8414

roc.test(roc.2, roc.3)
# p-value = 0.1261 not significantly better

## 5c)
t.2 = coords(roc.2, "best", ret = "threshold", transpose = FALSE)
t.3 = coords(roc.3, "best", ret = "threshold", transpose = FALSE)
t.4 = coords(roc.4, "best", ret = "threshold", transpose = FALSE)

weather.pred_prob$expect_lowrain <- as.numeric(weather.pred_prob$fit > t.2$threshold)
weather.pred_prob.3$expect_lowrain <- as.numeric(weather.pred_prob.3$fit > t.3$threshold)
weather.pred_prob.4$expect_lowrain <- as.numeric(weather.pred_prob.4$fit > t.4$threshold)

tp = sum((weather.pred_prob$expect_lowrain + weather.pred_prob$lowrain) == 2)
tn = sum((weather.pred_prob$expect_lowrain + weather.pred_prob$lowrain) == 0)
fp = sum((weather.pred_prob$expect_lowrain - weather.pred_prob$lowrain) == 1)
fn = sum((weather.pred_prob$expect_lowrain - weather.pred_prob$lowrain) == -1)

params.2 = confusion_params(weather.pred_prob$expect_lowrain, weather.pred_prob$lowrain)
params.3 = confusion_params(weather.pred_prob.3$expect_lowrain, weather.pred_prob.3$lowrain)
params.4 = confusion_params(weather.pred_prob.4$expect_lowrain, weather.pred_prob.4$lowrain)

(sensitivity.2 <- params.2[1] / (params.2[4] + params.2[1]))
(sensitivity.3 <- params.3[1] / (params.3[4] + params.3[1]))
(sensitivity.4 <- params.4[1] / (params.4[4] + params.4[1]))

(specificity.2 <- params.2[2] / (params.2[2] + params.2[3]))
(specificity.3 <- params.3[2] / (params.3[2] + params.3[3]))
(specificity.4 <- params.4[2] / (params.4[2] + params.4[3]))

(accuracy.2 <- (params.2[1] + params.2[2]) / (sum(params.2)))
(accuracy.3 <- (params.3[1] + params.3[2]) / (sum(params.3)))
(accuracy.4 <- (params.4[1] + params.4[2]) / (sum(params.4)))

(precision.2 <- params.2[1] / (params.2[3] + params.2[1]))
(precision.3 <- params.3[1] / (params.3[3] + params.3[1]))
(precision.4 <- params.4[1] / (params.4[3] + params.4[1]))

(sensitivities = cbind(sensitivity.2, sensitivity.3, sensitivity.4))
(specificities = cbind(specificity.2, specificity.3, specificity.4))
(accuracies = cbind(accuracy.2, accuracy.3, accuracy.4))
(precisions = cbind(precision.2, precision.3, precision.4))

# accuracy decreased for all models when maximising sensitivity

## 5d)
library(ResourceSelection)
length(model.2$coefficients)
  groups = 12
(mh.HL.2 <- hoslem.test(weather.pred_prob$lowrain, weather.pred_prob$fit, g = groups))
(mh.HL.3 <- hoslem.test(weather.pred_prob.3$lowrain, weather.pred_prob.3$fit, g = groups))
(mh.HL.4 <- hoslem.test(weather.pred_prob.4$lowrain, weather.pred_prob.4$fit, g = groups))
(HL.df.2 <- data.frame(group = seq(1, groups),
                       Obs0 = mh.HL.2$observed[, 1],
                       Obs1 = mh.HL.2$observed[, 2],
                       Exp0 = mh.HL.2$expected[, 1],
                       Exp1 = mh.HL.2$expected[, 2]))
(HL.df.3 <- data.frame(group = seq(1, groups),
                       Obs0 = mh.HL.3$observed[, 1],
                       Obs1 = mh.HL.3$observed[, 2],
                       Exp0 = mh.HL.3$expected[, 1],
                       Exp1 = mh.HL.3$expected[, 2]))
(HL.df.4 <- data.frame(group = seq(1, groups),
                       Obs0 = mh.HL.4$observed[, 1],
                       Obs1 = mh.HL.4$observed[, 2],
                       Exp0 = mh.HL.4$expected[, 1],
                       Exp1 = mh.HL.4$expected[, 2]))

(min(HL.df.2$Obs0 + HL.df.2$Obs1))
(min(HL.df.3$Obs0 + HL.df.3$Obs1))
(min(HL.df.4$Obs0 + HL.df.4$Obs1))
ggplot(HL.df.2, aes(group, Obs0)) +
  geom_line(linetype = "dashed", color = "red", size = 1) +
  geom_line(aes(y = Obs1), linetype = "dashed", size = 1) +
  geom_line(aes(y = Exp0), color = "red", size = 1) +
  geom_line(aes(y = Exp1), size = 1) +
  labs(title = "Model temp: Observed and expected in each group",
       caption = "solid = expected, dashed = observed, red = 0, black = 1",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, groups)) +
  theme(text = element_text(size = 14))

(mh.HL.3 <- hoslem.test(weather.pred_prob.3$lowrain, weather.pred_prob.3$fit, g = groups))
(HL.df.3 <- data.frame(group = seq(1, groups),
                       Obs0 = mh.HL.3$observed[, 1],
                       Obs1 = mh.HL.3$observed[, 2],
                       Exp0 = mh.HL.3$expected[, 1],
                       Exp1 = mh.HL.3$expected[, 2]))

ggplot(HL.df.3, aes(group, Obs0)) +
  geom_line(linetype = "dashed", color = "red", size = 1) +
  geom_line(aes(y = Obs1), linetype = "dashed", size = 1) +
  geom_line(aes(y = Exp0), color = "red", size = 1) +
  geom_line(aes(y = Exp1), size = 1) +
  labs(title = "Model pressure: Observed and expected in each group",
       caption = "solid = expected, dashed = observed, red = 0, black = 1",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, groups)) +
  theme(text = element_text(size = 14))

(mh.HL.4 <- hoslem.test(weather.pred_prob.4$lowrain, weather.pred_prob.4$fit, g = groups))
(HL.df.4 <- data.frame(group = seq(1, groups),
                       Obs0 = mh.HL.4$observed[, 1],
                       Obs1 = mh.HL.4$observed[, 2],
                       Exp0 = mh.HL.4$expected[, 1],
                       Exp1 = mh.HL.4$expected[, 2]))

ggplot(HL.df.4, aes(group, Obs0)) +
  geom_line(linetype = "dashed", color = "red", size = 1) +
  geom_line(aes(y = Obs1), linetype = "dashed", size = 1) +
  geom_line(aes(y = Exp0), color = "red", size = 1) +
  geom_line(aes(y = Exp1), size = 1) +
  labs(title = "Model temp*pressure + loc: Observed and expected in each group",
       caption = "solid = expected, dashed = observed, red = 0, black = 1",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, groups)) +
  theme(text = element_text(size = 14))
