# lecture 9, Goodness-of-fit

library(ggplot2)
# This library is for calculating ROC-curves, AUC and
# confidence interval for AUc:
library(pROC)
# Hosmer-Lemeshow goodness of fit test:
library(ResourceSelection)

# The PM10-data again:

load("Data/pm10.rda")

# Refit the models from Lecture 8####
model.0 <- glm(highpm10 ~ 1, family = "binomial", data = pm10)
model.1 <- glm(highpm10 ~ I(cars/1000), family = "binomial", data = pm10)
model.2 <- glm(highpm10 ~ I(cars/1000) + windspeed, family = "binomial", data = pm10)
model.red <- glm(highpm10 ~ tempdiff, family = "binomial", data = pm10)
model.3 <- glm(highpm10 ~ I(cars/1000) + zerodiff, family = "binomial", data = pm10)
model.4 <- glm(highpm10 ~ I(cars/1000)*windspeed, family = "binomial", data = pm10)
model.oslo <- glm(highpm10 ~ I(cars/1000)*windspeed + tempdiff,
                  family = "binomial", data = pm10)
model.5 <- step(model.oslo, k = log(nrow(pm10)))
model.6 <- glm(highpm10 ~ I(cars/100)*windspeed + zerodiff, family = "binomial", data = pm10)

# estimate p_i using all the different models:

pred.phat <- cbind(
  pm10,
  p.0 = predict(model.0, type = "response"),
  p.1 = predict(model.1, type = "response"),
  p.2 = predict(model.2, type = "response"),
  p.3 = predict(model.3, type = "response"),
  p.red = predict(model.red, type = "response"),
  p.4 = predict(model.4, type = "response"),
  p.5 = predict(model.5, type = "response"),
  p.6 = predict(model.6, type = "response"),
  p.oslo = predict(model.oslo, type = "response"))
head(pred.phat)

# Confusion matrix for model 3 and model oslo####
# Calculate Y-hat using model 3 and model oslo.

pred.phat$yhat.3 <- as.numeric(pred.phat$p.3 > 0.5)
pred.phat$yhat.oslo <- as.numeric(pred.phat$p.oslo > 0.5)

(row.01 <- table(pm10$highpm10))

(col.01.oslo <- table(pred.phat$yhat.oslo))
(confusion.oslo <- table(pred.phat$highpm10, pred.phat$yhat.oslo))
(spec.oslo <- confusion.oslo[1, 1] / row.01[1])
(sens.oslo <- confusion.oslo[2, 2] / row.01[2])
(accu.oslo <- sum(diag(confusion.oslo)) / sum(confusion.oslo))
(prec.oslo <- confusion.oslo[2, 2] / col.01.oslo[2])

(col.01.3 <- table(pred.phat$yhat.3))
(confusion.3 <- table(pred.phat$highpm10, pred.phat$yhat.3))
(spec.3 <- confusion.3[1, 1] / row.01[1])
(sens.3 <- confusion.3[2, 2] / row.01[2])
(accu.3 <- sum(diag(confusion.3)) / sum(confusion.3))
(prec.3 <- confusion.3[2, 2] / col.01.3[2])

# ROC-curves####
# Calculate for model 0 and 3.
(roc.0 <- roc(highpm10 ~ p.0, data = pred.phat))
# save the coordinates in a data frame for plotting.
roc.df.0 <- coords(roc.0, transpose = FALSE)
roc.df.0$model <- "0"
roc.df.0

(roc.3 <- roc(highpm10 ~ p.3, data = pred.phat))
# save the coordinates in a data frame for plotting.
roc.df.3 <- coords(roc.3, transpose = FALSE)
roc.df.3$model <- "3"
head(roc.df.3)

# Create the data for the Ideal model by hand:
roc.df.ideal <- data.frame(sensitivity = c(0, 1, 1),
                           specificity = c(1, 1, 0),
                           threshold = c(NA, NA, NA))
roc.df.ideal$model <- "ideal"

# experiment with different values of "limit" to find the
# optimal combination of sens and spec.
limit <- 0.635
roc.df.3[roc.df.3$sensitivity > limit & 
           roc.df.3$specificity > limit, ]
##
I_max.3 <- which(roc.df.3$sensitivity > limit & 
                   roc.df.3$specificity > limit)

# Built-in function for plotting one Roc-curve
# Note that the x-axis is reversed!
# If we want the diagonal with geom_abline, it has to be reversed!
# Since both axes are 0-1, we want a square plot area:
# + coord_fixed()
ggroc(roc.3) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  coord_fixed() +
  labs(title = "ROC-curve for model 3")

# Plot the three ROC-curves:
# Use geom_path() instead of geom_line()
#
# For model 3 the curve is color coded according to
# the threshold. The color scheme is set by
# + scale_color_gradientn(colours = rainbow(5)) +
#
# Note that the x-axis is reversed!
# + scale_x_reverse()
# You could use 1 - spec instead.
# If we want the diagonal with geom_abline, it has to be reversed!
#
# Since both axes are 0-1, we want a square plot area:
# + coord_fixed()
#
ggplot(roc.df.3, aes(specificity, sensitivity)) +
  geom_path(aes(color = threshold), size = 2) +
  geom_path(data = roc.df.ideal, color = "black", size = 1) +
  geom_path(data = roc.df.0, color = "red", size = 1,
            linetype = "dashed") +
  geom_point(data = roc.df.3[I_max.3, ], color = "black", size = 3) +
#  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  scale_color_gradientn(colours = rainbow(5)) +
  coord_fixed() +       # square plotting area
  scale_x_reverse() +   # Reverse scale on the x-axis!
  labs(title = "ROC-curve for model 3",
       caption = "Black dot = optimal threshold") +
  theme(text = element_text(size = 14))

# ROC-curves for all models####
roc.1 <- roc(highpm10 ~ p.1, data = pred.phat)
roc.df.1 <- coords(roc.1, transpose = FALSE)
roc.df.1$model <- "1"
roc.2 <- roc(highpm10 ~ p.2, data = pred.phat)
roc.df.2 <- coords(roc.2, transpose = FALSE)
roc.df.2$model <- "2"
roc.4 <- roc(highpm10 ~ p.4, data = pred.phat)
roc.df.4 <- coords(roc.4, transpose = FALSE)
roc.df.4$model <- "4"
roc.5 <- roc(highpm10 ~ p.5, data = pred.phat)
roc.df.5 <- coords(roc.5, transpose = FALSE)
roc.df.5$model <- "5"
roc.6 <- roc(highpm10 ~ p.6, data = pred.phat)
roc.df.6 <- coords(roc.6, transpose = FALSE)
roc.df.6$model <- "6"
roc.red <- roc(highpm10 ~ p.red, data = pred.phat)
roc.df.red <- coords(roc.red, transpose = FALSE)
roc.df.red$model <- "red"
roc.oslo <- roc(highpm10 ~ p.oslo, data = pred.phat)
roc.df.oslo <- coords(roc.oslo, transpose = FALSE)
roc.df.oslo$model <- "oslo"

roc.df <- rbind(roc.df.0, roc.df.1, roc.df.2, roc.df.3, 
                roc.df.4, roc.df.5, roc.df.6, roc.df.red,
                roc.df.oslo)

# Plot all the curves, in different colors:
ggplot(roc.df, aes(specificity, sensitivity,
                            color = model)) +
  geom_path(size = 1) +
  coord_fixed() +       # square plotting area
  scale_x_reverse() +   # Reverse scale on the x-axis!
  labs(title = "ROC-curves for all the models") +
  theme(text = element_text(size = 14))

# AUC####
roc.3
auc(roc.3)
# Confidence interval for AUC
(ci.3 <- ci(roc.3))
# lower limit:
ci.3[1]
# AUC:
ci.3[2]
# upper limit:
ci.3[3]

#Collect AUC and intervals for all the models:
(aucs <- 
  data.frame(
    model = c("0", "1", "2", "red", "3", "4", "5", "6", "oslo"),
    auc = c(auc(roc.0), auc(roc.1), auc(roc.2), auc(roc.red),
            auc(roc.3), auc(roc.4), auc(roc.5), auc(roc.6),
            auc(roc.oslo)),
    lwr = c(ci(roc.0)[1], ci(roc.1)[1],
            ci(roc.2)[1], ci(roc.red)[1],
            ci(roc.3)[1], ci(roc.4)[1],
            ci(roc.5)[1], ci(roc.6)[1],
            ci(roc.oslo)[1]),
    upr = c(ci(auc(roc.0))[3], ci(auc(roc.1))[3],
            ci(auc(roc.2))[3], ci(auc(roc.red))[3],
            ci(auc(roc.3))[3], ci(auc(roc.4))[3],
            ci(auc(roc.5))[3], ci(auc(roc.6))[3],
            ci(auc(roc.oslo))[3])))

# Compare the AUC for the models:
roc.test(roc.0, roc.oslo)
roc.test(roc.1, roc.oslo)
roc.test(roc.2, roc.oslo)
roc.test(roc.red, roc.oslo)
roc.test(roc.3, roc.oslo)
roc.test(roc.4, roc.oslo)
roc.test(roc.5, roc.oslo)
roc.test(roc.6, roc.oslo)

# Hosmer-Lemeshow-test####
#plot in sorted p-order
# order(variable) gives the ranks for the values in variable.
# It can then be used to sort the data frame:
pred.sort <- pred.phat[order(pred.phat$p.3), ]
pred.sort$rank <- seq(1, nrow(pred.sort))
head(pred.sort)

# Divide the n=500 observations into g=10 groups:
n <- nrow(pred.sort)
g <- 10
# with ng = 50 observations each:
ng <- n/g

# Plot p_i and Y_i
# Add i vertical jitter to Y_i to separate them
ggplot(pred.sort, aes(rank, p.3)) +
  geom_point() +
  geom_jitter(aes(y = highpm10), height = 0.01) +
  geom_vline(xintercept = seq(ng, nrow(pred.sort) - ng, ng)) +
  labs(title = "Model 3: Estimated probabilities by increasing size",
       caption = "g = 10 groups",
       x = "(i) = 1,...,n", y = "p-hat") +
  theme(text = element_text(size = 14))

# HL by hand####
# The following can be done using the output of the
# hoslem.test function, see below.
# I do is here to illustrate the steps.

# A for-loop to set the group numbers:
pred.sort$group <- NA
for (k in seq(1, g)) {
  I <- (k - 1)*ng + seq(1, ng)
  pred.sort$group[I] <- k
}
head(pred.sort)

# Calculate Observed and Expected in each group:
# aggregate(y ~ x, FUN = mean) calculates the mean of y
# separately for each group.
# merge(data1, data2) joins two data frames using any common
# variables as keys, in this case, "group".

# Number of successes:
OE1 <- merge(aggregate(highpm10 ~ group, data = pred.sort, FUN = sum),
             aggregate(p.3 ~ group, data = pred.sort, FUN = sum))
OE1
# Number of failures = n_g - successes:
OE0 <- OE1
OE0$highpm10 <- ng - OE1$highpm10
OE0$p.3 <- ng - OE1$p.3
# A variable to cure for color coding:
OE1$outcome <- "O1k, E1k"
OE0$outcome <- "O0k, E0k"
# Bind the two data sets as rows (r):
(OE <- rbind(OE1, OE0))

# And plot:
# Set the tickmarks on the x-axis to integers 1,...,g
ggplot(OE, aes(group, p.3, color = outcome)) +
  geom_line(size = 1) +
  geom_line(aes(y = highpm10), linetype = "dashed", size = 1) +
  labs(title = "Model 3: Observed and expected in each group",
       caption = "solid = expected, dashed = observed",
       y = "number of observations") +
  theme(text = element_text(size = 14)) +
  scale_x_continuous(breaks = seq(1, g))

# The test:
(chi2HL <- sum((OE$highpm10 - OE$p.3)^2/OE$p.3))
# chi2-quantile to compare with:
qchisq(1 - 0.05, g - 2)
# or P-value:
pchisq(chi2HL, g - 2, lower.tail = FALSE)

# HL using hoslem.test####
# p+1:
length(model.3$coefficients)
# so we need g > 3
# while the smallest expected value is at least approx 5:
# Allowing 4 here and have experimented with g:
(HL.3 <- hoslem.test(pred.sort$highpm10, pred.sort$p.3, g = 8))
HL.3$expected

# Collect the data in a useful form for plotting:
(HL.df.3 <- data.frame(group = seq(1, 8),
                       Obs0 = HL.3$observed[, 1],
                       Obs1 = HL.3$observed[, 2],
                       Exp0 = HL.3$expected[, 1],
                       Exp1 = HL.3$expected[, 2]))

ggplot(HL.df.3, aes(group, Obs0)) +
  geom_line(linetype = "dashed", color = "red", size = 1) +
  geom_line(aes(y = Obs1), linetype = "dashed", size = 1) +
  geom_line(aes(y = Exp0), color = "red", size = 1) +
  geom_line(aes(y = Exp1), size = 1) +
  labs(title = "Model 3: Observed and expected in each group",
       caption = "solid = expected, dashed = observed, red = 0, black = 1",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, 11)) +
  theme(text = element_text(size = 14))

# Do the same for model oslo:
length(model.oslo$coefficients)
(HL.oslo <- hoslem.test(pred.sort$highpm10, pred.sort$p.oslo, 
                        g = 7))
HL.oslo$expected

(HL.df.oslo <- data.frame(group = seq(1, 7),
                     Obs0 = HL.oslo$observed[, 1],
                     Obs1 = HL.oslo$observed[, 2],
                     Exp0 = HL.oslo$expected[, 1],
                     Exp1 = HL.oslo$expected[, 2]))
# How many observations in each group?
HL.df.oslo$Obs0 + HL.df.oslo$Obs1

ggplot(HL.df.oslo, aes(group, Obs0)) +
  geom_line(linetype = "dashed", color = "red", size = 1) +
  geom_line(aes(y = Obs1), linetype = "dashed", size = 1) +
  geom_line(aes(y = Exp0), color = "red", size = 1) +
  geom_line(aes(y = Exp1), size = 1) +
  labs(title = "Model oslo: Observed and expected in each group",
       caption = "solid = expected, dashed = observed, red = 0, black = 1",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, 10)) +
  theme(text = element_text(size = 14))

# And for model 1:
(HL.1 <- hoslem.test(pred.sort$highpm10, pred.sort$p.1, 
                        g = 12))
HL.1$expected
(HL.df.1 <- data.frame(group = seq(1, 12),
                       Obs0 = HL.1$observed[, 1],
                       Obs1 = HL.1$observed[, 2],
                       Exp0 = HL.1$expected[, 1],
                       Exp1 = HL.1$expected[, 2]))

ggplot(HL.df.1, aes(group, Obs0)) +
  geom_line(linetype = "dashed", color = "red", size = 1) +
  geom_line(aes(y = Obs1), linetype = "dashed", size = 1) +
  geom_line(aes(y = Exp0), color = "red", size = 1) +
  geom_line(aes(y = Exp1), size = 1) +
  labs(title = "Model 1: Observed and expected in each group",
       caption = "solid = expected, dashed = observed, red = 0, black = 1",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, 12)) +
  theme(text = element_text(size = 14))
