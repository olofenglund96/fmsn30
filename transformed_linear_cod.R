### log-log regression: Atlantic cod####
# Run through simple_linear_icecream and
# simple_linear_cod first.

library(ggplot2)

# Read in the data file.
load("Data/cod.RData")
summary(cod.data)
head(cod.data)

### Fit the loglog model####
(cod.logmod <- lm(log(weight) ~ log(length), 
                   data = cod.data))

### Get and save standard errors, etc####
(cod.logsum <- summary(cod.logmod))

# Extract and save the sigma estimate from the summary
# so you can use it later.
(cod.logsigma <- cod.logsum$sigma)

# Extract the beta estimates table from the summary
cod.logsum$coefficients
(a <- exp(cod.logsum$coefficients[1]))

# Calculate the confidence intervals for beta:
confint(cod.logmod)
(Ia <- exp(confint(cod.logmod)[1,]))

# Save all the beta-related things together.
# Since they are matrices we also need to force the result
# to becoma a data frame so we can refer to the columns
# by name.

### Covariance matrix for beta####
# Extract (X'X)^{-1}
(cod.logxtxinv <- cod.logsum$cov.unscaled)

# Calculate the covariance matrix for beta:
(cod.logbetacov <- cod.logsigma^2*cod.logxtxinv)

### Get confidence and prediction interval for x0 = 34####

# New data frame with the new x0-value(s) which must have
# the same name as in the data frame used to fit the model!
(cod.x0 <- data.frame(length = c(34)))

# Make a new data frame for the predictions:
# Add the fitted line, "fit", 
# and its standard error, "se.fit",
# degrees of freedom, "df" = n-2, 
# and, yet another, sigma-estimate = "residual.scale";
# Add confidence interval, as variables starting with "conf.";
# Add prediction interval, as variables starting with "pred.".
(
  cod.logy0pred <- 
    cbind(cod.x0, 
          predict(cod.logmod, cod.x0, se.fit = TRUE),
          conf = predict(cod.logmod, cod.x0,
                         interval = "confidence"),
          pred = predict(cod.logmod, cod.x0,
                         interval = "prediction"))
)

# We now have three versions of the fitted line! 
# And an extra copy of sigma. And df.
# Get rid of the extra ones by setting them to NULL:
cod.logy0pred$df <- cod.logy0pred$residual.scale <- 
  cod.logy0pred$conf.fit <- cod.logy0pred$pred.fit <- NULL
cod.logy0pred

# Calculate the standard error of the prediction 
# using the saved sigma-estimate, and add it;
cod.logy0pred$se.pred <- 
  sqrt(cod.logsigma^2 + cod.logy0pred$se.fit^2)
cod.logy0pred

# This is the predictions of ln(weight)!
# Transform them to the original scale
cod.logy0pred$exp.fit <- exp(cod.logy0pred$fit)
cod.logy0pred$expconf.lwr <- exp(cod.logy0pred$conf.lwr)
cod.logy0pred$expconf.upr <- exp(cod.logy0pred$conf.upr)
cod.logy0pred$exppred.lwr <- exp(cod.logy0pred$pred.lwr)
cod.logy0pred$exppred.upr <- exp(cod.logy0pred$pred.upr)
cod.logy0pred

### Plot the data####

# Original scale:
ggplot(data = cod.data, aes(x = length, y = weight)) +
  geom_point() +
  expand_limits(x = c(20, 50))

# Plot the logarithms instead:
ggplot(data = cod.data, 
       aes(x = log(length), y = log(weight))) +
  geom_point() +
  expand_limits(x = c(log(20), log(50)))

# Alternative way that changes the scale instead of
# the values:
ggplot(data = cod.data, aes(x = length, y = weight)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  expand_limits(x = c(20, 50))


# Fine tune the original plot (see simple_linear_cod):

(plot.data <- plot.data + 
    labs(caption = "original scale") +
    expand_limits(x = c(20, 50)))

# Save the log-plot as well:
(
  plot.logdata <- 
    ggplot(data = cod.data, 
           aes(x = log(length), y = log(weight))) + 
    geom_point() +
    xlab("ln(Length(cm))") +
    ylab("ln(Weight(g))") +
    labs(title = "Altantic cod: ln weight by ln length") +
    theme(text = element_text(size = 18)) +
    labs(caption = "logarithms before plotting") +
    expand_limits(x = c(log(20), log(50)))
)

### Add the fitted line####
# without confidence interval:
(
  plot.logline <- plot.logdata + 
    geom_smooth(method = lm, se = FALSE) +
    labs(caption = "data and fitted line")
  )

### Add the confidence interval####
(
  plot.logconf <- plot.logline + 
    geom_smooth(method = lm) +
    labs(caption = "data, fitted line and conf.int")
)

# Calculate and save fitted line, confidence, and 
# prediction intervals. This is required in order
# to be able to plot them in the original scale.

cod.logpred <- 
  cbind(cod.data,
        logfit = predict(cod.logmod),
        logconf = predict(cod.logmod,
                          interval = "confidence"),
        logpred = predict(cod.logmod,
                          interval = "prediction"))
head(cod.logpred)

# remove the extra fitted lines:
cod.logpred$logconf.fit <- cod.logpred$logpred.fit <- NULL

# Add the prediction interval to the plot of the 
# log-values:

(
  plot.logpred <- plot.logconf +
    geom_smooth(method = lm) +
    geom_line(data = cod.logpred, 
              aes(y = logpred.lwr),
              color = "red", linetype = "dashed",
              size = 1) +
    geom_line(data = cod.logpred, 
              aes(y = logpred.upr),
              color = "red", linetype = "dashed",
              size = 1) +
    labs(caption = "fitted line and 95% conf. and pred. intervals")
)  

# Add the fitted line, confidence interval and prediction
# interval to the original data. REQUIRES "anti-log"
# of all predictions and we cannot use geom_smooth,
# we have to use geom_ribbon instead (with is what smooth does).

(
  plot.pred <- plot.data +
    geom_line(data = cod.logpred, 
              aes(y = exp(logfit)),
              color = "blue", size = 1) +
    geom_ribbon(data = cod.logpred,
                aes(ymin = exp(logconf.lwr), 
                ymax = exp(logconf.upr)),
                alpha = 0.2) +
    geom_line(data = cod.logpred, 
              aes(y = exp(logpred.lwr)),
              color = "red", linetype = "dashed",
              size = 1) +
    geom_line(data = cod.logpred, 
              aes(y = exp(logpred.upr)),
              color = "red", linetype = "dashed",
              size = 1) +
    labs(caption = "fitted line and 95% conf. and pred. intervals")
)

### Basic residual analysis####

### Add the residuals to the predicted data
cod.logpred$e.log <- cod.logmod$residuals
head(cod.logpred)

# Save the max-value in order to make the y-axins symmetrical 
# in the plots.
(max.elog <- max(abs(cod.logpred$e.log)))
(cod.lim.elog <- c(-max.elog, max.elog))

# Plot residuals against x, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot(data = cod.logpred, 
       aes(x = log(length), y = e.log)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  expand_limits(y = cod.lim.elog) +
  xlab("ln(Length (cm))") +
  ylab("Residual") +
  labs(title = "Residuals vs x-values") +
  theme(text = element_text(size = 18))

ggplot(data = cod.logpred, aes(x = logfit, y = e.log)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  expand_limits(y = cod.lim.elog) +
  xlab("Predicted ln weight") +
  ylab("Residual") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

ggplot(data = cod.logpred,
       aes(sample = e.log)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

ggplot(data = cod.logpred,
       aes(x = e.log)) +
  geom_histogram(bins = 20) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 18))
