### Simple linear regression: Ice cream####

# Read in the data file.
# Assumes that the data is located in a subfolder called "Data".
# This is generally good practice!
# Keep your data in one subfolder,  
# your R-code in another, 
# your plots in a third folder,
# and your reports in a fourth.

# Load and look at the data####
load("Data/icecream.RData")

# Check what's in your environment:
ls()

# look at the first few lines:
head(ice.data)

# make a basic summary of the data
summary(ice.data)

### Fit the linear model####
# When a command saves its result in a variable, the
# default is to NOT print the result as well.
# The () around the command tells R to print it.

(ice.model <- lm(loss ~ weeks, data = ice.data))

### Get and save standard errors, etc####
(ice.summary <- summary(ice.model))

# Extract and save the sigma estimate from the summary
# so you can use it later.
(ice.sigma <- ice.summary$sigma)

# Extract the beta estimates table from the summary
ice.summary$coefficients

# Calculate the confidence intervals for beta:
confint(ice.model)

### Covariance matrix for beta####
# Extract (X'X)^{-1}
(ice.xtxinv <- ice.summary$cov.unscaled)

# Calculate the covariance matrix for beta:
(ice.beta.cov <- ice.sigma^2*ice.xtxinv)

# Just double checking that, yes, the standard 
# errors of the beta-estimates are the square roots 
# of the diagonal elements of the covariance matrix.
# Note the backwards slanting apostrophies around the 
# variable name. This is necessary since there are
# spaces in the name.
sqrt(diag(ice.beta.cov))
ice.summary$coefficients

### Get confidence and prediction interval for x0 = 34####

# New data frame with the new x0-value(s) which must
# have the same name as in the data frame used to fit
# the model!
# It is possible to do this for several x0-values at once.
# Use, e.g., weeks = c(34, 54, 23).
(ice.x0 <- data.frame(weeks = c(34)))

# Make a new data frame for the predictions:
# Add the fitted line, "fit", 
# and its standard error, "se.fit",
# degrees of freedom, "df" = n-(p+1), 
# and, yet another, sigma-estimate = "residual.scale";
# Add confidence interval, as variables starting with "conf.";
# Add prediction interval, as variables starting with "pred.".
(
  ice.y0.pred <- 
    cbind(ice.x0, 
          predict(ice.model, ice.x0, se.fit = TRUE),
          conf = predict(ice.model, ice.x0, 
                         interval = "confidence"),
          pred = predict(ice.model, ice.x0,
                         interval = "prediction"))
)

# We now have three versions of the fitted line! 
# And an extra copy of sigma.
# Get rid of the extra ones by setting them to NULL:
ice.y0.pred$residual.scale <-  ice.y0.pred$conf.fit <-
  ice.y0.pred$pred.fit <- NULL
ice.y0.pred

# Calculate the standard error of the prediction 
# using the previously saved sigma-estimate, 
# and add it;
ice.y0.pred$se.pred <- 
  sqrt(ice.sigma^2 + ice.y0.pred$se.fit^2)
ice.y0.pred

### Plot the data####
# Activate the ggplot-commands:
library(ggplot2)


# Plot the data as points:
# Tell ggplot in which data frame all necessary 
# variables for the plot are located, also specifying 
# an aestetic stating the default x- and y-variables.
# Then add a geometry specifying that we want points
# with the default x- and y-variables on the axes.

ggplot(data = ice.data, 
       aes(x = weeks, y = loss)) + 
  geom_point()

# That was a very basic plot. Add some modifications and
# save it so that we can add other things to it later:
(
  plot.data <- 
    ggplot(data = ice.data, 
           aes(x = weeks, y = loss)) + 
    geom_point(size = 3) +
    xlab("Time in storage (weeks)") +
    ylab("Weight loss (g)") +
    labs(title = "Ice cream: weight loss by time in storage") +
    theme(text = element_text(size = 18))
  )


### Add the fitted line do the data plot####
# without confidence interval: se = FALSE:
# A warnng! This only works for simple linear regression.
# When we have transformed variables or more covariates,
# e.g. different categories, we will have to calculate
# the fitted line similarly to the prediction intervals.
(
  plot.data + 
    geom_smooth(method = lm, se = FALSE) +
    labs(caption = "data and fitted line")
)

### Histograms of marginal distributions####
ggplot(ice.data, aes(x = loss)) + 
  geom_histogram(bins = 20) +
  xlab("Weight loss (g)") +
  labs(title = "Marginal distribution of weight loss") +
  labs(caption = "non-normal distribution") +
  theme(text = element_text(size = 18))

ggplot(ice.data, aes(x = weeks)) + 
  geom_histogram(bins = 20) +
  xlab("Storage time (weeks)") +
  labs(title = "Distribution of storage time") +
  labs(caption = "reason for the non-normal weight loss") +
  theme(text = element_text(size = 18))

### Add fitted line WITH confidence interval####
# An even bigger warning warnng!
# This only works for simple linear regression.
# When we have transformed variables or more covariates,
# e.g. different categories, we will have to calculate
# the confidence intervals first, similarly to the
# prediction intervals.
(
  plot.conf <- plot.data + 
    geom_smooth(method = lm) +
    labs(caption = "data, fitted line and 95% confidence interval")
)

### Calculate and add the prediction interval####
# ggplot cannot add the prediction intervals
# automatically.
#
# Make a new data frame by adding the prediction 
# intervals for each of the observations. It is
# a good idea not to mess up your original data! 
#
# You will get a warning that you should contemplate.

ice.pred <- 
  cbind(ice.data, 
        pred = predict(ice.model, interval = "prediction"))
head(ice.pred)

# Add the prediction interval to the confidence 
# interval plot,
# We need to specify the new data frame since pred.lwr
# and pred.upr were not present in ice.data when we 
# made the original plot.
# But ggplot remembers that our x-variable is still 
# named weeks so we don't have to specify that again.

(
  plot.conf +
    geom_line(data = ice.pred, aes(y = pred.lwr),
              color = "red", linetype = "dashed", size = 1) +
    geom_line(data = ice.pred, aes(y = pred.upr),
              color = "red", linetype = "dashed", size = 1) +
    labs(caption = "data, fitted line, 95% confidence and prediction intervals")
)

### Basic residual analysis####

### Add the residuals to the predicted data
ice.pred$e <- ice.model$residuals
head(ice.pred)

# Save the max-value in order to make the y-axis symmetrical 
# in the plots.
(max.e <- max(abs(ice.pred$e)))
(ice.elims <- c(-max.e, max.e))

# Plot residuals against x, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot(data = ice.pred, 
       aes(x = weeks, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = ice.elims) +
  xlab("Storage time (weeks)") +
  ylab("Residual") +
  labs(tag = "A") +
  labs(title = "Residuals vs x-values") +
  theme(text = element_text(size = 18))


# Plot residuals against yhat, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot(data = ice.pred, 
       aes(x = pred.fit, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = ice.elims) +
  xlab("Predicted weight loss (g)") +
  ylab("Residual") +
  labs(tag = "B") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

# Make a normal qq-plot of the residuals.
ggplot(data = ice.pred, 
       aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

# Hisogram of the residuals:
ggplot(data = ice.pred,
       aes(x = e)) +
  geom_histogram(bins = 10) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 18))
