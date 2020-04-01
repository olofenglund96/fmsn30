### Simple linear regression: Atlantic cod####
#
# Run through simple_linear_icecream.R first.
#
# This cod-code does the same thing but in a situation
# where the relationship is not linear.
#
# Afterwords, continue with transformed_linear_cod.R

# Read in the data file.
load("Data/cod.RData")
summary(cod.data)
head(cod.data)

### Fit the linear model####
(cod.linmod <- lm(weight ~ length, data = cod.data))

### Get and save standard errors, etc####
(cod.linsum <- summary(cod.linmod))

# Extract and save the sigma estimate from the summary
# so you can use it later.
(cod.linsigma <- cod.linsum$sigma)

# Extract the beta estimates table from the summary
cod.linsum$coefficients

# Calculate the confidence intervals for beta:
confint(cod.linmod)

# Save all the beta-related things together.
# Since they are matrices we also need to force the result
# to becoma a data frame so we can refer to the columns
# by name.
(
  cod.linbeta <- 
    cbind.data.frame(cod.linsum$coefficients, 
                     confint(cod.linmod))
)

### Covariance matrix for beta####
# Extract (X'X)^{-1}
(cod.linxtxinv <- cod.linsum$cov.unscaled)

# Calculate the covariance matrix for beta:
(cod.linbetacov <- cod.linsigma^2*cod.linxtxinv)

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
  cod.liny0pred <- 
    cbind(cod.x0, 
          predict(cod.linmod, cod.x0, se.fit = TRUE),
          conf = predict(cod.linmod, cod.x0, 
                         interval = "confidence"),
          pred = predict(cod.linmod, cod.x0,
                         interval = "prediction"))
)

# We now have three versions of the fitted line! 
# And an extra copy of sigma. And df.
# Get rid of the extra ones by setting them to NULL:
cod.liny0pred$df <- cod.liny0pred$residual.scale <- 
  cod.liny0pred$conf.fit <- cod.liny0pred$pred.fit <- NULL
cod.liny0pred

# Calculate the standard error of the prediction 
# using the saved sigma-estimate, and add it;
cod.liny0pred$se.pred <- 
  sqrt(cod.linsigma^2 + cod.liny0pred$se.fit^2)
cod.liny0pred

### Plot the data####
# Activate the ggplot-commands:
library(ggplot2)

# Plot the data as points:

ggplot(data = cod.data, aes(x = length, y = weight)) +
  geom_point()

# That was a very basic plot. Add some modifications and
# save it so that we can add other things to it later:
(
  plot.data <- 
    ggplot(data = cod.data, 
           aes(x = length, y = weight)) + 
    geom_point() +
    xlab("Length (cm)") +
    ylab("Weight (g)") +
    labs(title = "Altantic cod: weight by length") +
    theme(text = element_text(size = 18))
)

### Add the fitted line####
# without confidence interval: se = FALSE:
(
  plot.linline <- plot.data + 
    geom_smooth(method = lm, se = FALSE) +
    labs(caption = "data and fitted line")
  )

### Histograms of marginal distributions####
ggplot(cod.data, aes(x = weight)) + 
  geom_histogram(bins = 20) +
  xlab("Weight (g)") +
  labs(title = "Marginal distribution of weight") +
  labs(caption = "non-normal distribution") +
  theme(text = element_text(size = 18))

ggplot(cod.data, aes(x = length)) + 
  geom_histogram(bins = 20) +
  xlab("Length (cm)") +
  labs(title = "Distribution of length") +
#  labs(caption = "reason for the non-normal weight loss") +
  theme(text = element_text(size = 18))

### Fitted line with confidence interval####
(
  plot.linconf <- plot.linline + 
    geom_smooth(method = lm) +
    labs(caption = "data, fitted line and 95% confidence interval")
)

### Calculate and add the prediction interval####
# ggplot cannot add the prediction intervals automatically.
# Make a new data frame by adding the prediction intervals
# for each of the observed x_i. It is a good idea not to mess
# up your original data! You will get a warning.
(
  cod.linpred <- 
    cbind(cod.data, 
          linpred = predict(cod.linmod, 
                            interval = "prediction")
          )
)

# Add the prediction interval to the confidence interval plot,
# We need to specify the new data frame since pi.lwr
# and pi.upr were not present when we made the original plot,
# but ggplot remembers that our x-variable is still named weeks.

(
  plot.linpred <- plot.linconf +
    geom_line(data = cod.linpred, aes(y = linpred.lwr),
              color = "red", linetype = "dashed", size = 1) +
    geom_line(data = cod.linpred, aes(y = linpred.upr),
              color = "red", linetype = "dashed", size = 1) +
    labs(caption = 
           "fitted linear model, 95% conf. and pred. intervals")
  )

#Highlight observations outside the prediction intervals:
(
  cod.linoutside <- which(
    cod.linpred$weight < cod.linpred$linpred.lwr | 
      cod.linpred$weight > cod.linpred$linpred.upr)
  )

# Replot with only the data outside the prediction interval.
ggplot(data = cod.linpred, aes(x = length, y = weight)) +
  geom_line(aes(y = linpred.lwr), color = "red", 
            linetype = "dashed", size = 1) +
  geom_line(aes(y = linpred.upr), color = "red", 
            linetype = "dashed", size = 1) +
  geom_point(data = cod.linpred[cod.linoutside,], size = 3) +
  xlab("Length (cm)") +
  ylab("Weight (g)") +
  labs(title = "Atlantic cod: data outside the prediction interval") +
  labs(caption = "95% prediction interval") +
  theme(text = element_text(size = 18))

### Basic residual analysis####

### Add the residuals to the predicted data
cod.linpred$e.lin <- cod.linmod$residuals
head(cod.linpred)

# Save the max-value in order to make the y-axins symmetrical 
# in the plots.
(max.elin <- max(abs(cod.linpred$e.lin)))
(cod.lim.elin <- c(-max.elin, max.elin))

# Plot residuals against x, add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot(data = cod.linpred, aes(x = length, y = e.lin)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  expand_limits(y = cod.lim.elin) +
  xlab("Length (cm)") +
  ylab("Residual") +
  labs(tag = "A") +
  labs(title = "Residuals vs x-values") +
  theme(text = element_text(size = 18))

ggplot(data = cod.linpred, aes(x = linpred.fit, y = e.lin)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    expand_limits(y = cod.lim.elin) +
    xlab("Predicted weight (g)") +
    ylab("Residual") +
#    labs(tag = "A") +
    labs(title = "Residuals vs predicted values Y-hat") +
    theme(text = element_text(size = 18))

ggplot(data = cod.linpred, aes(sample = e.lin)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

ggplot(data = cod.linpred, aes(x = e.lin)) +
  geom_histogram(bins = 20) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 18))

