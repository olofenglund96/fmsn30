load('Data/weather.rda')
library(ggplot2)

(
  plot.data <- 
    ggplot(data = weather, 
         aes(x = temp, y = rain)) + 
    geom_point() +
    labs(title = "Rain vs temp")
)

(weather.linear_model <- lm(rain ~ temp, data = weather))
summary(weather.linear_model)

(
  plot.data <- plot.data +
    geom_smooth(method = lm)
)

(
  plot.data <- plot.data +
  geom_line(data = weather.pred, 
            aes(y = pred.lwr),
            color = "red", linetype = "dashed",
            size = 1) +
  geom_line(data = weather.pred, 
            aes(y = pred.upr),
            color = "red", linetype = "dashed",
            size = 1)
)

# predict
weather.pred <- 
  cbind(weather, 
        pred = predict(weather.linear_model, interval = "prediction"))

weather.pred$e <- weather.linear_model$residuals
head(weather.pred)

(max.e <- max(abs(weather.pred$e)))
(weather.elims <- c(-max.e, max.e))

# residuals vs x
ggplot(data = weather.pred, 
       aes(x = temp, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = weather.elims) +
  xlab("Temp") +
  ylab("Residual") +
  labs(tag = "A") +
  labs(title = "Residuals vs x") +
  theme(text = element_text(size = 18))

# residuals vs yhat
ggplot(data = weather.pred, 
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
ggplot(data = weather.pred, 
       aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

# it is not a very good model since the qq-plot does not follow the line

(
  plot.data <- 
    ggplot(data = weather, 
           aes(x = temp, y = log(rain)))+ 
    geom_point() +
    labs(title = "Rain vs temp")
)

weather.log_model <- lm(log(rain) ~ temp, data = weather)

(
  plot.data <- plot.data +
    geom_smooth(method = lm)
)
summary(weather.log_model)

weather.logpred <-
  cbind(weather, 
        pred = predict(weather.log_model, interval = "prediction"))

weather.logpred$e <- weather.log_model$residuals

# residuals vs x
ggplot(data = weather.logpred, 
       aes(x = temp, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = weather.elims) +
  xlab("Temp") +
  ylab("Residual") +
  labs(tag = "A") +
  labs(title = "Residuals vs x") +
  theme(text = element_text(size = 18))

# residuals vs yhat
ggplot(data = weather.logpred, 
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
ggplot(data = weather.logpred, 
       aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

summary(weather.log_model)
# confidence interval
confint(weather.log_model)
exps <- exp(weather.log_model$coefficients)
confint(exp(exps))

# increases on average by around 3% / degree
weather.log_summary <- summary(weather.log_model)
weather.b0 = weather.log_summary$coefficients[1]
weather.b1 = weather.log_summary$coefficients[2]
weather.a = exp(weather.b0)
weather.b = exp(weather.b1)
weather.Ia = exp(confint(weather.model)[1,])
weather.Ib = exp(confint(weather.model)[2,])

(
  plot.data <- 
    ggplot(data = weather, 
           aes(x=temp, y=rain)) + 
    geom_point(size = 3) +
    xlab("temp") +
    ylab("rain") +
    labs(title = "rain vs temp") +
    theme(text = element_text(size = 18))
)

weather.logpred <- 
  cbind(weather,
        fit = predict(weather.log_model),
        conf = predict(weather.log_model,
                       interval = "confidence"),
        pred = predict(weather.log_model,
                       interval = "prediction"))
head(weather.logpred)

(
  plot.pred <- plot.data +
    geom_line(data = weather.logpred,
              aes(y = exp(fit)),
              color = "blue", size = 1) +
    geom_ribbon(data = weather.logpred,
                aes(ymin = exp(conf.lwr), 
                    ymax = exp(conf.upr)),
                alpha = 0.2) +
    geom_line(data = weather.logpred, 
              aes(y = exp(pred.lwr)),
              color = "red", linetype = "dashed",
              size = 1) +
    geom_line(data = weather.logpred, 
              aes(y = exp(pred.upr)),
              color = "red", linetype = "dashed",
              size = 1) +
    labs(caption = "fitted line and 95% conf. and pred. intervals")
)


(weather.x0 <- data.frame(temp = c(5)))

(
  weather.y0.pred <- 
    cbind(weather.x0, 
          predict(weather.log_model, weather.x0, se.fit = TRUE),
          pred = predict(weather.log_model, weather.x0, 
                         interval = "prediction"))
)

exp(weather.y0.pred)
