# Lecture 5: Regression diagnostics
# 6/4-20

library(ggplot2)

load("Data/pike.rda")
head(pike)

# Plot the data####
ggplot(pike, aes(length, weight)) + geom_point()

#Select the strange long fish####
I_strange <- which(pike$length > 200)

# and add it to the plot marked in red####
# pike[I_strange, ] will select the row(s) with the 
# strange fish and all the columns.
ggplot(pike, aes(length, weight)) + 
  geom_point() +
  geom_point(data = pike[I_strange, ], color = "red",
           size = 3, shape = 24) +
  labs(title = "Pike in Halland: weight by length",
       caption = "including one 4.8m(?!) long weighing 0.7kg(!?)") +
  theme(text = element_text(size = 18))

# fit and plot in log-log-scale####
model.all <- lm(log(weight) ~ log(length), pike)
pike.pred <- cbind(pike, 
                   fit = predict(model.all),
                   e = residuals(model.all))

(pike.plot <- ggplot(pike.pred, aes(log(length), log(weight))) +
    geom_point() +
    geom_point(data = pike[I_strange, ], color = "red",
               size = 3, shape = 24) +
    geom_line(aes(y = fit), size = 1, color = "red", linetype = "dashed") +
    labs(title = "Pike: fitted line including strange observation") +
    theme(text = element_text(size = 18))
)

# plot residuals####
elim <- max(abs(pike.pred$e)) * c(-1, 1)
ggplot(pike.pred, aes(x = fit, y = e)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  expand_limits(y = elim) +
  geom_point(data = pike.pred[I_strange, ], color = "red",
             size = 3, shape = 24) +
  labs(title = "Pike: residuals vs fitted values") +
  xlab("fitted values (log weight)") +
  theme(text = element_text(size = 18))

# leverage####
pike.pred$v <- influence(model.all)$hat
head(pike.pred)

# with 1/n and 2(p+1)/n horizontal lines:
ggplot(cbind(pike.pred), aes(x = log(length), y = v)) +
  geom_point() +
  geom_point(data = pike.pred[I_strange, ], color = "red",
             size = 3, shape = 24) +
  geom_hline(yintercept = c(I(1/nrow(pike)))) +
  geom_hline(yintercept = I(2*2/nrow(pike)), color = "red") +
  labs(title = "Pike: leverage vs log length") +
  labs(caption = "y = 1/n (black) and 2(p+1)/n (red)") +
  theme(text = element_text(size = 18))

# studentized residuals####
pike.pred$r <- rstudent(model.all)
head(pike.pred)

# plot against y-hat to detect patterns:
ggplot(pike.pred, aes(x = fit, y = r)) +
  geom_point() +
  geom_point(data = pike.pred[I_strange, ], color = "red",
             size = 3, shape = 24) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red") +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("r*") +
  labs(title = "Pike: studentized residuals vs fitted values") +
  labs(caption = "y = +/- 2 and +/- 4") +
  theme(text = element_text(size = 18))

# plot sqrt(abs(r)) to detect heteroscedastic variance:
ggplot(pike.pred, aes(x = fit, y = sqrt(abs(r)))) +
  geom_point() +
  geom_point(data = pike.pred[I_strange, ], color = "red",
             size = 3, shape = 24) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(2), color = "red") +
  geom_hline(yintercept = 2, color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("sqrt(|r*|)") +
  labs(title = "Pike: sqrt(|r*|) vs fitted values") +
  labs(caption = "y = sqrt(2) and sqrt(4)") +
  theme(text = element_text(size = 18))

# Cook's D####
pike.pred$D <- cooks.distance(model.all)
head(pike.pred)

# Plot against r*
ggplot(pike.pred, aes(r, D)) + 
  geom_point() +
  geom_point(data = pike.pred[I_strange, ], color = "red",
             size = 3, shape = 24) +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  geom_hline(yintercept = I(4/nrow(pike)), color = "red") +
  xlab("r_i*") +
  ylab("D_i") +
  labs(title = "Pike: Cook's D vs studentized residuals") +
  labs(caption = "y = 1 and 4/n") +
  theme(text = element_text(size = 18))

# Exclude the strange fish and re-fit the model####
pike.exc <- pike[-I_strange, ]
model.exc <- lm(log(weight) ~ log(length), data = pike.exc)

#add predictions to whole data set
pike.pred$fit.exc <- predict(model.exc, pike)

# add new line to pike.plot:
pike.plot + geom_line(data = pike.pred, 
                      aes(y = fit.exc), color = "blue", size = 1) +
  labs(title = "Pike: fitted line excluding strange observation") +
  labs(caption = "Fitted line with (red dashed) and without (blue solid) problematic fish")

#residual analysis####
pike.exc.pred <- cbind(pike.exc,
                       fit = predict(model.exc),
                       r = rstudent(model.exc))

ggplot(pike.exc.pred, aes(x = fit, y = r)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red") +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("r*") +
  labs(title = "Pike: studentized residuals vs fitted values",
       subtitle = "without the strange fish") +
  labs(caption = "y = +/- 2 and +/- 4") +
  theme(text = element_text(size = 18))

# DFBETAS####
head(dfbetas(model.all))
pike.pred$df0 <- dfbetas(model.all)[,"(Intercept)"]
pike.pred$df1 <- dfbetas(model.all)[,"log(length)"]

#dfbetas for beta_0:
ggplot(pike.pred, aes(x = fit, y = df0)) +
  geom_point() +
  geom_point(data = pike.pred[I_strange, ], color = "red",
             size = 3, shape = 24) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1), color = "red", linetype = "dashed") +
  geom_hline(yintercept = 2/sqrt(nrow(pike))*c(-1, 1), color = "red") +
  ylab("DFBETAS_0(i)") +
  labs(title = "Pike: DFBETAS_0 vs fitted value") +
  labs(caption = "y = 1 and 2/sqrt(n)") +
  theme(text = element_text(size = 18))

# dfbetas for beta1:
ggplot(pike.pred, aes(x = log(length), y = df1)) +
  geom_point() +
  geom_point(data = pike.pred[I_strange, ], color = "red",
             size = 3, shape = 24) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1), color = "red", linetype = "dashed") +
  geom_hline(yintercept = 2/sqrt(nrow(pike))*c(-1, 1), color = "red") +
  #  xlab("r*") +
  ylab("DFBETAS_1(i)") +
  labs(title = "Pike: DFBETAS_1 vs log length") +
  labs(caption = "y = 1 and 2/sqrt(n)") +
  theme(text = element_text(size = 18))
