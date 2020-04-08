library(ggplot2)

load("Data/Pb_mossa.rda")
head(Pb_mossa)
Pb_mossa$nyear = I(Pb_mossa$year-1975)
summary(Pb_mossa)
Pb_log = Pb_mossa
Pb_log$Pb <- log(Pb_mossa$Pb)
Pb_log$nyear <- Pb_mossa$nyear

Pb_log$loc <- relevel(Pb_log$loc, "Norrbotten")
Pb_log.locmodel <- lm(Pb ~ nyear + loc, data = Pb_log)

ggplot(data =Pb_log, 
       aes(x = nyear, y = Pb, color = loc)) + 
  geom_point() +
  facet_wrap(~ loc)


Pb_log.pred <- cbind(
  Pb_log, 
  fit = predict(Pb_log.locmodel),
  e = residuals(Pb_log.locmodel)
)

Pb_log.pred$v <- influence(Pb_log.locmodel)$hat
head(Pb_log.pred)

for (location in unique(Pb_log.pred$loc)) {
  Pb_log.pred[(Pb_log.pred$loc == location), c("ubound")] <- with(Pb_log.pred, 1/sum(Pb_log.pred$loc == location))
  Pb_log.pred[(Pb_log.pred$loc == location), c("lbound")] <- with(Pb_log.pred, 2*3/sum(Pb_log.pred$loc == location))
}

ggplot(data = Pb_log.pred, aes(x = nyear, y = v, color = loc)) +
  geom_jitter(width = 1) +
  geom_line(data = Pb_log.pred, aes(y = lbound), color = "black") +
  geom_line(data = Pb_log.pred, aes(y = ubound), color = "red") +
  facet_wrap(~ loc) +
  labs(caption = "y = 1/n (black) and 2(p+1)/n (red)", title = "Leverages") +
  theme(text = element_text(size = 18)) +
  geom_hline(yintercept = 0, color = NA)

Pb_log.pred$r <- rstudent(Pb_log.locmodel)
head(Pb_log.pred)

ggplot(data = Pb_log.pred, aes(x = nyear, y = r, color = loc)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red") +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dashed") +
  facet_wrap(~ loc) +
  labs(caption = "y = +/- 2 and +/- 4") +
  labs(title = "Studentized residuals") +
  theme(text = element_text(size = 18))

# some of the points lie outside the +-4 range, especially one for VastraGotaland which is "far" above the line.

ggplot(data = Pb_log.pred, aes(x = nyear, y = sqrt(abs(r)), color = loc)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(2), color = "red") +
  geom_hline(yintercept = 2, color = "red", linetype = "dashed") +
  facet_wrap(~ loc) +
  xlab("Years after 1975") +
  ylab("sqrt(|r*|)") +
  labs(title = "sqrt(|r*|) vs fitted values") +
  labs(caption = "y = sqrt(2) and sqrt(4)") +
  theme(text = element_text(size = 18))

# for some of the locations, the variability seem to increase with time. There also seems to be a period around 10 years
# after 1975 where the variability decreases. But overall, the variablility look quite homogenous.

Pb_log.pred$D <- cooks.distance(Pb_log.locmodel)
head(Pb_log.pred)

ggplot(Pb_log.pred, aes(x = nyear, y = D, color = loc)) + 
  geom_point() +
  geom_hline(yintercept = 4/nrow(Pb_log), color = "red") +
  facet_wrap(~ loc) +
  xlab("time") +
  ylab("D_i") +
  labs(title = "Cook's D vs time") +
  labs(caption = "y = 4/n") +
  theme(text = element_text(size = 18))

# the largest influential point can be found in VastraGotaland (top right corner) and Orebro also has some large
# ones.

head(dfbetas(Pb_log.locmodel))
Pb_log.pred$df0 <- dfbetas(Pb_log.locmodel)[,"(Intercept)"]
Pb_log.pred$df1 <- dfbetas(Pb_log.locmodel)[,"nyear"]

for (location in unique(Pb_log.pred$loc)) {
  Pb_log.pred[(Pb_log.pred$loc == location), c("Dfbeta_bound")] <- with(Pb_log.pred, 2/sqrt(sum(Pb_log.pred$loc == location)))
}

ggplot(Pb_log.pred, aes(x = nyear, y = df1, color = loc)) +
  geom_point() +
  geom_hline(yintercept = 2/sqrt(nrow(Pb_log))*c(1, -1)) +
  #geom_line(aes(y = Dfbeta_bound), color = "red") +
  #geom_line(aes(y = -1*Dfbeta_bound), color = "red") +
  facet_wrap(~ loc) +
  ylab("DFBETAS_0(i)") +
  labs(title = "DFBETAS_1 vs time") +
  labs(caption = "y = +-2/sqrt(n)") +
  theme(text = element_text(size = 18))

# The points from Orebro seem to have little impact on the decline
# the point in VastraGotaland does seem to contribute a bit more

I_strange <- which(Pb_log$loc == "VastraGotaland" & Pb_log$nyear == 35 & Pb_log$Pb > 2)

ggplot(data =Pb_log, 
       aes(x = nyear, y = Pb, color = loc)) + 
  geom_point() +
  geom_point(data = Pb_log[I_strange, ], color = "red",
             size = 3, shape = 24) +
  facet_wrap(~ loc)

# the point is clearly separated from the rest of the points and will affect the decrease

Pb_log.locmodel2 <- lm(Pb ~ nyear * loc, data = Pb_log)

Pb_log.pred2 <- 
  cbind(Pb_log,
        fit = predict(Pb_log.locmodel2),
        conf = predict(Pb_log.locmodel2,
                       interval = "confidence"),
        pred = predict(Pb_log.locmodel2,
                       interval = "prediction"))

Pb_log.pred <- 
  cbind(Pb_log.pred,
        conf = predict(Pb_log.locmodel,
                       interval = "confidence"),
        pred = predict(Pb_log.locmodel,
                       interval = "prediction"))

(
  plot.data <- 
    ggplot(data =Pb_log, 
           aes(x = nyear, y = Pb, color = loc)) + 
    geom_point() +
    facet_wrap(~ loc) + 
    geom_smooth(method = lm) +
    geom_line(data = Pb_log.pred2, aes(y = pred.lwr),
              color = "red", linetype = "dashed", size = 1) +
    geom_line(data = Pb_log.pred2, aes(y = pred.upr),
              color = "red", linetype = "dashed", size = 1) +
    labs(caption = "data, fitted line, 95% confidence and prediction intervals")
)

(
  plot.data <- 
    ggplot(data =Pb_log, 
           aes(x = nyear, y = Pb, color = loc)) + 
    geom_point() +
    facet_wrap(~ loc) + 
    geom_smooth(method = lm) +
    geom_line(data = Pb_log.pred, aes(y = pred.lwr),
              color = "red", linetype = "dashed", size = 1) +
    geom_line(data = Pb_log.pred, aes(y = pred.upr),
              color = "red", linetype = "dashed", size = 1) +
    labs(caption = "data, fitted line, 95% confidence and prediction intervals")
)

model.0 <- lm(Pb ~ I(year-1975), data = Pb_mossa)
model.1 <- Pb_log.locmodel
model.2 <- Pb_log.locmodel2

sum.0 <- summary(model.0)
sum.1 <- summary(model.1)
sum.2 <- summary(model.2)

sum.0$r.squared # 0.4470135
sum.1$r.squared # 0.8100139
sum.2$r.squared # 0.8128897

sum.0$adj.r.squared # 0.4465636
sum.1$adj.r.squared # 0.8092385
sum.2$adj.r.squared # 0.8115105

AIC(model.0, model.1, model.2)
#         df      AIC
# model.0  3 7645.182
# model.1  7 1370.274
# model.2 11 1359.497

BIC(model.0, model.1, model.2)
#         df      BIC
# model.0  3 7660.529
# model.1  7 1406.083
# model.2 11 1415.769


# the measurements seem to agree on which two models are the best (model.1 and model.2).
# but BIC seems to dislike the amount of variables in model.2

I_VG <- which(Pb_mossa$loc == "VastraGotaland")
Pb_mossa$year_VG <- 0
Pb_mossa$year_VG[I_VG] <- Pb_mossa$year[I_VG] - 1975

model.3 <- lm(log(Pb) ~ nyear + loc + year_VG, data = Pb_mossa)

sum.3 <- summary(model.3)
sum.3$r.squared # 0.8122497
sum.3$adj.r.squared # 0.8113294
AIC(model.3) # 1357.701
BIC(model.3) # 1398.626

# adding the interaction term year_VG results in the best model. 

Pb_log.pred3 <- 
  cbind(Pb_log,
        fit = predict(model.3),
        conf = predict(model.3,
                       interval = "confidence"),
        pred = predict(model.3,
                       interval = "prediction"))

(
  plot.data <- 
    ggplot(data =Pb_log, 
           aes(x = nyear, y = exp(Pb), color = loc)) + 
    geom_point() +
    facet_wrap(~ loc) + 
    geom_line(data = Pb_log.pred3, aes(y = exp(pred.fit))) +
    geom_ribbon(data = Pb_log.pred3, aes(ymin = exp(conf.lwr), ymax = exp(conf.upr), fill = "band"), alpha = 0.3) +
    geom_line(data = Pb_log.pred3, aes(y = exp(pred.lwr)),
              color = "red", linetype = "dashed", size = 1) +
    geom_line(data = Pb_log.pred3, aes(y = exp(pred.upr)),
              color = "red", linetype = "dashed", size = 1) +
    labs(caption = "data, fitted line, 95% confidence and prediction intervals")
)

# the plots look very alike