# Lecture 6: variable selection with cabbage heads
# 7/4-20.

library(ggplot2)

# Create the data set:
cabbage <- data.frame(
  soil = c(rep(1, 10), rep(2, 10), rep(3, 10)),
  fertilize = c(10, 15, 20, 25, 30, 40, 45, 50, 55, 60,
                10, 15, 20, 25, 30, 40, 45, 50, 55, 60,
                10, 15, 20, 25, 30, 40, 45, 50, 55, 60),
  headwt = c(1.4, 1.9, 3.4, 2.4, 4.2, 1.9, 1.7, 4.7, 1.9,
             2.0, 3.1, 2.9, 3.6, 3.9, 3.6, 4.0, 3.3, 4.3,
             5.3, 4.6, 1.9, 2.0, 3.1, 3.5, 3.3, 2.4, 2.5,
             3.5, 1.9, 3.3))
## tell R that soil is a categorical variable, not numerical:
cabbage$soil <-
  factor(cabbage$soil,
         levels = c(1, 2, 3),
         labels = c("sand", "clay", "loam"))

# fit model 0-4 and test for nested models####
model.0 <- lm(headwt ~ 1, data = cabbage)
(sum.0 <- summary(model.0))

model.1 <- lm(headwt ~ fertilize, data = cabbage)
(sum.1 <- summary(model.1))
anova(model.0, model.1)

model.2 <- lm(headwt ~ soil, data = cabbage)
(sum.2 <- summary(model.2))
anova(model.0, model.2)

model.3 <- lm(headwt ~ soil + fertilize, data = cabbage)
(sum.3 <- summary(model.3))
anova(model.1, model.3)
anova(model.2, model.3)

model.4 <- lm(headwt ~ soil*fertilize, data = cabbage)
(sum.4 <- summary(model.4))
anova(model.3, model.4)

# collect the R2-s for plotting####
(collect.R2s <- data.frame(
  nr = seq(1, 5),
  model = c("0.intercept", "1.fertilize", "2.soil",  
            "3.soil+fert", "4.soil*fert"),
  R2 = c(sum.0$r.squared,
         sum.1$r.squared,
         sum.2$r.squared,
         sum.3$r.squared,
         sum.4$r.squared),
  R2.adj = c(sum.0$adj.r.squared,
             sum.1$adj.r.squared,
             sum.2$adj.r.squared,
             sum.3$adj.r.squared,
             sum.4$adj.r.squared)))

#and plot them:
ggplot(collect.R2s, aes(model, R2)) +
  geom_point(size = 3) + 
  geom_point(aes(y = R2.adj), color = "red", size = 3) + 
  geom_line(aes(x = nr), size = 1) +
  geom_line(aes(x = nr, y = R2.adj), 
            color = "red", size = 1, linetype = "dashed") +
  geom_hline(yintercept = 1) +
  labs(caption = "R2 (black), R2-adj (red dashed)") +
  labs(title = "Cabbage: R2 and R2-adjusted") +
  ylab("R2 and R2-adj") +
  theme(text = element_text(size = 18))

#AIC and BIC####
(collect.AIC <- data.frame(
  nr = seq(1, 5),
  model = c("0.intercept", "1.fertilize", "2.soil",  
            "3.soil+fert", "4.soil*fert"),
  AIC(model.0, model.1, model.2, model.3, model.4),
  BIC(model.0, model.1, model.2, model.3, model.4)))

# and plot:
ggplot(collect.AIC, aes(model, AIC)) +
  geom_point(size = 3) + 
  geom_point(aes(y = BIC), color = "red", size = 3) + 
  geom_line(aes(x = nr), size = 1) +
  geom_line(aes(x = nr, y = BIC), 
            color = "red", size = 1, linetype = "dashed") +
  labs(caption = "AIC (black), BIC (red dashed)") +
  labs(title = "Cabbage: AIC and BIC") +
  ylab("AIC and BIC") +
  theme(text = element_text(size = 18))

# Fine tuning####

# model 5####
# soilloam is very non-significant, see
sum.3
# easiest to create a new dummy-variable for clay:
cabbage$clay <- as.numeric(cabbage$soil == "clay")
head(cabbage)

model.5 <- lm(headwt ~ clay + fertilize, data = cabbage)
(sum.5 <- summary(model.5))
anova(model.5, model.3)
sum.5$adj.r.squared
AIC(model.5)
BIC(model.5)

#model 6####
# fertilize is not really significant:
sum.5
#remove it:
model.6 <- lm(headwt ~ clay, data = cabbage)
(sum.6 <- summary(model.6))
anova(model.6, model.5)
anova(model.6, model.2)
sum.6$adj.r.squared
AIC(model.6)
BIC(model.6)

# model 7####
model.7 <- lm(headwt ~ clay*fertilize, data = cabbage)
(sum.7 <- summary(model.7))
anova(model.6, model.7)
sum.7$adj.r.squared
AIC(model.7)
BIC(model.7)

# model 8####
# but fertilze itself is not significant:
sum.7
# easiest to create a new variable:
cabbage$clay_fertilize <- cabbage$clay*cabbage$fertilize
head(cabbage)
model.8 <- lm(headwt ~ clay + clay_fertilize, data = cabbage)
(sum.8 <- summary(model.8))
anova(model.7, model.8)
sum.8$adj.r.squared
AIC(model.8)
BIC(model.8)

# model 9####
# but clay itself is not significant:
sum.8
model.9 <- lm(headwt ~ clay_fertilize, data = cabbage)
(sum.9 <- summary(model.9))
anova(model.8, model.9)
sum.9$adj.r.squared
AIC(model.9)
BIC(model.9)

sum.8$r.squared

# Backward elimination####
# using the default AIC (k = 2):
step(model.4, direction = "backward")
# using BIC:
step(model.4, k = log(nrow(cabbage)))
# first step:
# best = remove interaction
# second best = do nothing
#
# second step (after removing interaction):
# best = remove fertilize
# second best = do nothing
# third best = remove soil
#
# third step (after removinf fertilize):
# best = do nothing
# second best = remove soil
# STOP

# Forward selection####
# start with a model, e.g. model 0,
# specify the scope,
# upper = largest model allowed
# and a direction:
step(model.0, 
     scope = list(upper = model.4), 
     direction = "forward")

# Stepwise selection####
# start with a model, e.g. model 1,
# specify the scope,
# lower = smallest model allowed
# upper = largest model allowed
step(model.1, 
     scope = list(lower = model.0, upper = model.4),
     direction = "both",
     k = log(nrow(cabbage)))
