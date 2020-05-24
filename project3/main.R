library(ggplot2)
library(dplyr)
library(MASS)

load("fhm_data.RData")

region_group <- fhm.data %>% group_by(obs_date) %>% summarise(tot = sum(new_cases))

ggplot(region_group, aes(x = obs_date, y = tot)) +
  geom_point() +
  geom_smooth()

ggplot(fhm.data[fhm.data$region %in% c("Stockholm", "Västra Götaland", "Uppsala", "Skåne"),], aes(x = day_nbr_region, y = new_cases)) +
  geom_point() +
  geom_smooth() +
  facet_wrap( ~ region) +
  labs(title = "Daily new cases for various regions")

stockholm.data <- fhm.data[fhm.data$region == "Stockholm",]
stockholm.data$tot_cases <- cumsum(stockholm.data$new_cases)
stockholm.data.elim <- stockholm.data[stockholm.data$day_nbr_region > 5,]

model.lin <- lm(new_cases ~ day_nbr_region + I(day_nbr_region^2) + I(day_nbr_region^3), data = stockholm.data)
stockholm.pred.lin <- cbind(stockholm.data,
                        fit = predict(model.lin, stockholm.data))
model.pois <- glm(new_cases ~ day_nbr_region, family="poisson", data=stockholm.data)
stockholm.pred.pois <- cbind(stockholm.data,
                            fit = exp(predict(model.pois, stockholm.data)))

pred_plot <- function(pred) {
  ggplot(pred, aes(x = day_nbr_region, y = new_cases)) +
    geom_point() +
    geom_line(data = pred, aes(x = day_nbr_region, y = fit))
}

pred_plot(stockholm.pred.lin)
pred_plot(stockholm.pred.pois)

ggplot(fhm.data[fhm.data$region == "Värmland",], aes(x = day_nbr_region, y = new_cases)) +
  geom_point() + 
  geom_smooth()

chosen <- c("Stockholm", "Östergötland", "Sörmland", "Västmanland", "Dalarna", "Örebro", "Västerbotten", "Jönköping")

chosen.data <- fhm.data[fhm.data$region %in% chosen,]
not_chosen.data <- fhm.data[!(fhm.data$region %in% chosen),]

ggplot(chosen.data, aes(x = day_nbr_region, y = new_cases)) +
  geom_point() +
  geom_smooth() +
  facet_wrap( ~region )

day_top <- data.frame(dt = c(48, 25, 32, 37, 45, 53, 32, 61))
idx = 1
ggplot(chosen.data[chosen.data$region == chosen[idx],], aes(x = day_nbr_region, y = new_cases)) +
  geom_point() +
  geom_vline(xintercept = day_top[idx,]) +
  geom_smooth() +
  labs(title = "New cases for Stockholm with a moving average", caption = "vertical line: peak of new cases")

p_before <- chosen.data[(chosen.data$region == chosen[idx] & chosen.data$day_nbr_region < day_top[idx,]),]
p_after <- chosen.data[(chosen.data$region == chosen[idx] & chosen.data$day_nbr_region >= day_top[idx,]),]

ggplot(chosen.data[chosen.data$region == chosen[idx],], aes(x = day_nbr_region, y = new_cases)) +
  geom_point() +
  geom_point(data = p_before, aes(x = day_nbr_region, y = new_cases), size = 3, shape = 23, color = 'red') +
  geom_point(data = p_after, aes(x = day_nbr_region, y = new_cases), size = 3, shape = 23, color = 'green') +
  geom_vline(xintercept = day_top[idx,]) +
  geom_smooth() +
  labs(title = "New cases for Stockholm with a moving average", caption = "vertical line: peak of new cases")


for (i in 1:length(chosen)) {
  data <- chosen.data[chosen.data$region == chosen[i],]
  chosen.data[chosen.data$region == chosen[i], 'day_after'] <- as.numeric(data$day_nbr_region > day_top[i,])
  chosen.data[chosen.data$region == chosen[i], 'day_rise'] <- data$day_nbr_region*(1 - chosen.data[chosen.data$region == chosen[i], 'day_after']) + day_top[i,]*chosen.data[chosen.data$region == chosen[i], 'day_after']
  chosen.data[chosen.data$region == chosen[i], 'day_fall'] <- data$day_nbr_region*chosen.data[chosen.data$region == chosen[i], 'day_after'] + day_top[i,]*(1 - chosen.data[chosen.data$region == chosen[i], 'day_after'])
}

model.piece.linear <- lm(new_cases ~ (day_rise + day_fall) * region, data = chosen.data)
model.piece.log <- lm(log(new_cases + 1) ~ (day_rise + day_fall) * region, data = chosen.data)
model.piece.poly <- lm(new_cases ~ (I(day_rise^2) + I(day_fall^2)) * region, data = chosen.data)


pred.piece.linear <- cbind(chosen.data,
                   fit = predict(model.piece.linear, chosen.data))
pred.piece.log <- cbind(chosen.data,
                           fit = exp(predict(model.piece.log, chosen.data)))
pred.piece.poly <- cbind(chosen.data,
                        fit = predict(model.piece.poly, chosen.data))


ggplot(pred.piece.poly, aes(x = day_nbr_region, y = new_cases)) +
  geom_point() +
  geom_smooth(color='green') +
  geom_line(data = pred.piece.poly, aes(x = day_rise, y = fit), color = 'red') + 
  geom_line(data = pred.piece.poly, aes(x = day_fall, y = fit), color = 'blue') +
  facet_wrap ( ~region, scales = "free_y" )

X <- data.frame(day_nbr_region = c(), day_rise = c(), day_fall = c(), region = c())

for (i in 1:length(chosen)) {
  X <- rbind(X, data.frame(day_nbr_region = -20:day_top[i,], day_rise = -20:day_top[i,], day_fall = day_top[i,],region = chosen[i]))
  X <- rbind(X, data.frame(day_nbr_region = day_top[i,]:150, day_rise = day_top[i,], day_fall = day_top[i,]:150,region = chosen[i]))
}

pred.piece.longer <- cbind(X,
                           fit = exp(predict(model.piece, X)),
                           conf = exp(predict(model.piece, X, 
                                          interval = "confidence")),
                           pred = exp(predict(model.piece, X,
                                          interval = "prediction")))
reg = "Dalarna"
ggplot(chosen.data[chosen.data$region == reg,], aes(x = day_nbr_region, y = new_cases)) +
  geom_point() + 
  geom_smooth() +
  geom_line(data = pred.piece.longer[pred.piece.longer$region == reg,], aes(x = day_rise, y = fit), color = 'red') + 
  geom_line(data = pred.piece.longer[pred.piece.longer$region == reg,], aes(x = day_fall, y = fit), color = 'blue') +
  geom_line(data = pred.piece.longer[pred.piece.longer$region == reg,], aes(y = conf.lwr),
            color = "red", linetype = "dashed", size = 1) +
  geom_line(data = pred.piece.longer[pred.piece.longer$region == reg,], aes(y = conf.upr),
            color = "red", linetype = "dashed", size = 1)

pred.piece.linear$v <- influence(model.piece.linear)$hat
pred.piece.log$v <- influence(model.piece.log)$hat
pred.piece.poly$v <- influence(model.piece.poly)$hat

pred.piece.linear$e <- model.piece.linear$residuals
pred.piece.log$e <- model.piece.log$residuals
pred.piece.poly$e <- model.piece.poly$residuals

pred.piece.linear$r <- rstudent(model.piece.linear)
pred.piece.log$r <- rstudent(model.piece.log)
pred.piece.poly$r <- rstudent(model.piece.poly)

for (region in unique(pred.piece$region)) {
  pred.piece[(pred.piece$region == region), c("ubound")] <- with(pred.piece, 1/sum(pred.piece$region == region))
  pred.piece[(pred.piece$region == region), c("lbound")] <- with(pred.piece, 2*3/sum(pred.piece$region == region))
}

ggplot() +
   #geom_point(data = pred.piece.linear, aes(x = fit, y = e)) +
   geom_point(data = pred.piece.poly, aes(x = fit, y = e)) +
  #geom_point(data = pred.piece.log, aes(x = fit, y = e)) +
  facet_wrap(~ region, scales = "free") +
  #labs(caption = "y = 1/n (black) and 2(p+1)/n (red)", title = "Leverages") +
  theme(text = element_text(size = 18)) +
  geom_hline(yintercept = 0, color = "transparent")

ggplot(data = pred.piece.log, aes(sample = e)) +
  geom_qq() +
  geom_qq_line() +
  #facet_wrap(~ region, scales = "free") +
  #labs(caption = "y = 1/n (black) and 2(p+1)/n (red)", title = "Leverages") +
  theme(text = element_text(size = 18))

ggplot() +
  geom_point(data = pred.piece.linear, aes(x = fit, y = r, color = 'linear')) +
  geom_point(data = pred.piece.log, aes(x = fit, y = r, color = 'log')) +
  geom_point(data = pred.piece.poly, aes(x = fit, y = r, color = "poly")) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), color = "red") +
  geom_hline(yintercept = c(-4, 4), color = "red", linetype = "dashed") +
  facet_wrap( ~region, scales = 'free_x') +
  xlab("Fitted values") +
  ylab("r*") +
  labs(title = "Pike: studentized residuals vs fitted values") +
  labs(caption = "y = +/- 2 and +/- 4") +
  theme(text = element_text(size = 18))



model.piece.reduced <- step(model.piece, k = log(nrow(chosen.data)))

X <- data.frame(day_nbr_region = c(), day_rise = c(), day_fall = c(), region = c())

for (i in 1:length(chosen)) {
  X <- rbind(X, data.frame(day_nbr_region = -20:day_top[i,], day_rise = -20:day_top[i,], day_fall = day_top[i,],region = chosen[i]))
  X <- rbind(X, data.frame(day_nbr_region = day_top[i,]:150, day_rise = day_top[i,], day_fall = day_top[i,]:150,region = chosen[i]))
}

pred.piece.reduced.longer <- cbind(X,
                           fit = exp(predict(model.piece.reduced, X)),
                           conf = exp(predict(model.piece.reduced, X, 
                                              interval = "confidence")),
                           pred = exp(predict(model.piece.reduced, X,
                                              interval = "prediction")))
reg = "Stockholm"
ggplot(fhm.data[fhm.data$region == reg,], aes(x = day_nbr_region, y = new_cases)) +
  geom_point() + 
  geom_smooth() +
  geom_line(data = pred.piece.reduced.longer[pred.piece.reduced.longer$region == reg,], aes(x = day_rise, y = fit), color = 'red') + 
  geom_line(data = pred.piece.reduced.longer[pred.piece.reduced.longer$region == reg,], aes(x = day_fall, y = fit), color = 'blue') +
  geom_line(data = pred.piece.longer[pred.piece.longer$region == reg,], aes(y = conf.lwr),
            color = "red", linetype = "dashed", size = 1) +
  geom_line(data = pred.piece.longer[pred.piece.longer$region == reg,], aes(y = conf.upr),
            color = "red", linetype = "dashed", size = 1)



reduced <- c('Västmanland', 'Dalarna', 'Örebro')

pred.piece.linear.red <- pred.piece.linear[pred.piece.linear$region %in% reduced,]

ggplot() +
  geom_point(data = pred.piece.linear.red, aes(x = fit, y = e, color = region)) +
  #geom_point(data = pred.piece.poly, aes(x = fit, y = e)) +
  #geom_point(data = pred.piece.log, aes(x = fit, y = e)) +
  #facet_wrap(~ region, scales = "free") +
  #labs(caption = "y = 1/n (black) and 2(p+1)/n (red)", title = "Leverages") +
  theme(text = element_text(size = 18)) +
  geom_hline(yintercept = 0, color = "transparent")
