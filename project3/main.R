library(ggplot2)
library(dplyr)
library(MASS)

load("fhm_data.RData")

region_group <- fhm.data %>% group_by(obs_date) %>% summarise(tot = sum(new_cases))

ggplot(region_group, aes(x = obs_date, y = tot)) +
  geom_point()

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
