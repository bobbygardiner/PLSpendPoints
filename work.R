library(dplyr)
library(ggplot2)

# The source for wages up to 14/15: https://www.reddit.com/r/soccer/comments/24z9pg/premier_league_wage_bill_data_since_the_20002001/
# The source for wages of 14/15: https://www.theguardian.com/football/2016/may/25/premier-league-finances-club-by-club-breakdown-david-conn
# Transfer fees were scraped from Transfermarkt
# 1. Read csvs of data, which contain Premier League wage and transfer spend from 2009/10 to 2014/15

spendpoints <- read.csv('~/PLSpendPoints/Data/PLspendpoints.csv') %>%
  select(-X)

averages <- read.csv('~/PLSpendPoints/Data/PLAverages.csv') %>%
  select(-X)

# For a model, a version of the data is needed that accounts for inflation from season to season.
# The easiest way to do this is to divide each team's transfer/wage spend in each season by the season average.
# PLAverages.csv gives us these averages, so we can do just that.
# We also want to create single data points for each team, so we need average finish and average spends.
# Average finish (mean), and average spends (mean) are also logged, and quadratic terms are created.
# 2. Create model suitable data

model_prep <- function(spendpoints, averages) {
  relative <- merge(spendpoints, averages, by = "season") %>%
    mutate(relative_wage_spend = wage_bill / avg_wage_bill,
         relative_transfer_spend = transferspend / avg_transfer_spend)
  
  teams <- relative %>%
    group_by(team) %>%
    summarise(avg_finish = log(mean(league_standing)),
              avg_rel_wage_spend = log(mean(relative_wage_spend)),
              avg_rel_tra_spend = log(mean(relative_transfer_spend)))
  
  return(teams)
}

model_data <- model_prep(spendpoints, averages)

#Linear regression model
#3. Create linear regression

fit <- lm(avg_finish ~ . - team, model_data)
summary(fit)

#We want to use the model to see whether Arsene Wenger has ever under-achieved relative to his inputs
#To do this, we want to make finish predictions from the model with confidence intervals

predictions_prep <- function(spendpoints, averages) {
  relative <- merge(spendpoints, averages, by = "season") %>%
    mutate(avg_rel_wage_spend = log(wage_bill / avg_wage_bill),
           avg_rel_tra_spend = log(transferspend / avg_transfer_spend)) %>%
    select(team, season, league_standing, avg_rel_wage_spend, avg_rel_tra_spend)
  
  return(relative)
}

prediction_data <- predictions_prep(spendpoints, averages)

predictions <- predict(fit, prediction_data, interval = "predict")

prediction_data <- cbind(prediction_data, predictions) %>%
  mutate(fit = exp(fit),
         lwr = exp(lwr),
         upr = exp(upr))

#4. Plot Arsenal model predictions vs results

prediction_data %>%
  mutate(upr = ifelse(upr > 20, 20, upr)) %>%
  filter(team == "Arsenal FC") %>%
  ggplot(aes(x = season)) +
  geom_point(aes(y = league_standing), size = 2) +
  geom_point(aes(y = upr), colour = "red", alpha = 0.7, size = 0.8) +
  geom_point(aes(y = lwr), colour = "red", alpha = 0.7, size = 0.8) +
  geom_segment(aes(y = lwr, yend = upr, x = season, xend = season), linetype = 2, colour = "red", alpha = 0.5) +
  coord_cartesian(ylim = c(1, 20)) +
  scale_y_continuous(trans = "reverse", breaks = c(1:20)) +
  labs(x = "\nSeason",
       y = "League Position\n",
       title = "Arsenal",
       subtitle = "Actual finish vs predicted range of finishes based on relative spending") +
  theme_minimal() +
  theme(plot.title=element_text(hjust = 0.5),
        plot.subtitle=element_text(hjust = 0.5))