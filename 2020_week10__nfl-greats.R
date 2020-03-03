season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

library(tidyverse)
library(extrafont)
library(ggplot2)
library(ggtext)
library(patchwork)
theme_set(theme_minimal())
loadfonts()
summary(season_goals)

cb4 <- c("#E69F00", "#56B4E9", "#009E73", "#D55E00") # colourblind friendly palette

data <- season_goals %>%
  filter(total_goals > 450) %>%
  select(player, age, goals, assists) %>%
  mutate(player = as.factor(player),
         type = "factual")

# create linear models fitting goals to player and polynomials of degree 1-5 to age
models <- list()
models <- lapply(1:5, function(x) lm(goals ~ poly(age, degree=x)+player, x=TRUE, y=TRUE, data=data))
names(models) <- 1:5
# use cross-validation to estimate error rates and find best choice of poly-degree
cv_mses <- list()
cv_mses <- lapply(1:5, function(x) cv.lm(models[[x]], k=6, seed = 1337)$MSE)
names(cv_mses) <- 1:5

for(i in 1:5){
  print(paste(names(cv_mses)[[i]], "degree model ==============="))
  print(paste("MSE: ", cv_mses[[i]]$mean))
  print(paste("SD: ", cv_mses[[i]]$sd))
}
# model with polynomial of degree 4 shows lowest MSE, will be used
model <- models[[4]]

# predict goals for ages 35-40 (including min-max values as confidence interval) and add to data
newdata <- data.frame(player=as.factor("Alex Ovechkin"),
                                 age=35:40)
predictions <- as.data.frame(predict(model, newdata = newdata, type = "response", interval = "predict")) %>%
  mutate(type = "predicted",
         goals = round(fit),
         max = round(upr),
         min = round(lwr)) %>%
  select(-(fit:upr))

newdata <- cbind(newdata, predictions)


# plot comparing Alex's scoring record(and predictions) against players of same age (and their mean)
# and data frames that are needed to make it
ageplot_temp <- rbind(select(data, -assists), select(newdata, -max, -min)) %>%
  filter(age >= 20, age <=40)
ageplot_others <- filter(ageplot_temp, player != "Alex Ovechkin")
ageplot_alex <- filter(ageplot_temp, player == "Alex Ovechkin")

ageplot <- ggplot(ageplot_others) + 
  geom_jitter(aes(x=age, y=goals), height = 0, width = 0.05, colour = "grey", size = 1) + 
  geom_point(data=ageplot_alex, aes(x=age, y=goals, colour = type), size = 4) +
  stat_summary(fun = mean, geom="line", aes(x =age, y=goals), size = 1) + 
  scale_colour_manual(values = c("factual" = cb4[1], "predicted" = cb4[4])) + 
  labs(x="Age", y="Goals",
       title = "A comparison of three greats: <span style=color:'#56B4E9';>Wayne Gretzky</span>, <span style=color:'#009E73';>Gordie Howe</span> and <span style=color:'#E69F00';>Alex Ovechkin</span>",
       subtitle = "According to this <span style=color:'#D55E00';>prediction</span>, <span style=color:'#E69F00';>Alex Ovechkin</span> will likely reach 900 goals if he continues until age 40.") +
  theme(text=element_text(family="Calibri"),
        legend.position = "none",
        plot.title=element_markdown(size=16, face = "bold"),
        plot.subtitle = element_markdown(size = 14))



# compare total sum of goals over career against age
# predictions 
pred_total_data <- filter(season_goals, (player == "Alex Ovechkin" | total_goals > 800),
                          team != "TOT", league == "NHL", age <= 40) %>%
  select(player, age, goals) %>% 
  group_by(player, age) %>% summarize(goals = sum(goals)) %>%
  mutate(sum_at_age = cumsum(goals))

pred_other_data <- filter(season_goals, !(player == "Alex Ovechkin" | total_goals > 800), 
                          team != "TOT", league == "NHL", age <= 40) %>%
  select(player, age, goals) %>%
  group_by(player, age) %>% summarize(goals = sum(goals)) %>%
  mutate(sum_at_age = cumsum(goals))

pred_alex <- newdata[1,] %>%
  mutate(goals = goals +700, max = max+700, min = min+700) %>%
  rbind(newdata[-1,]) %>% 
  group_by(player) %>%
  mutate(pred_at_age = cumsum(goals),
         max_at_age = cumsum(max),
         min_at_age = cumsum(min)) %>%
  bind_rows(data.frame(player = "Alex Ovechkin", age = 34, type="NA", goals=0, max=0,min=0, pred_at_age=700,
                   max_at_age=700, min_at_age = 700))
  
pred_total_plot <- ggplot(pred_total_data) + 
  geom_line(data=pred_other_data, aes(x=age, y=sum_at_age, group=player), colour = "grey", size = 0.5, alpha = 0.33) +
  geom_ribbon(data = pred_alex, aes(x=age, ymin=min_at_age, ymax=max_at_age),fill = "grey", alpha = 0.5) +
  geom_line(aes(x=age, y=sum_at_age, colour = player), size = 1.5) + 
  geom_line(data=pred_alex, aes(x=age, y=pred_at_age), size = 1.5,
            colour = cb4[4], linetype = "dashed") +
  scale_colour_manual(values = c("Alex Ovechkin" = cb4[1], "Wayne Gretzky" = cb4[2], "Gordie Howe" = cb4[3])) +
  labs(x="Age", y="Goals",
       subtitle = "He'll most likely reach more than Howe's 801 Goals and could\n realistically topple Gretzky's 894 goal record...") + 
  theme(text = element_text(family="Calibri"),
        legend.position = "none",
        plot.subtitle = element_text(size = 14))


#scatterplot to compare assists and goals over their career
ga_select <- filter(data, player %in% c("Alex Ovechkin", "Wayne Gretzky", "Gordie Howe"))
ga_others <- filter(data, !(player %in% c("Alex Ovechkin", "Wayne Gretzky", "Gordie Howe")))

ga_plot <- ggplot(ga_others) +
  geom_point(aes(x=goals, y=assists), colour="grey") + 
  geom_point(data=ga_select, aes(x=goals, y=assists, colour=player)) +
  stat_ellipse(data=ga_select, aes(x=goals, y=assists, colour = player, fill = player),
               type="t", geom="polygon", level = .95, alpha = 0.33) + 
  scale_colour_manual(values = c("Alex Ovechkin" = cb4[1], "Wayne Gretzky" = cb4[2], "Gordie Howe" = cb4[3])) +
  scale_fill_manual(values = c("Alex Ovechkin" = cb4[1], "Wayne Gretzky" = cb4[2], "Gordie Howe" = cb4[3])) +
  labs(x="Goals", y="Assists",
       subtitle = "... but Gretzky's assist record still sets him apart.") + 
  theme(text = element_text(family="Calibri"), legend.position = "none",
        plot.subtitle = element_text(size = 14))

ageplot / ( pred_total_plot | ga_plot)


ggsave(path = "output", filename="2020-03-03__nfl-greats.png", dpi = 600, width = 12, height = 8, type="cairo")
