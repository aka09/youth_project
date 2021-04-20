library(tidyverse)
library(ggplot2)
library(cregg)
library(gtools)
library(viridis)
library(extrafont)

# Loading our data from 2019
load("cleaned_data/survey_2019_pooled.RData")

# Custom color scale for results by generation
gen_cols <- c(viridis(5)[1:4], "#D53992")

# Importance, by age
survey_pooled %>% 
  filter(age <= 75) %>% 
  group_by(age, Country) %>% 
  summarise(mean = mean(democracy_imp, na.rm = T),
            lwr = lwr_conf(democracy_imp),
            upr = upr_conf(democracy_imp),
            n = n()) %>% 
  ggplot(aes(x = age, y = mean, 
             ymin = lwr, ymax = upr, size = n)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~Country) +
  geom_smooth(aes(weight = n), linetype = "dashed", col = "red") +
  guides(size = FALSE) +
  labs(y = "Importance of democracy (0-10)",
       x = "Age",
       caption = "Data from original 2019 survey; only ages 18-75") +
  scale_x_continuous(breaks = seq(20, 70, 10)) +
  theme_minimal(base_family = "Fira Sans")

ggsave("2019 surveys/figures/imp~age.png", height = 7, width = 9)
