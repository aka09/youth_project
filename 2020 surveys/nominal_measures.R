library(tidyverse)
library(ggplot2)
library(cregg)
library(gtools)
library(viridis)
library(extrafont)

# Loading our data from 2020
load("cleaned_data/survey_2020_pooled.RData")

# Custom color scale for results by generation
gen_cols <- c(viridis(6)[1:5], "#D53992")

# Importance, by age
survey_pooled %>% 
  filter(age <= 75) %>% 
  group_by(age, Country) %>% 
  summarise(mean = mean(importance_democracy, na.rm = T),
            lwr = lwr_conf(importance_democracy),
            upr = upr_conf(importance_democracy),
            n = n()) %>% 
  ggplot(aes(x = age, y = mean, 
             ymin = lwr, ymax = upr, size = n)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~Country) +
  geom_smooth(aes(weight = n), linetype = "dashed", col = "red") +
  guides(size = FALSE) +
  labs(y = "Importance of democracy (0-10)",
       x = "Age",
       caption = "Data from original 2020 survey; only ages 18-75") +
  scale_x_continuous(breaks = seq(20, 70, 10)) +
  scale_y_continuous(limits = c(5,10)) +
  theme_minimal(base_family = "Fira Sans")

ggsave("2020 surveys/figures/imp~age.png", height = 7, width = 9)

# Correlation between importance and strong leader
survey_pooled %>% 
  filter(!is.na(generation) & !is.na(importance_democracy)) %>% 
  mutate(strong_leader = ifelse(strong_leader %in% c(0.66,1), 1, 0),
         important_quart = quantcut(importance_democracy, q = 3)) %>%
  group_by(Country, important_quart, generation) %>% 
  summarise(mean = mean(strong_leader, na.rm = T),
            lwr = lwr_conf(strong_leader),
            upr = upr_conf(strong_leader)) %>% 
  mutate(generation = factor(generation, 
                             levels = c("Pre-1950s", "1950s", "1960s",
                                        "1970s", "1980s", "Post-1980s")),
         important_quart = recode(important_quart,
                                  "[0,8]" = "0 to 8",
                                  "(8,10)" = "9")) %>% 
  ggplot(aes(x = important_quart, y = mean, 
             ymin = lwr, ymax = upr, col = generation)) +
  geom_point(position = position_dodge(0.5)) +
  geom_pointrange(position = position_dodge(0.5)) +
  facet_wrap(~Country) +
  scale_y_continuous(breaks = seq(0.1, 0.6, 0.1)) +
  scale_color_manual(values = gen_cols) +
  theme_bw(base_family = "Fira Sans") %+replace%
  theme(legend.position = "bottom") +
  labs(y = "Proportion endorsing strong leader",
       x = "Stated importance of democracy",
       caption = "Data from original 2020 survey")

ggsave("2020 surveys/figures/strong~important+country+gen.png", height = 6, width = 8)
