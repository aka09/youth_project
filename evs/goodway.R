library(tidyverse)
library(ggplot2)
library(gtools)
library(DescTools)
library(viridis)

load("cleaned_data/evs_w5.RData")
source("functions/confint.R")
# Custom color scale for results by generation
gen_cols <- c(viridis(6)[1:5], "#D53992")

evs5 %>% 
  filter(!is.na(dem_important)) %>% 
  mutate(across(sys1_strongleader:sys2_army, ~ifelse(. %in% c(1,2), 1, 0)),
         dem_important_cut = cut(dem_important, breaks = c(0, 3, 6, 8, 10))) %>% 
  group_by(generation, dem_important_cut) %>% 
  summarise(strongleader = mean(sys1_strongleader,na.rm = T),
            lwr = lwr_conf(strongleader),
            upr = upr_conf(strongleader)) %>% 
  ggplot(aes(x = dem_important_cut, y = strongleader, 
             ymin = lwr, ymax = upr, col = generation)) +
  geom_point(position = position_dodge(0.75), size = 3) +
  geom_pointrange(position = position_dodge(0.75)) +
  scale_color_manual(values = gen_cols) +
  theme_minimal(base_family = "Fira Sans") +
  labs(y = "Prop. supporting strong leader",
       x = "Importance of democracy (0-10)")

evs5 %>% 
  filter(dem_important == 10 & !is.na(generation)) %>% 
  mutate(across(sys1_strongleader:sys2_army, ~ifelse(. %in% c(1,2), 1, 0))) %>% 
  group_by(country_name) %>% 
  summarise(mean = mean(sys1_strongleader,na.rm = T),
            lwr = lwr_conf(sys1_strongleader),
            upr = upr_conf(sys1_strongleader)) %>%
  arrange(mean) %>% View()
