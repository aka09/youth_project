library(tidyverse)
library(ggplot2)
library(DescTools)
library(viridis)

# Data from EVS Wave 5
load("cleaned_data/evs_w5.RData")
source("functions/confint.R")

# Vector of country names we're interested in
country_list <- c(
  "Austria", "Denmark", "Finland", "France", "Germany", "Iceland", 
  "Italy", "Netherlands", "Norway", "Portugal", "Spain", 
  "Sweden", "Switzerland", "United Kingdom"
)

# Custom color scale for results by generation
gen_cols <- c(viridis(6)[1:5], "#D53992")

# Correlations between nominal items for each age 
evs5 %>% 
  filter(birth_year >= 1945 & birth_year <= 2000 & 
           country_name %in% country_list) %>% 
  group_by(birth_year) %>% 
  summarise(`Strong leader` = 
              cor(sys1_strongleader, sys3_democracy,
                  use = "complete.obs"),
            `Army rule` = 
              cor(sys2_army, sys3_democracy,
                  use = "complete.obs"),
            `Democracy (importance)` = 
              cor(dem_important, sys3_democracy,
                  use = "complete.obs"),
            n = n()) %>% 
  pivot_longer(cols = c(`Strong leader`, 
                        `Army rule`,
                        `Democracy (importance)`),
               names_to = "Variables") %>% 
  ggplot(aes(x = birth_year, y = value, size = n, weight = n, col = Variables)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE) +
  labs(x = "Birth year",
       y = "Correlations between items") +
  scale_x_continuous(breaks = seq(1945, 2000, 5)) +
  theme_bw(base_family = "Fira Sans",
           base_size = 18) %+replace%
  theme(legend.position = "bottom") +
  guides(size = FALSE)

ggsave("evs/figures/nominal_corr.png", height = 7, width = 12)

# How many of those who answer that democracy is important (8-10, each)
# endorse a strong leader
evs5 %>% 
  filter(country_name %in% country_list & dem_important >= 8) %>% 
  # dummy: answering that strong leader is very good or fairly good
  mutate(across(sys1_strongleader:sys2_army, ~ifelse(. %in% c(1,2), 1, 0))) %>% 
  group_by(dem_important, country_name) %>% 
  summarise(mean = mean(sys1_strongleader, na.rm = T),
            lwr = lwr_conf(sys1_strongleader),
            upr = upr_conf(sys1_strongleader)) %>% 
  # pivot so as to arrange by ascending value 
  pivot_wider(values_from = c("mean", "lwr", "upr"),
              names_from = dem_important) %>% 
  # reordering country name by value for mean_10
  mutate(country_name = factor(country_name) %>% reorder(mean_10)) %>% 
  # repivoting to long format 
  pivot_longer(!country_name, 
               names_sep = "_",
               names_to = c(".value", "dem_important")) %>% 
  # importance of democracy has lost its order
  mutate(dem_important = factor(dem_important, levels = c("8", "9", "10"))) %>% 
  ggplot(aes(x = country_name, y = mean,
             ymin = lwr, ymax = upr, col = dem_important)) +
  geom_point(position = position_dodge(0.5)) +
  geom_pointrange(position = position_dodge(0.5)) +
  scale_y_continuous(breaks = seq(0.1, 0.6, 0.1)) +
  scale_color_manual(values = c(viridis(3)[1:2], "#D53992"),
                     name = "Stated importance of democracy") +
  theme_bw(base_family = "Fira Sans") %+replace%
  theme(legend.position = "bottom") +
  labs(y = "Proportion endorsing strong leader",
       x = "Country",
       caption = "Data from EVS Wave 5")

ggsave("evs/figures/strong~important+country.png", height = 6, width = 10)  


# How many of those who answer that democracy is "absolutely important" (10)
# endorse a strong leader; by generation
evs5 %>% 
  filter(country_name %in% country_list & dem_important %in% c(8,9,10) &
           !is.na(generation)) %>% 
  # dummy: answering that strong leader is very good or fairly good
  mutate(across(sys1_strongleader:sys2_army, ~ifelse(. %in% c(1,2), 1, 0))) %>% 
  group_by(generation, country_name) %>% 
  summarise(mean = mean(sys1_strongleader, na.rm = T),
            lwr = lwr_conf(sys1_strongleader),
            upr = upr_conf(sys1_strongleader)) %>% 
  # pivot so as to arrange by ascending value 
  pivot_wider(values_from = c("mean", "lwr", "upr"),
              names_from = generation) %>% 
  # reordering country name by value for mean_10
  mutate(country_name = factor(country_name) %>% reorder(`mean_Pre-1950s`)) %>% 
  # repivoting to long format 
  pivot_longer(!country_name, 
               names_sep = "_",
               names_to = c(".value", "generation")) %>% 
  # generation has lost its order
  mutate(generation = factor(generation, 
                             levels = c("Pre-1950s", "1950s", "1960s",
                                        "1970s", "1980s", "Post-1980s"))) %>% 
  ggplot(aes(x = country_name, y = mean,
             ymin = lwr, ymax = upr, col = generation)) +
  geom_point(position = position_dodge(0.5)) +
  geom_pointrange(position = position_dodge(0.5)) +
  scale_y_continuous(breaks = seq(0.1, 0.6, 0.1)) +
  scale_color_manual(values = gen_cols) +
  theme_bw(base_family = "Fira Sans") %+replace%
  theme(legend.position = "bottom") +
  labs(y = "Proportion endorsing strong leader",
       x = "Country",
       caption = "Data from EVS Wave 5")

ggsave("evs/figures/strong~important+country+gen.png", height = 6, width = 10)
