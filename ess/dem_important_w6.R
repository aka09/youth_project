library(tidyverse)
library(haven)
library(magrittr)
library(ggplot2)
library(extrafont)
library(gtools)
library(janitor)
library(viridis)
library(DescTools)
library(margins)
source("functions/confint.R")

# Data from 6th ESS wave
load("cleaned_data/ess_clean.RData")

# Custom color scale for results by generation
gen_cols <- c(viridis(6)[1:5], "#D53992")

# Vector of country names we're interested in
country_list <- c(
  "Belgium", "Denmark", "Finland", "France", "Germany", "Iceland", 
  "Ireland", "Italy", "Netherlands", "Norway", "Portugal", "Spain", 
  "Sweden", "Switzerland", "United Kingdom"
)

# Importance by age and country
ess6 %>% 
  filter(age >= 18 & age < 80 & country_name %in% country_list) %>% 
  group_by(age, country_name) %>% 
  summarise(mean_important = weighted.mean(dem_important, 
                                           na.rm = T,
                                           w = pspwght),
            lwr = lwr_conf(dem_important),
            upr = upr_conf(dem_important),
            `Sample size` = n()) %>% 
  ggplot(aes(x = age, y = mean_important, weight = `Sample size`)) +
  geom_point(aes(size = `Sample size`), alpha = 0.2) +
  geom_smooth(se = FALSE, col = "red") +
  labs(x = "Age",
       y = "Importance of democracy (0-10)",
       caption = "Loess line weighted by sample size; data from ESS Wave 6") +
  scale_x_continuous(breaks = seq(20, 80, 10)) +
  theme_bw(base_family = "Fira Sans") +
  facet_wrap(~country_name) +
  guides(size = FALSE)

ggsave("ess/figures/important~age+country.png", height = 10, width = 10)

# Importance by birth cohort and country
ess6 %>% 
  filter(country_name %in% country_list &
           !is.na(cohort)) %>% 
  group_by(cohort, country_name) %>% 
  summarise(mean_important = mean(dem_important, na.rm = T),
            lwr = lwr_conf(dem_important),
            upr = upr_conf(dem_important)) %>% 
  ggplot(aes(x = cohort, y = mean_important, 
             ymin = lwr, ymax = upr)) +
  geom_point(size = 2.5) +
  geom_pointrange() +
  labs(x = "Birth cohort",
       y = "Importance of democracy (0-10)",
       caption = "Data from ESS Wave 6") +
  scale_y_continuous(breaks = seq(7.5, 10, 0.5)) +
  theme_bw(base_family = "Fira Sans",
           base_size = 13) +
  facet_wrap(~country_name)

ggsave("ess/figures/important~cohort+country.png", height = 10, width = 14)

# Satisfaction by age and country
ess6 %>% 
  filter(age >= 18 & age < 80 & country_name %in% country_list) %>% 
  group_by(age, country_name) %>% 
  summarise(mean_satisfied = weighted.mean(dem_satisfied, 
                                           na.rm = T,
                                           w = pspwght),
            lwr = lwr_conf(dem_satisfied),
            upr = upr_conf(dem_satisfied),
            `Sample size` = n()) %>% 
  ggplot(aes(x = age, y = mean_satisfied, weight = `Sample size`)) +
  geom_point(aes(size = `Sample size`), alpha = 0.2) +
  geom_smooth(se = FALSE, col = "red") +
  labs(x = "Age",
       y = "Satisfaction democracy (0-10)",
       caption = "Loess line weighted by sample size; data from ESS Wave 6") +
  scale_x_continuous(breaks = seq(20, 80, 10)) +
  theme_bw(base_family = "Fira Sans") +
  facet_wrap(~country_name) +
  guides(size = FALSE)

ggsave("ess/figures/satisfaction~age+country.png", height = 10, width = 10)

# Satisfaction by cohort and country
ess6 %>% 
  filter(country_name %in% country_list & !is.na(cohort)) %>% 
  group_by(cohort, country_name) %>% 
  summarise(mean_satisfied =mean(dem_satisfied, na.rm = T),
            lwr = lwr_conf(dem_satisfied),
            upr = upr_conf(dem_satisfied),
            `Sample size` = n()) %>% 
  ggplot(aes(x = cohort, y = mean_satisfied,
             ymin = lwr, ymax = upr)) +
  geom_point() +
  geom_pointrange() +
  labs(x = "Birth cohort",
       y = "Satisfaction democracy (0-10)",
       caption = "Loess line weighted by sample size; data from ESS Wave 6") +
  theme_bw(base_family = "Fira Sans",
           base_size = 13) +
  facet_wrap(~country_name)

ggsave("ess/figures/satisfaction~cohort+country.png", height = 10, width = 14)


# Importance by satisfaction and generation
ess6 %>% 
  filter(!is.na(cohort) & dem_satisfied <= 10 &
           country_name %in% country_list) %>% 
  mutate(satisfied_quart = quantcut(dem_satisfied)) %>% 
  group_by(cohort, satisfied_quart) %>% 
  summarise(mean_important = mean(dem_important, na.rm = T),
            lwr = lwr_conf(dem_important),
            upr = upr_conf(dem_important)) %>% 
  ggplot(aes(x = satisfied_quart, y = mean_important, 
             ymin = lwr, ymax = upr, col = cohort)) +
  geom_point(position = position_dodge(0.75), size = 3) +
  geom_pointrange(position = position_dodge(0.75)) +
  scale_color_manual(values = gen_cols) +
  scale_y_continuous(breaks = seq(7.25, 9.75, 0.25)) +
  theme_minimal(base_family = "Fira Sans") +
  labs(y = "Importance of democracy (0-10)",
       x = "Satisfaction with democracy (0-10)")

ggsave("ess/figures/importance~satisfied+gen.png", height = 7, width = 9)

# Model: importance ~ satisfaction*generation
m1 <- lm(dem_important ~ dem_satisfied*generation + country_name, 
         data = filter(ess6, country_name %in% country_list))

cplot(m1, "generation", what = "effect")

# Proportion of 0s and 10s
ess6 %>% 
  filter(!is.na(generation)) %>% 
  mutate(dem0or1 = ifelse(dem_important %in% c(0,1,2), 1, 0)) %>% 
  group_by(generation) %>% 
  summarise(mean(dem0or1,na.rm = T))
