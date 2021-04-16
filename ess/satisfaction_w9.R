library(tidyverse)
library(haven)
library(magrittr)
library(ggplot2)
library(extrafont)
library(DescTools)

source("functions/confint.R")

# Data from 9th ESS wave
ess9.orig <- read_dta("ess/data/ess_wave9.dta")
load("cleaned_data/ess_clean.RData")

# Renaming columns
ess9 <- 
  rename(ess9.orig,
         dem.satisfied = stfdem,
         gov.say = psppsgva,
         age = agea)

# Recoding 
ess9 %<>% 
  mutate(dem.satisfied.0 = ifelse(ess9$dem.satisfied==0, 1, 0),
         dem.satisfied.10 = ifelse(ess9$dem.satisfied==10, 1, 0))
  
# Distribution of satisfaction with democracy
ggplot(ess9, aes(x = dem.satisfied)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  labs(x = "Satisfaction with democracy (1-10)",
       y = "# of respondents") +
  theme_bw(base_family = "Fira Sans")

# Mean satisfaction by age (with loess)
ess9 %>% 
  group_by(age) %>% 
  summarise(mean.satisfied = weighted.mean(dem.satisfied, 
                                           na.rm = T,
                                           w = anweight),
            lwr = lwr_conf(dem.satisfied),
            upr = upr_conf(dem.satisfied),
            `Sample size` = n()) %>% 
  ggplot(aes(x = age, y = mean.satisfied, weight = `Sample size`)) +
  geom_point(aes(size = `Sample size`), alpha = 0.2) +
  geom_smooth(se = FALSE) +
  labs(x = "Age",
       y = "Satisfaction with democracy (0-10)",
       caption = "Loess line weighted by sample size") +
  scale_x_continuous(breaks = seq(15, 90, 5)) +
  theme_bw(base_family = "Fira Sans")

# With CIs instead 
ess9 %>% 
  group_by(age) %>% 
  summarise(mean.satisfied = weighted.mean(dem.satisfied, 
                                           na.rm = T,
                                           w = anweight),
            lwr = lwr_conf(dem.satisfied),
            upr = upr_conf(dem.satisfied),
            `Sample size` = n()) %>% 
  ggplot(aes(x = age, y = mean.satisfied,
             ymin = lwr, ymax = upr)) +
  geom_point(aes(alpha = `Sample size`)) +
  geom_pointrange(aes(alpha = `Sample size`)) +
  geom_smooth() +
  labs(x = "Age",
       y = "Satisfaction with democracy (0-10)",
       caption = "Point transparency represents n") +
  scale_x_continuous(breaks = seq(15, 90, 5)) +
  theme_bw(base_family = "Fira Sans")

# Proportion of 0s and 10s, by age
ess9 %>% 
  group_by(age) %>% 
  summarise(`Proportion of 0s` = weighted.mean(dem.satisfied.0, 
                                               na.rm = T,
                                               weight = anweight),
            `Proportion of 10s` = weighted.mean(dem.satisfied.10, 
                                                na.rm = T,
                                                weight = anweight),
            `Sample size` = n()) %>% 
  pivot_longer(cols = c("Proportion of 0s", "Proportion of 10s"),
               values_to = "proportion",
               names_to = "type") %>% 
  ggplot(aes(x = age, y = proportion, weight = `Sample size`)) +
  geom_point(aes(size = `Sample size`), alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~type) +
  labs(x = "Age",
       y = "Proportion not satisfied at all (0)",
       caption = "Point transparency represents n") +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  scale_y_continuous(breaks = seq(0, .1, .02)) +
  theme_bw(base_family = "Fira Sans")

# by age and country
ess_cum %>% 
  filter(age >= 18 & age <= 75) %>% 
  filter(dem_satisfied < 11) %>% 
  group_by(age, country) %>% 
  summarise(mean_satisfied = weighted.mean(dem_satisfied, 
                                           na.rm = T,
                                           w = pspwght),
            lwr = lwr_conf(dem_satisfied),
            upr = upr_conf(dem_satisfied),
            `Sample size` = n()) %>% 
  ggplot(aes(x = age, y = mean_satisfied, weight = `Sample size`)) +
  geom_point(aes(size = `Sample size`), alpha = 0.2) +
  geom_smooth(se = FALSE) +
  labs(x = "Age",
       y = "Satisfaction with democracy (0-10)") +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  theme_bw(base_family = "Fira Sans",
           base_size = 12) +
  guides(size = FALSE) +
  facet_wrap(~country)

ggsave("ess/satisfaction_age_country.png", height = 10, width = 10)

# Centered around 0 (0 is country-level mean)
ess_cum %>% 
  filter(age >= 18 & age <= 75) %>% 
  filter(dem_satisfied < 11) %>% 
  group_by(country) %>% 
  mutate(satisfied_diff = dem_satisfied - mean(dem_satisfied, na.rm = T)) %>% 
  group_by(age, country) %>% 
  summarise(mean_satisfied = weighted.mean(satisfied_diff, 
                                           na.rm = T,
                                           w = pspwght),
            lwr = lwr_conf(satisfied_diff),
            upr = upr_conf(satisfied_diff),
            `Sample size` = n()) %>% 
  ggplot(aes(x = age, y = mean_satisfied, weight = `Sample size`)) +
  geom_point(aes(size = `Sample size`), alpha = 0.2) +
  geom_smooth(se = FALSE) +
  labs(x = "Age",
       y = "Satisfaction with democracy (0-10)") +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  theme_bw(base_family = "Fira Sans",
           base_size = 12) +
  guides(size = FALSE) +
  facet_wrap(~country)

ggsave("ess/satisfaction_age_country(diff).png", height = 10, width = 10)
