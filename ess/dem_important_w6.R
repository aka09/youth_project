library(tidyverse)
library(haven)
library(magrittr)
library(ggplot2)
library(extrafont)
library(DescTools)
library(janitor)

source("functions/confint.R")

# Data from 6th ESS wave
ess6.orig <- read_dta("ess/data/ess_wave6.dta")

# Renaming columns
ess6 <- 
  rename(ess6.orig,
         dem_satisfied = stfdem,
         dem_important = implvdm,
         age = agea)

# Recoding
ess6 %<>% 
  mutate(
    generation = case_when(
      age >= 18 & age < 25 ~ "18-24",
      age >= 25 & age < 35 ~ "25-34",
      age >= 35 & age < 45 ~ "35-44",
      age >= 45 & age < 54 ~ "45-54",
      age >= 55 & age < 65 ~ "55-64",
      age >= 65 ~ "65+",
    ),
    dem_important_0 = ifelse(dem_important==0, 1, 0),
    dem_important_10 = ifelse(dem_important==10, 1, 0)
  )

# Saving data in the cleaned data folder 
write_dta(data = ess6, path = "cleaned_data/ess6.dta")

ess6 %>% 
  filter(!is.na(generation)) %>% 
  group_by(generation) %>% 
  summarize(cor = cor(dem.important, dem.satisfied, 
                      use = "complete.obs"))

# Mean importance by age (with loess)
ess6 %>% 
  filter(age < 90) %>% 
  group_by(age) %>% 
  summarise(mean.important = weighted.mean(dem.important, 
                                           na.rm = T,
                                           w = pspwght),
            lwr = lwr_conf(dem.important),
            upr = upr_conf(dem.important),
            `Sample size` = n()) %>% 
  ggplot(aes(x = age, y = mean.important, weight = `Sample size`)) +
  geom_point(aes(size = `Sample size`), alpha = 0.2) +
  geom_smooth(se = FALSE) +
  labs(x = "Age",
       y = "Importance of democracy (0-10)",
       caption = "Loess line weighted by sample size") +
  scale_x_continuous(breaks = seq(15, 90, 5)) +
  theme_bw(base_family = "Fira Sans")

# With CIs instead 
ess6 %>% 
  filter(age < 90) %>% 
  group_by(age) %>% 
  summarise(mean.important = weighted.mean(dem.important, 
                                           na.rm = T,
                                           w = pspwght),
            lwr = lwr_conf(dem.important),
            upr = upr_conf(dem.important),
            `Sample size` = n()) %>% 
  ggplot(aes(x = age, y = mean.important,
             ymin = lwr, ymax = upr)) +
  geom_point(aes(alpha = `Sample size`)) +
  geom_pointrange(aes(alpha = `Sample size`)) +
  geom_smooth() +
  labs(x = "Age",
       y = "Importance of democracy (0-10)",
       caption = "Point transparency represents n") +
  scale_x_continuous(breaks = seq(15, 90, 5)) +
  theme_bw(base_family = "Fira Sans")

# Proportion of 0s and 10s, by age
ess6 %>% 
  filter(age < 90) %>% 
  group_by(age) %>% 
  summarise(`Proportion of 0s` = weighted.mean(dem.important.0, 
                                               na.rm = T,
                                               weight = pspwght),
            `Proportion of 10s` = weighted.mean(dem.important.10, 
                                                na.rm = T,
                                                weight = pspwght),
            `Sample size` = n()) %>% 
  pivot_longer(cols = c("Proportion of 0s", "Proportion of 10s"),
               values_to = "proportion",
               names_to = "type") %>% 
  ggplot(aes(x = age, y = proportion, weight = `Sample size`)) +
  geom_point(aes(size = `Sample size`), alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~type, scales = "free_y") +
  labs(x = "Age",
       y = "Proportion not important at all (0)",
       caption = "Point transparency represents n") +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  theme_bw(base_family = "Fira Sans")
