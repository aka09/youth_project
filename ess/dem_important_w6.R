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
# Oldest birth cohort: at 0 
# Each point is then deviation from Pre-1950s
# this decompresses the scale by taking out intercountry variation 
imp_cohort <- 
  ess6 %>% 
  filter(country_name %in% country_list &
           !is.na(cohort)) %>% 
  group_by(cohort, country_name) %>% 
  summarise(mean = mean(dem_important, na.rm = T),
            sd = sd(dem_important, na.rm = T),
            n = n()) %>% 
  # wide format to have each generation in a column 
  pivot_wider(values_from = c("mean", "sd", "n"),
              names_from = cohort) %>% 
  # for pre-1950s, give 0; or others, give diff from pre-1950s
  mutate(across(mean_1950s:`mean_Post-1980s`,
                .fns = list(diff = ~. - `mean_Pre-1950s`),
                .names = "{fn}.{col}"),
         `diff.mean_Pre-1950s` = 0,
         pre1950s.sd = `sd_Pre-1950s`,
         pre1950s.n = `n_Pre-1950s`) %>% 
  pivot_longer(cols = `mean_Pre-1950s`:`diff.mean_Pre-1950s`, 
               names_to = c(".value", "cohort"),
               names_sep = "_") %>% 
  mutate(
    cohort = factor(cohort,
                    levels = c("Pre-1950s", "1950s",
                               "1960s", "1970s",
                               "1980s", "Post-1980s")),
    s_p = sqrt(((pre1950s.n-1)*pre1950s.sd^2 + (n-1)*sd^2) /
                 (pre1950s.n + n - 2)),
    se = s_p*sqrt(1/pre1950s.n+1/n)
  ) %>% 
  mutate(
    upr = ifelse(diff.mean != 0, diff.mean + qnorm(0.975)*se, 0),
    lwr = ifelse(diff.mean != 0, diff.mean - qnorm(0.975)*se, 0),
    significant = ifelse((upr > 0 & lwr & 0) | (upr < 0 & lwr <0) &
                           cohort %in% c("1980s", "Post-1980s"),1,0)
  )

# it works! except I use critical value from the normal instead of 
# t distribution, hence the small discrepancies 
t.test(ess6$dem_important[ess6$cohort=="Pre-1950s" & ess6$country_name=="France"], ess6$dem_important[ess6$cohort=="1980s" & ess6$country_name=="France"], var.equal = TRUE)

imp_cohort %>% 
  mutate(cohort = recode(cohort, "Post-1980s" = "1990s+",
                         "Pre-1950s" = "<1950")) %>% 
ggplot(aes(x = cohort, y = diff.mean,
           ymin = lwr, ymax = upr, col = factor(significant))) +
  geom_point(size = 2.5) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Birth cohort",
       y = "Importance of democracy (centered around pre-1950s mean)",
       caption = "Data from ESS Wave 6; significant differences in red") +
  theme_bw(base_family = "Fira Sans",
           base_size = 16) +
  facet_wrap(~country_name,
             nrow = 5) +
  scale_color_manual(values = c("black", "red")) +
  guides(color = FALSE)
    
ggsave("ess/figures/important~cohort+country.png", height = 10, width = 11)

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
  facet_wrap(~country_name,
             nrow = 5)

ggsave("ess/figures/satisfaction~cohort+country.png", height = 10, width = 14)

# Satisfaction by birth cohort and country
# Oldest birth cohort: at 0 
# Each point is then deviation from Pre-1950s
# this decompresses the scale by taking out intercountry variation 
sat_cohort <- 
  ess6 %>% 
  filter(country_name %in% country_list &
           !is.na(cohort)) %>% 
  group_by(cohort, country_name) %>% 
  summarise(mean = mean(dem_satisfied, na.rm = T),
            sd = sd(dem_satisfied, na.rm = T),
            n = n()) %>% 
  # wide format to have each generation in a column 
  pivot_wider(values_from = c("mean", "sd", "n"),
              names_from = cohort) %>% 
  # for pre-1950s, give 0; or others, give diff from pre-1950s
  mutate(across(mean_1950s:`mean_Post-1980s`,
                .fns = list(diff = ~. - `mean_Pre-1950s`),
                .names = "{fn}.{col}"),
         `diff.mean_Pre-1950s` = 0,
         pre1950s.sd = `sd_Pre-1950s`,
         pre1950s.n = `n_Pre-1950s`) %>% 
  pivot_longer(cols = `mean_Pre-1950s`:`diff.mean_Pre-1950s`, 
               names_to = c(".value", "cohort"),
               names_sep = "_") %>% 
  mutate(
    cohort = factor(cohort,
                    levels = c("Pre-1950s", "1950s",
                               "1960s", "1970s",
                               "1980s", "Post-1980s")),
    s_p = sqrt(((pre1950s.n-1)*pre1950s.sd^2 + (n-1)*sd^2) /
                 (pre1950s.n + n - 2)),
    se = s_p*sqrt(1/pre1950s.n+1/n)
  ) %>% 
  mutate(
    upr = ifelse(diff.mean != 0, diff.mean + qnorm(0.975)*se, 0),
    lwr = ifelse(diff.mean != 0, diff.mean - qnorm(0.975)*se, 0),
    significant = ifelse(((upr > 0 & lwr > 0) | (upr < 0 & lwr <0)) &
                           cohort %in% c("1980s", "Post-1980s"),1,0)
  )
sat_cohort %>% 
  mutate(cohort = recode(cohort, "Post-1980s" = "1990s+",
                         "Pre-1950s" = "<1950")) %>% 
  ggplot(aes(x = cohort, y = diff.mean,
             ymin = lwr, ymax = upr, col = factor(significant))) +
  geom_point(size = 2.5) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Birth cohort",
       y = "Satisfaction with democracy (centered around pre-1950s mean)",
       caption = "Data from ESS Wave 6; significant differences in red") +
  theme_bw(base_family = "Fira Sans",
           base_size = 16) +
  facet_wrap(~country_name,
             nrow = 5) +
  scale_color_manual(values = c("black", "red")) +
  guides(color = FALSE)

ggsave("ess/figures/satisfied~cohort+country.png", height = 10, width = 11)


# Importance by dissatisfaction and generation
ess6 %>% 
  filter(!is.na(cohort) & dem_dissatisfied <= 10 &
           country_name %in% country_list) %>%
  mutate(dissatisfied_quart = quantcut(dem_dissatisfied)) %>% 
  group_by(cohort, dissatisfied_quart) %>% 
  summarise(mean_important = mean(dem_important, na.rm = T),
            lwr = lwr_conf(dem_important),
            upr = upr_conf(dem_important)) %>% 
  ggplot(aes(x = dissatisfied_quart, y = mean_important, 
             ymin = lwr, ymax = upr, col = cohort)) +
  geom_point(position = position_dodge(0.75), size = 3) +
  geom_pointrange(position = position_dodge(0.75)) +
  scale_color_manual(values = gen_cols,
                     name = "Birth cohort") +
  scale_y_continuous(breaks = seq(7.25, 9.75, 0.25)) +
  theme_minimal(base_family = "Fira Sans") +
  labs(y = "Importance of democracy (0-10)",
       x = "Dissatisfaction with democracy (0-10)")

ggsave("ess/figures/importance~satisfied+gen.png", height = 7, width = 9)

#----------------------------------------------
# Model: importance ~ satisfaction*cohort
m1 <- lm(dem_important ~ dem_dissatisfied*cohort + country_name, 
         data = filter(ess6, country_name %in% country_list))

cm <- c(`cohort1950s` = "1950s cohort",
        `cohort1960s` = "1960s cohort",
        `cohort1970s` = "1970s cohort",
        `cohort1980s` = "1980s cohort",
        `cohortPost-1980s` = "Post-1980s cohort",
        `dem_dissatisfied` = "Dissatisfaction with democracy",
        `dem_dissatisfied:cohort1950s` = "Dissatisfaction * 1950s cohort",
        `dem_dissatisfied:cohort1960s` = "Dissatisfaction * 1960s cohort",
        `dem_dissatisfied:cohort1970s` = "Dissatisfaction * 1970s cohort",
        `dem_dissatisfied:cohort1980s` = "Dissatisfaction * 1980s cohort")

modelsummary::modelsummary(
  m1, 
  output = "ess/tables/imp~satisfied+cohort.docx",
  coef_map = cm
)

m1_pred <- expand.grid(
  cohort = c("Pre-1950s", "Post-1980s"),
  dem_dissatisfied = 0:10,
  country_name = "United Kingdom"
)

m1_pred[,c("fit", "lwr", "upr")] <- 
  predict(m1, m1_pred, interval = "confidence")

ggplot(data = m1_pred,
       aes(x = dem_dissatisfied, y = fit,
           ymin = lwr, ymax = upr, fill = cohort, col = cohort)) +
  geom_line(linetype = "dashed") +
  geom_ribbon(alpha = 0.2) +
  labs(x = "Dissatisfaction with democracy (0-10)",
       y = "Predicted importance of democracy",
       caption = "Data from ESS Wave 6") +
  scale_y_continuous(breaks = seq(6, 10, 1),
                     limits = c(5.5, 10)) +
  scale_color_manual(values = c(gen_cols[1], gen_cols[6]),
                     name = "Birth cohort") +
  scale_fill_manual(values = c(gen_cols[1], gen_cols[6]),
                    name = "Birth cohort") +
  theme_bw(base_family = "Fira Sans",
           base_size = 16) %+replace%
  theme(legend.position = "bottom") +
  guides(col = FALSE)

ggsave("ess/figures/imp~satisfied+cohort.png", height = 7, width = 9)