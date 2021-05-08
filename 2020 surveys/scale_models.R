library(tidyverse)
library(ggplot2)
library(extrafont)
library(magrittr)
library(margins)

# Loading our data from 2020
load("cleaned_data/survey_2020_pooled.RData")

# Custom color scale for results by generation
gen_cols <- c(viridis(6)[1:5], "#D53992")

# to reorder factors when necessary
gens <- c("Pre-1950s", "1950s", "1960s",
          "1970s", "1980s", "Post-1980s")

# Scale of "old" backsliding from 2018 survey 
survey_pooled %<>% 
  mutate(aggrandize_scale = (oldbackslide1_veto_std + oldbackslide2_obstruction_std +
           oldbackslide3_ignore_std + oldbackslide4_appoint_std) / 4,
         unemployed = ifelse((Q155 %in% c("Temporarily laid off", 
                                         "Unemployed/looking for work") &
                                Country == "United States") |
                               (employment_status %in% c("Temporarily laid off", 
                                                        "Unemployed/looking for work") &
                                  Country == "Canada"), 1, 0),
         educ_cat = recode(educ_cat, "Master's degree" = "Graduate degree",
                           "PhD/Doctorate" = "Graduate degree"),
         generation = factor(generation, levels = gens))

# Formula for regression models (without region so I can reuse for Canada)
agg_formula <- formula(
  aggrandize_scale ~ generation + partyid + gender + race + unemployed +
    deprivation_scale + educ_cat
)

# USA only
survey_usa <- filter(survey_pooled, Country == "United States")
# Canada only 
survey_can <- filter(survey_pooled, Country == "Canada")

# First model: without democratic knowledge
m1_agg_usa <- lm(
  formula = update.formula(agg_formula, . ~ . + region),
  data = survey_usa
)

# Second model: adding democratic knowledge
m2_agg_know_usa <- lm(
  formula = update.formula(agg_formula, . ~ . + region + dem_knowledge),
  data = survey_usa
)

# Data to predict with
agg_pred <- expand.grid(
  generation = c("Pre-1950s", "1950s", "1960s", 
                 "1970s", "1980s", "Post-1980s"),
  partyid = "Democrat",
  gender = "Female",
  race = "White",
  unemployed = 0,
  deprivation_scale = mean(survey_usa$deprivation_scale,na.rm=T),
  educ_cat = "High school graduate",
  region = "Northeast",
  dem_knowledge = mean(survey_usa$dem_knowledge,na.rm=T)
)

# Generating predictions using both models
agg_pred[,c("m1_fit", "m1_lwr", "m1_upr")] <- 
  predict(m1_agg_usa, agg_pred, interval = "confidence")

agg_pred[,c("m2_fit", "m2_lwr", "m2_upr")] <- 
  predict(m2_agg_know_usa, agg_pred, interval = "confidence")

# Long format to facet_wrap more easily 
agg_pred %<>% 
  pivot_longer(cols = m1_fit:m2_upr, 
               names_to = c("model", ".value"),
               names_sep = "_") %>% 
  mutate(model = recode(model, "m1" = "Without democratic knowledge",
                        "m2" = "With democratic knowledge") %>% 
           factor(levels = c("Without democratic knowledge",
                             "With democratic knowledge")))

# Plotting predicted values 
ggplot(agg_pred,
       aes(x = generation, y = fit, ymin = lwr, ymax = upr)) +
  geom_point(size = 2.5) +
  geom_linerange(size = 1) +
  theme_bw(base_family = "Fira Sans",
           base_size = 13) +
  facet_wrap(~model) +
  scale_y_continuous(limits = c(0, 0.6)) +
  labs(x = "Birth cohort",
       y = "Predicted support for executive aggrandizement",
       caption = "Data from original 2020 survey (United States)")

ggsave("2020 surveys/figures/agg_models_usa.png", height = 7, width = 10)

#---------------------------------------------------
# Canada
# First model: without democratic knowledge
m1_agg_can <- lm(
  formula = update.formula(agg_formula, . ~ . + province),
  data = survey_can
)

# Second model: adding democratic knowledge
m2_agg_know_can <- lm(
  formula = update.formula(agg_formula, . ~ . + province + dem_knowledge),
  data = survey_can
)

# Data to predict with
agg_pred_can <- expand.grid(
  generation = c("Pre-1950s", "1950s", "1960s", 
                 "1970s", "1980s", "Post-1980s"),
  partyid = "Liberal",
  gender = "Female",
  race = "White",
  unemployed = 0,
  deprivation_scale = mean(survey_can$deprivation_scale,na.rm=T),
  educ_cat = "High school graduate",
  province = "Ontario",
  dem_knowledge = mean(survey_can$dem_knowledge,na.rm=T)
)

# Generating predictions using both models
agg_pred_can[,c("m1_fit", "m1_lwr", "m1_upr")] <- 
  predict(m1_agg_can, agg_pred_can, interval = "confidence")

agg_pred_can[,c("m2_fit", "m2_lwr", "m2_upr")] <- 
  predict(m2_agg_know_can, agg_pred_can, interval = "confidence")

# Long format to facet_wrap more easily 
agg_pred_can %<>% 
  pivot_longer(cols = m1_fit:m2_upr, 
               names_to = c("model", ".value"),
               names_sep = "_") %>% 
  mutate(model = recode(model, "m1" = "Without democratic knowledge",
                        "m2" = "With democratic knowledge") %>% 
           factor(levels = c("Without democratic knowledge",
                             "With democratic knowledge")))

# Plotting predicted values 
ggplot(agg_pred_can,
       aes(x = generation, y = fit, ymin = lwr, ymax = upr)) +
  geom_point(size = 2.5) +
  geom_linerange(size = 1) +
  theme_bw(base_family = "Fira Sans",
           base_size = 13) +
  facet_wrap(~model) +
  scale_y_continuous(limits = c(0, 0.6)) +
  labs(x = "Birth cohort",
       y = "Predicted support for executive aggrandizement",
       caption = "Data from original 2020 survey (Canada)")

ggsave("2020 surveys/figures/agg_model_can.png", height = 7, width = 10)

#------------------------------------------------
# Marginal effects of cohorts, USA, model 1
m1_usa_me <- margins(m1_agg_usa,
                     variables = "generation") %>% summary()

# Manipulating: reordering generations, adding model type + country
m1_usa_me %<>% 
  select(factor, AME, lower, upper) %>% 
  mutate(factor = str_remove(factor, "generation")) %>% 
  add_row(factor = "Pre-1950s",AME=0,lower=0,upper=0) %>% 
  mutate(factor = factor(factor, levels = gens)) %>% 
  mutate(country = "United States",
         model = "Without democratic knowledge")

# Marginal effects of cohorts, USA, model 2
m2_usa_me <- margins(m2_agg_know_usa,
                     variables = "generation") %>% summary()

# Manipulating: reordering generations, adding model type + country
m2_usa_me %<>% 
  select(factor, AME, lower, upper) %>% 
  mutate(factor = str_remove(factor, "generation")) %>% 
  add_row(factor = "Pre-1950s",AME=0,lower=0,upper=0) %>% 
  mutate(factor = factor(factor, levels = gens)) %>% 
  mutate(country = "United States",
         model = "With democratic knowledge")

me_usa <- bind_rows(m1_usa_me, m2_usa_me) %>% 
  mutate(model = factor(model, levels = c("Without democratic knowledge",
                                          "With democratic knowledge")))

ggplot(me_usa, 
       aes(x = factor, y = AME, ymin = lower, ymax = upper)) +
  geom_point(size = 2.5) +
  geom_linerange(size = 1) +
  facet_wrap(~model) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Cohort marginal effects",
       x = "Cohort") +
  theme_bw(base_family = "Fira Sans",
           base_size = 16)

ggsave("2020 surveys/figures/agg_models_usa_me.png", height= 7, width = 10)

# Do those who are "democratic" according to the aggrandizement scale 
# nonetheless support a strong leader
survey_pooled %>%
  mutate(aggrandize_scale_quart = quantcut(aggrandize_scale, q = 4) %>% 
           recode("[0,0.208]" = "1st quartile", 
                  "(0.208,0.375]" = "2nd quartile",
                  "(0.375,0.521]" = "3rd quartile", 
                  "(0.521,1]" = "4th quartile"),
         sl_dummy = ifelse(strong_leader %in% c(0.66, 1), 1, 0)) %>% 
  group_by(aggrandize_scale_quart, generation, Country) %>% 
  summarise(mean = mean(sl_dummy, na.rm = TRUE),
            lwr = lwr_conf(sl_dummy),
            upr = upr_conf(sl_dummy)) %>% 
  mutate(generation = factor(generation, 
                             levels = c("Pre-1950s", "1950s", "1960s",
                                        "1970s", "1980s", "Post-1980s"))) %>% 
  ggplot(aes(x = aggrandize_scale_quart, y = mean, 
             ymin = lwr, ymax = upr, col = generation)) +
  geom_point(position = position_dodge(0.5)) +
  geom_pointrange(position = position_dodge(0.5)) +
  scale_y_continuous(breaks = seq(0.1, 0.9, 0.1)) +
  scale_color_manual(values = gen_cols,
                     name = "Birth cohort") +
  theme_bw(base_family = "Fira Sans",
           base_size = 16) +
  facet_wrap(~Country) +
  labs(y = "Proportion endorsing strong leader",
       x = "Support for executive aggrandizement",
       caption = "Data from original 2020 survey")

ggsave("2020 surveys/figures/strong~agg+country+gen.png", height = 7, width = 11)
