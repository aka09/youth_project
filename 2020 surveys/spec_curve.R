library(tidyverse)
library(ggplot2)
library(gtools)
library(viridis)
library(extrafont)
library(specr)
library(magrittr)

# Loading our data from 2020
load("cleaned_data/survey_2020_pooled.RData")
source("2020 surveys/functions/spec_chart.R")

# Recoding factor levels for generation/cohort 
survey_pooled %<>% mutate(
  generation = factor(generation,
                      levels = c("Pre-1950s", "1950s", "1960s", 
                                 "1970s", "1980s", "Post-1980s")),
  age_squared = age^2,
  `Duration quartile` = case_when(
    duration < quantile(survey_pooled$duration, 0.25) ~ "1st quartile",
    duration >= quantile(survey_pooled$duration, 0.25) &
      duration < median(survey_pooled$duration) ~ "2nd quartile",
    duration >= quantile(survey_pooled$duration, 0.5) &
      duration < quantile(survey_pooled$duration,0.75) ~ "3rd quartile",
    duration >= quantile(survey_pooled$duration, 0.75) ~ "4th quartile"
  ) %>% 
    factor(levels = c("1st quartile", "2nd quartile", "3rd quartile",
                      "4th quartile"))
) %>% 
  rename(`Democratic knowledge` = dem_knowledge,
         `Economic deprivation` = deprivation_scale,
         `Party ID` = partyid, 
         `Covid health worry` = worry_health,
         `Covid economic worry` = worry_economic,
         Authoritarianism = authority_scale,
         Gender = gender)

# Running specr analysis using the Canadian data 
specr_results_can <- run_specs(
  df = filter(survey_pooled, Country == "Canada"),
  y = c("importance_democracy"), 
  x = c("age"), 
  model = c("lm"), 
  controls = c("`Democratic knowledge`", "`Economic deprivation`", 
               "`Party ID`",
               "`Covid health worry`", "`Covid economic worry`",
               "Authoritarianism", "Gender"),
  subsets = list(`Duration quartile` = unique(survey_pooled$`Duration quartile`))
)

plot_specs(specr_results_can,
           choices = c("controls", "subsets"))

ggsave("2020 surveys/figures/spec_imp_can.png", height = 7, width = 10)

#-----------------------------------------
# Running specr analysis using the US data 
specr_results_usa <- run_specs(
  df = filter(survey_pooled, Country == "United States"),
  y = c("importance_democracy"), 
  x = c("age"), 
  model = c("lm"), 
  controls = c("`Democratic knowledge`", "`Economic deprivation`", 
               "`Party ID`",
               "`Covid health worry`", "`Covid economic worry`",
               "Authoritarianism", "Gender"),
  subsets = list(`Duration quartile` = unique(survey_pooled$`Duration quartile`))
)

plot_specs(specr_results_usa,
           choices = c("controls", "subsets"))

ggsave("2020 surveys/figures/spec_imp_usa.png", height = 7, width = 10)

# --------------------------------------------
# Analyses using the "strong leader" question
# Running specr analysis using the Canadian data 
specr_strong_can <- run_specs(
  df = filter(survey_pooled, Country == "Canada"),
  y = c("strong_leader"), 
  x = c("age"), 
  model = c("lm"), 
  controls = c("`Democratic knowledge`", "`Economic deprivation`", 
               "`Party ID`",
               "`Covid health worry`", "`Covid economic worry`",
               "Authoritarianism", "Gender"),
  subsets = list(`Duration quartile` = unique(survey_pooled$`Duration quartile`))
)

plot_specs(specr_strong_can,
           choices = c("controls", "subsets"))

ggsave("2020 surveys/figures/spec_sl_can.png", height = 7, width = 10)

#-----------------------------------------
# Running specr analysis using the US data 
specr_strong_usa <- run_specs(
  df = filter(survey_pooled, Country == "United States"),
  y = c("strong_leader"), 
  x = c("age"), 
  model = c("lm"), 
  controls = c("`Democratic knowledge`", "`Economic deprivation`", 
               "`Party ID`",
               "`Covid health worry`", "`Covid economic worry`",
               "Authoritarianism", "Gender"),
  subsets = list(`Duration quartile` = unique(survey_pooled$`Duration quartile`))
)

plot_specs(specr_strong_usa,
           choices = c("controls", "subsets"))

ggsave("2020 surveys/figures/spec_sl_usa.png", height = 7, width = 10)


#----------------------------------------------
# Trying to do things manually
#----------------------------------------------
# All possible combinations of RHS variables
models <- expand.grid(
  generation = "generation",
  deprivation_scale = c("deprivation_scale", "quantcut(deprivation_scale)"),
  dem_knowledge = c("dem_knowledge", "quantcut(dem_knowledge)"),
  educ_cat = c("educ_cat", "education")
)

# All possible RHS variables
var_list <- models %>% 
  pivot_longer(cols = everything()) %>% 
  pull(value) %>% 
  unique()

# Pastes the different combinations into a single formula that can be 
# passed to a regression model 
formulas <- apply(models, 1, paste, collapse = " + ") %>% 
  as.data.frame() %>% 
  rename(formul = 1) %>% 
  mutate(formul = paste("importance_democracy ~", formul))

# Matrix that will store the coefficient on the Post-1980s cohort, as well
# as the associated standard error 
coefs <- matrix(nrow = nrow(formulas), ncol = 2)

for(i in 1:nrow(formulas)){
  coefs[i, 1:2] <- 
    lm(formula = formulas$formul[i], data = survey_pooled) %>% 
    summary() %>% 
    .$coefficients %>% 
    .[6,1:2]
}

coefs <- data.frame(coefs) %>% 
  rename(coefficient = 1,
         se = 2)



ggplot(data = coefs, 
       aes(x = model_nb, y = coefficient, ymin = lwr, ymax = upr)) +
  geom_point() +
  geom_errorbar(width = 0) +
  scale_y_continuous(limits = c(min(coefs$lwr-0.20), 0.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw()

schart(coefs)