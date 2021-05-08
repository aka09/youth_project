library(tidyverse)
library(modelsummary)
library(ggplot2)
library(magrittr)

load("cleaned_data/survey_2018.RData")

# Model with support for executive aggrandizement as DV
m1_agg <- lm(
  aggrandize_scale ~ cohort + partyid + gender + race + unemployed + 
    deprivation_scale + educ + region,
  data = survey_2018
)

m2_vote <- lm(
  voting_scale ~ cohort + partyid + gender + race + unemployed + 
    deprivation_scale + educ + region,
  data = survey_2018
)

# Renaming and reordering coefficients for table 
cm <- c(`(Intercept)` = "Intercept",
        `cohort1950s` = "1950s cohort",
        `cohort1960s` = "1960s cohort",
        `cohort1970s` = "1970s cohort",
        `cohort1980s` = "1980s cohort",
        `cohortPost-1980s` = "Post-1980s cohort",
        `partyidNot very strong Democrat` = "Not very strong Democrat",
        `partyidIndependent/leaner` = "Independent/leaner",
        `partyidNot very strong Republican` = "Not very strong Republican",
        `partyidStrong Republican` = "Strong Republican",
        `genderMale` = "Male",
        `raceBlack` = "Black",
        `raceOther minority race` = "Other minority race",
        `unemployed` = "Unemployed/laid-off",
        `deprivation_scale` = "Deprivation scale",
        `educHigh school graduate` = "High school graduate",
        `educSome college` = "Some college", 
        `educ2-year college degree` = "2-year college",
        `educ4-year college degree` = "4-year college",
        `educGraduate degree` = "Graduate degree",
        `regionMidwest` = "Midwest",
        `regionSouth` = "South",
        `regionWest` = "West")

# Saving regression table
modelsummary(
  list(m1_agg, m2_vote),
  output = "2018 survey/tables/agg_model.docx",
  coef_map = cm
)

pred <- expand.grid(
  cohort = levels(survey_2018$cohort),
  partyid = "Independent/leaner",
  gender = "Female",
  race = "White",
  unemployed = 0,
  deprivation_scale = mean(survey_2018$deprivation_scale,na.rm=T),
  educ = "High school graduate",
  region = "Northeast"
)

pred[,c("m1_fit", "m1_lwr", "m1_upr")] <- 
  predict(m1_agg, pred, interval = "confidence")

pred[,c("m2_fit", "m2_lwr", "m2_upr")] <- 
  predict(m2_vote, pred, interval = "confidence")

pred %<>% 
  pivot_longer(cols = m1_fit:m2_upr, 
               names_to = c("model", ".value"),
               names_sep = "_") %>% 
  mutate(model = recode(model, "m1" = "Executive aggrandizement",
                        "m2" = "Limiting voting rights"))

ggplot(pred,
       aes(x = cohort, y = fit, ymin = lwr, ymax = upr)) +
  geom_point(size = 2.5) +
  geom_linerange(size = 1) +
  theme_bw(base_family = "Fira Sans",
           base_size = 13) +
  facet_wrap(~model) +
  scale_y_continuous(limits = c(0, 0.5)) +
  labs(x = "Birth cohort",
       y = "Predicted support",
       caption = "Data from original 2018 survey")

ggsave("2018 survey/figures/model_pred.png", height = 7, width = 10)
  

