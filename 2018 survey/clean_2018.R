library(tidyverse)
library(haven)
library(ggplot2)
library(labelled)

survey_2018 <- read_dta("2018 survey/data/survey_2018.dta") %>% 
  mutate(aggrandize1_veto = Q7b,
         aggrandize2_shutdown = Q25_b,
         aggrandize4_scotus = Q7g,
         voting1_harder = Q7e,
         voting2_native = Q33b,
         deprive1_income = Q18_a,
         deprive2_savings = Q18_b, 
         deprive3_debt = Q18_c) %>% 
  mutate(
    # this question was changed after a soft launch
    # merging the two versions into a single question
    aggrandize3_ignore = ifelse(Q38a != 9, Q38a, Q38a_new),
    # Coding "skipped" and "not asked" as NA + numeric 
    across(
      c(aggrandize1_veto:aggrandize3_ignore), 
      ~as.numeric(.) %>% 
        ifelse(. %in% c(8,9), NA, .))
    ) %>% 
  # recoding variables from 0 to 1
  mutate(
    across(
      c(aggrandize1_veto, aggrandize4_scotus, voting1_harder,
        deprive1_income, deprive2_savings, deprive3_debt),
      ~(.-1)/(7-1)
      ),
    across(
      c(aggrandize2_shutdown, aggrandize3_ignore, voting2_native),
      ~(.-1)/(5-1)
    )
  ) %>% 
  # creating the scales and recoding the RHS variables
  mutate(
    aggrandize_scale = (aggrandize1_veto + aggrandize2_shutdown +
      aggrandize3_ignore + aggrandize4_scotus) / 4,
    voting_scale = (voting1_harder + voting2_native) / 2,
    deprivation_scale = (deprive1_income + deprive2_savings + deprive3_debt)/3,
    gender = recode(Q2, `1` = "Male", `2` = "Female", `3` = "Other") %>% 
      factor(levels = c("Female", "Male", "Other")),
    partyid = recode(pid7, `1` = "Strong Democrat", `2` = "Not very strong Democrat",
                     `3` = "Independent/leaner", `4` = "Independent/leaner",
                     `5` = "Independent/leaner", `6` = "Not very strong Republican",
                     `7` = "Strong Republican") %>% 
      factor(levels = c("Strong Democrat", "Not very strong Democrat",
                        "Independent/leaner", "Not very strong Republican",
                        "Strong Republican")),
    cohort = case_when(
      birthyr < 1950 ~ "Pre-1950s",
      birthyr >= 1950 & birthyr < 1960 ~ "1950s",
      birthyr >= 1960 & birthyr < 1970 ~ "1960s",
      birthyr >= 1970 & birthyr < 1980 ~ "1970s",
      birthyr >= 1980 & birthyr < 1990 ~ "1980s",
      birthyr >= 1990 ~ "Post-1980s"
    ) %>% 
      factor(levels = c("Pre-1950s", "1950s", "1960s", 
                        "1970s", "1980s", "Post-1980s")),
    race = recode(race, `1` = "White", `2` = "Black",
                  `3` = "Other minority race", `4` = "Other minority race",
                  `5` = "Other minority race", `6` = "Other minority race",
                  `7` = "Other minority race", `8` = "Other minority race") %>% 
      factor() %>% relevel(ref = "White"),
    unemployed = ifelse(employ %in% c(3, 4), 1, 0),
    educ = recode(educ, `1` = "Less than high school", 
                  `2` = "High school graduate", `3` = "Some college",
                  `4` = "2-year college degree", `5` = "4-year college degree",
                  `6` = "Graduate degree") %>% 
      factor(levels = c("Less than high school", "High school graduate", 
                        "Some college", "2-year college degree", 
                        "4-year college degree", "Graduate degree")),
    region = recode(region, `1` = "Northeast", `2` = "Midwest",
                    `3` = "South", `4` = "West") %>% 
      factor(levels = c("Northeast", "Midwest", "South", "West"))
  )

# Saving cleaned data
save(survey_2018, 
     file = "cleaned_data/survey_2018.RData")
