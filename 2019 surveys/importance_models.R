library(tidyverse)
library(ggplot2)
library(modelsummary)
library(flextable)

# Loading our data from 2019
load("cleaned_data/survey_2019_pooled.RData")

survey_pooled$generation <- 
  factor(survey_pooled$generation,
         levels = c("Pre-1950s", "1950s", "1960s", 
                    "1970s", "1980s", "Post-1980s"))

# Effect of knowledge on importance
m1 <- lm(democracy_imp ~ generation, 
         data = filter(survey_pooled, Country == "Canada"))
m2 <- lm(democracy_imp ~ generation + dem_knowledge, 
         data = filter(survey_pooled, Country == "Canada"))
m3 <- lm(democracy_imp ~ generation, 
         data = filter(survey_pooled, Country == "United States"))
m4 <- lm(democracy_imp ~ generation + dem_knowledge, 
         data = filter(survey_pooled, Country == "United States"))

modelsummary(models = list(m1, m2, m3, m4),
             output = "2019 surveys/tables/imp~know.docx")
