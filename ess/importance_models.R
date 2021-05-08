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

m1 <- lm(
  dem_important ~ cohort*dem_satisfied + country_name,
  data = ess6, country_name %in% country_list
)

interplot::interplot(m1, "dem_satisfied", "cohort")
