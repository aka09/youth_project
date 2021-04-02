library(pacman)
pacman::p_load(tidyverse,
               data.table,
               magrittr,
               haven)

#---------------------------------------------------------
# Loading ESS cumulative file 
# fread much faster to read large datasets; selecting relevant columns 
ess_cum <- fread("ess/data/ess_cum.csv",
             select = c("idno", "pspwght", "essround", "cntry", 
                        "stfdem", "prtyban", "yrbrn", "agea")) %>% 
  rename(respid = idno,
         country = cntry,
         dem_satisfied = stfdem,
         ban_party = prtyban,
         birth_year = yrbrn,
         age = agea)

# Loading ESS wave 6
ess6 <- read_dta(
  "ess/data/ess_wave6.dta",
  col_select = c(idno, agea, implvdm, stfdem, dmcntov, pspwght)
) %>% 
  rename(respid = idno,
         age = agea,
         dem_important = implvdm,
         dem_satisfied = stfdem,
         dem_country = dmcntov)

#---------------------------------------------------
# Recoding
#---------------------------------------------------
# Coding extreme values of democratic satisfaction Q
ess_cum %<>% 
  mutate(dem.satisfied.0 = ifelse(dem_satisfied==0, 1, 0),
         dem.satisfied.10 = ifelse(dem_satisfied==10, 1, 0),
         age = ifelse(age > 114, NA, age)) # NA for 123 and 999 values

# Coding generations
ess_cum %<>%
  mutate(age_group = case_when(
    age >= 18 & age < 25 ~ "18-24",
    age >= 25 & age < 35 ~ "25-34",
    age >= 35 & age < 45 ~ "35-44",
    age >= 45 & age < 54 ~ "45-54",
    age >= 55 & age < 65 ~ "55-64",
    age >= 65 ~ "65+",
  ))

#------------------------------------------------------
# Saving cumulative data
save(ess_cum, ess6, file = "cleaned_data/ess_clean.RData")

