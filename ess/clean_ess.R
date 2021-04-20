library(pacman)
pacman::p_load(tidyverse,
               data.table,
               magrittr,
               haven,
               countrycode)

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
  col_select = c(idno, cntry, agea, yrbrn, implvdm, stfdem, dmcntov, pspwght)
) %>% 
  rename(respid = idno,
         country = cntry,
         age = agea,
         birth_year = yrbrn,
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
         age = ifelse(age > 114, NA, age), # NA for 123 and 999 values
  )

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
# Recoding wave 6
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
    cohort = case_when(
      birth_year >= 1909 & birth_year <= 1949 ~ "Pre-1950s",
      birth_year >= 1950 & birth_year <= 1959 ~ "1950s",
      birth_year >= 1960 & birth_year <= 1969 ~ "1960s",
      birth_year >= 1970 & birth_year <= 1979 ~ "1970s",
      birth_year >= 1980 & birth_year <= 1989 ~ "1980s",
      birth_year >= 1990 & birth_year <= 2002 ~ "Post-1980s"
    ) %>% 
      factor(levels = c("Pre-1950s", "1950s", "1960s",
                        "1970s", "1980s", "Post-1980s")),
    dem_important_0 = ifelse(dem_important==0, 1, 0),
    dem_important_10 = ifelse(dem_important==10, 1, 0),
    country_name = countrycode(country, "iso2c", "country.name")
  )

#------------------------------------------------------
# Saving data
save(ess_cum, ess6, file = "cleaned_data/ess_clean.RData")
