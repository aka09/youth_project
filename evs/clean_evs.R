library(pacman)
pacman::p_load(tidyverse,
               haven,
               magrittr,
               labelled,
               countrycode)

# Importing data, renaming columns
evs5 <- read_dta("evs/data/evs_w5.dta",
                 col_select = c(id_cocas, country, v226, v142, v145, v147, v148)) %>% 
  rename(respid = id_cocas, 
         birth_year = v226,
         dem_important = v142,
         sys1_strongleader = v145,
         sys2_army = v147,
         sys3_democracy = v148)

# coding NAs for incorrect values + reverse-coding democracy item
# Reverse-coding items on regimes: 
# higher values = more democratic; lower values = more autocratic 
evs5 %<>% 
  mutate(
    across(sys1_strongleader:sys3_democracy,
           ~case_when(. %in% 1:4 ~ .)),
    dem_important = case_when(dem_important %in% 1:10 ~ dem_important)
  ) %>% 
  mutate(sys3_democracy = recode(sys3_democracy, 
                                 `1` = 4, `2` = 3,
                                 `3` = 2, `4` = 1))

# Coding incorrect birth years + generations
evs5 %<>% 
  mutate(birth_year = ifelse(!(birth_year %in% 1937:2002), NA, birth_year),
         generation = case_when(
           birth_year >= 1937 & birth_year <= 1949 ~ "Pre-1950s",
           birth_year >= 1950 & birth_year <= 1959 ~ "1950s",
           birth_year >= 1960 & birth_year <= 1969 ~ "1960s",
           birth_year >= 1970 & birth_year <= 1979 ~ "1970s",
           birth_year >= 1980 & birth_year <= 1989 ~ "1980s",
           birth_year >= 1990 & birth_year <= 2002 ~ "Post-1980s",
         ))

save(evs5, file = "cleaned_data/evs_w5.RData")

