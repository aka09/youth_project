library(pacman)
pacman::p_load(tidyverse,
               haven,
               magrittr,
               labelled)

# Importing data, renaming columns
evs5 <- read_dta("evs/data/evs_w5.dta",
                 col_select = c(id_cocas, country, v145, v147, v148)) %>% 
  rename(respid = id_cocas, 
         sys1_strongleader = v145,
         sys2_army = v147,
         sys3_democracy = v148)

# coding NAs for incorrect values + reverse-coding democracy item
# Reverse-coding items on regimes: 
# higher values = more democratic; lower values = more autocratic 
evs5 %<>% 
  mutate(across(sys1_strongleader:sys3_democracy,
                ~case_when(. %in% 1:4 ~ .))) %>% 
  mutate(sys3_democracy = recode(sys3_democracy, 
                                 `1` = 4, `2` = 3,
                                 `3` = 2, `4` = 1))

save(evs5, file = "cleaned_data/evs_w5.RData")

