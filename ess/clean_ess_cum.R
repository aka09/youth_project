library(pacman)
pacman::p_load(tidyverse,
               data.table)

# fread much faster to read large datasets; selecting relevant columns 
ess <- fread("ess/data/ess_cum.csv",
             select = c("idno", "pspwght", "essround", "cntry", 
                        "stfdem", "prtyban"))

ess <- haven::read_dta(unz("ess/data/ess_cum.zip", "ess_cum.dta"),
                col_select = c(essround, pspwght,cntry,idno),
                n_max = 1)

ess <- read_csv("ess/data/ess_cum.csv")

ess <- haven::read_dta("ess/data/ess_cum.dta",
                       .name_repair = "minimal",
                       col_select = contains("no"),
                       n_max = 10)
