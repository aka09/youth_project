# Packages
library(tidyverse)
library(ggplot2)
library(DescTools)
library(extrafont)
library(viridis)

# Conjoint data (2020) from both Canada and US 
load("cleaned_data/conjoint_2020_pooled.RData")

# Loading function that produces scenario results
source("2020 surveys/functions/conjoint_scenario.R")

# Custom color scale for results by generation
gen_cols <- c(viridis(6)[c(1,3)], "#D53992")

# Recoding to have three generations instead
conjoint_courts_pooled$generation3 <- 
  dplyr::recode(conjoint_courts_pooled$generation,
                "Pre-1950s" = "Pre-1960s",
                "1950s" = "Pre-1960s",
                "1960s" = "1960s-70s",
                "1970s" = "1960s-70s",
                "1980s" = "1980s-up",
                "Post-1980s" = "1980s-up")
conjoint_legislature_pooled$generation3 <- 
  dplyr::recode(conjoint_legislature_pooled$generation,
                "Pre-1950s" = "Pre-1960s",
                "1950s" = "Pre-1960s",
                "1960s" = "1960s-70s",
                "1970s" = "1960s-70s",
                "1980s" = "1980s-up",
                "Post-1980s" = "1980s-up")

# Legislature----
## Lockdown defections by generation----
# Only taking the two most extreme scenarios
leg_lockdown_gen <- conjoint_scenario(conjoint_legislature_pooled,
                                      condition = "legislature",
                                      defect_cause = "lockdown",
                                      by = "generation3") %>% 
  filter(scenario %in% c("Only D+ candidate", "Only D- candidate"))


ggplot(leg_lockdown_gen,
       aes(x = scenario, y = mean, ymin = lwr, ymax = upr, col = by)) +
  geom_point(position = position_dodge(0.5)) +
  geom_pointrange(position = position_dodge(0.5)) +
  theme_minimal(base_family = "Fira Sans") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_color_manual(name = "Generation",
                     values = gen_cols) +
  scale_y_continuous(breaks = seq(0.2, 0.8, 0.1)) +
  labs(y = "Proportion voting for D- candidate",
       x = "Respondent agrees with lockdown position of...") +
  facet_wrap(~Country)

ggsave("2020 surveys/figures/scenario_lockdown (legislature).png", height = 7, width = 8)

## Party defections----
# Only the two most extreme scenarios (only D- and only D+) for simplicity
leg_party_gen <- 
  conjoint_legislature_pooled %>% 
  # recoding so that NDP respondents are matched to NDP profiles/candidates
  mutate(partyid = recode(partyid, "New Democrat" = "NDP")) %>% 
  filter(partyid %in% levels(conjoint_legislature_pooled$Party)) %>% 
  conjoint_scenario(condition = "legislature", 
                    defect_cause = "party",
                    by = "generation3") %>% 
  filter(scenario %in% c("Only D+ candidate", "Only D- candidate"))

ggplot(leg_party_gen,
       aes(x = scenario, y = mean, ymin = lwr, ymax = upr, col = by)) +
  geom_point(position = position_dodge(0.5)) +
  geom_pointrange(position = position_dodge(0.5)) +
  theme_minimal(base_family = "Fira Sans") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(y = "Proportion voting for D- candidate",
       x = "Respondent from same party as of...") +
  scale_color_manual(values = gen_cols) +
  scale_y_continuous(breaks = seq(0.1, 0.8, 0.1)) +
  facet_wrap(~Country)

ggsave("2020 surveys/figures/scenario_party (legislature).png", height = 7, width = 8)


# Courts----
## Lockdown defections----
# Only taking the two most extreme scenarios
courts_lockdown_gen <- conjoint_scenario(conjoint_courts_pooled,
                                      condition = "courts",
                                      defect_cause = "lockdown",
                                      by = "generation3") %>% 
  filter(scenario %in% c("Only D+ candidate", "Only D- candidate"))


ggplot(courts_lockdown_gen,
       aes(x = scenario, y = mean, ymin = lwr, ymax = upr, col = by)) +
  geom_point(position = position_dodge(0.5)) +
  geom_pointrange(position = position_dodge(0.5)) +
  theme_minimal(base_family = "Fira Sans") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_color_manual(name = "Generation",
                     values = gen_cols) +
  scale_y_continuous(breaks = seq(0.1, 0.8, 0.1)) +
  labs(y = "Proportion voting for D- candidate",
       x = "Respondent agrees with lockdown position of...") +
  facet_wrap(~Country)

ggsave("2020 surveys/figures/scenario_lockdown (courts).png", height = 7, width = 8)

## Party defections----
# Only the two most extreme scenarios (only D- and only D+) for simplicity
courts_party_gen <- 
  conjoint_courts_pooled %>% 
  # recoding so that NDP respondents are matched to NDP profiles/candidates
  mutate(partyid = recode(partyid, "New Democrat" = "NDP")) %>% 
  filter(partyid %in% levels(conjoint_courts_pooled$Party)) %>% 
  conjoint_scenario(condition = "courts", 
                    defect_cause = "party",
                    by = "generation3") %>% 
  filter(scenario %in% c("Only D+ candidate", "Only D- candidate"))

ggplot(courts_party_gen,
       aes(x = scenario, y = mean, ymin = lwr, ymax = upr, col = by)) +
  geom_point(position = position_dodge(0.5)) +
  geom_pointrange(position = position_dodge(0.5)) +
  theme_minimal(base_family = "Fira Sans") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(y = "Proportion voting for D- candidate",
       x = "Respondent from same party as of...") +
  scale_color_manual(values = gen_cols) +
  scale_y_continuous(breaks = seq(0.1, 0.8, 0.1)) +
  facet_wrap(~Country)

ggsave("2020 surveys/figures/scenario_party (courts).png", height = 7, width = 8)
