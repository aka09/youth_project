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
gen_cols <- c(viridis(6)[1:5], "#D53992")


# Parliament----
## Lockdown defections----
### Overall----
leg_lockdown_overall <- conjoint_scenario(conjoint_legislature_pooled,
                                  condition = "legislature",
                                  defect_cause = "lockdown")

ggplot(leg_lockdown_overall,
       aes(x = scenario, y = mean, ymin = lwr, ymax = upr, col = Country)) +
  geom_point(position = position_dodge(0.5)) +
  geom_pointrange(position = position_dodge(0.5)) +
  theme_minimal(base_family = "Fira Sans") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(y = "Proportion voting for D- candidate",
       x = "Respondent agrees with lockdown position of...") +
  scale_color_manual(values = c("#ff0000", "#3c3b6e"))

### By generation----
# Only taking the two most extreme scenarios
leg_lockdown_gen <- conjoint_scenario(conjoint_legislature_pooled,
                                      condition = "legislature",
                                      defect_cause = "lockdown",
                                      by = "generation") %>% 
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

## Party defections----
### Overall----
# Only taking respondents from parties that are represented in conjoint 
leg_party_overall <- 
  conjoint_legislature_pooled %>% 
  # recoding so that NDP respondents are matched to NDP profiles/candidates
  mutate(partyid = recode(partyid, "New Democrat" = "NDP")) %>% 
  filter(partyid %in% levels(conjoint_legislature_pooled$Party)) %>% 
  conjoint_scenario(condition = "legislature", defect_cause = "party")

ggplot(leg_party_overall,
       aes(x = scenario, y = mean, ymin = lwr, ymax = upr, col = Country)) +
  geom_point(position = position_dodge(0.5)) +
  geom_pointrange(position = position_dodge(0.5)) +
  theme_minimal(base_family = "Fira Sans") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(y = "Proportion voting for D- candidate",
       x = "Respondent from same party as of...") +
  scale_color_manual(values = c("#ff0000", "#3c3b6e"))

### By generation----
# Only the two most extreme scenarios (only D- and only D+) for simplicity
leg_party_gen <- 
  conjoint_legislature_pooled %>% 
  # recoding so that NDP respondents are matched to NDP profiles/candidates
  mutate(partyid = recode(partyid, "New Democrat" = "NDP")) %>% 
  filter(partyid %in% levels(conjoint_legislature_pooled$Party)) %>% 
  conjoint_scenario(condition = "legislature", 
                    defect_cause = "party",
                    by = "generation") %>% 
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

# Courts----
## Lockdown defections----
### Overall----
courts_lockdown_overall <- conjoint_scenario(conjoint_courts_pooled,
                                     condition = "courts",
                                     defect_cause = "lockdown")

ggplot(courts_lockdown_overall,
       aes(x = scenario, y = mean, ymin = lwr, ymax = upr, col = Country)) +
  geom_point(position = position_dodge(0.5)) +
  geom_pointrange(position = position_dodge(0.5)) +
  theme_minimal(base_family = "Fira Sans") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(y = "Proportion voting for D- candidate",
       x = "Respondent agrees with lockdown position of...") +
  scale_color_manual(values = c("#ff0000", "#3c3b6e"))

### By generation----
# Only taking the two most extreme scenarios
courts_lockdown_gen <- conjoint_scenario(conjoint_courts_pooled,
                                      condition = "courts",
                                      defect_cause = "lockdown",
                                      by = "generation") %>% 
  filter(scenario %in% c("Only D+ candidate", "Only D- candidate"))


ggplot(courts_scenario_gen,
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

## Party defections----
### Overall----
# Only taking respondents from parties that are represented in conjoint 
courts_party_overall <- 
  conjoint_courts_pooled %>% 
  # recoding so that NDP respondents are matched to NDP profiles/candidates
  mutate(partyid = recode(partyid, "New Democrat" = "NDP")) %>% 
  filter(partyid %in% levels(conjoint_courts_pooled$Party)) %>% 
  conjoint_scenario(condition = "courts", defect_cause = "party")

ggplot(courts_party_overall,
       aes(x = scenario, y = mean, ymin = lwr, ymax = upr, col = Country)) +
  geom_point(position = position_dodge(0.5)) +
  geom_pointrange(position = position_dodge(0.5)) +
  theme_minimal(base_family = "Fira Sans") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(y = "Proportion voting for D- candidate",
       x = "Respondent from same party as of...") +
  scale_color_manual(values = c("#ff0000", "#3c3b6e"))

### By generation----
# Only the two most extreme scenarios (only D- and only D+) for simplicity
courts_party_gen <- 
  conjoint_courts_pooled %>% 
  # recoding so that NDP respondents are matched to NDP profiles/candidates
  mutate(partyid = recode(partyid, "New Democrat" = "NDP")) %>% 
  filter(partyid %in% levels(conjoint_courts_pooled$Party)) %>% 
  conjoint_scenario(condition = "courts", 
                    defect_cause = "party",
                    by = "generation") %>% 
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
