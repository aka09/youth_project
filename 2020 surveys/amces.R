library(tidyverse)
library(ggplot2)
library(cregg)
library(gtools)
library(viridis)
library(extrafont)

# Loading our data from 2020
load("cleaned_data/conjoint_2020_pooled.RData")

f1 <- selected ~ Gender + Age + `Political experience` + Party +
  `Economic aid policy` + `Lockdown policy` + `Legislative checks`
f2 <- selected ~ Gender + Age + `Political experience` + Party +
  `Economic aid policy` + `Lockdown policy` + `Judicial checks`

# AMCEs by generation and country
leg_amces <- conjoint_legislature_pooled %>% 
  filter(!is.na(generation)) %>% 
  mutate(
    subgroups = paste(Country, generation, sep = "_")
  ) %>% 
  # Running conjoint analysis
  cj(
    formula = f1,
    id = ~id,
    by = ~subgroups
  ) %>% 
  # Only the attributes we're intersted in
  filter(
    level %in% c("Shut down legislature")
  ) %>% 
  # Extracting 2 subgroup variables from concatenations
  mutate(
    Country = str_split(subgroups, "_") %>% lapply("[", 1) %>% unlist(),
    Generation = str_split(subgroups, "_") %>% 
      lapply("[", 2) %>% 
      unlist() %>% 
      factor(levels = c("Pre-1950s", "1950s", "1960s",
                        "1970s", "1980s", "Post-1980s")),
    `Antidemocratic attribute` = "Shut down legislature"
  )

courts_amces <- conjoint_courts_pooled %>% 
  filter(!is.na(generation)) %>% 
  mutate(
    subgroups = paste(Country, generation, sep = "_")
  ) %>% 
  # Running conjoint analysis
  cj(
    formula = f2,
    id = ~id,
    by = ~subgroups
  ) %>% 
  # Only the attributes we're intersted in
  filter(
    level %in% c("Ignore courts")
  ) %>% 
  # Extracting 2 subgroup variables from concatenations
  mutate(
    Country = str_split(subgroups, "_") %>% lapply("[", 1) %>% unlist(),
    Generation = str_split(subgroups, "_") %>% 
      lapply("[", 2) %>% 
      unlist() %>% 
      factor(levels = c("Pre-1950s", "1950s", "1960s",
                        "1970s", "1980s", "Post-1980s")),
    `Antidemocratic attribute` = "Ignore courts"
  )

combined_amces <- rbind(leg_amces, courts_amces)

  ggplot(combined_amces, 
         aes(x = Generation, y = estimate, 
             ymin = lower, ymax = upper, col = `Antidemocratic attribute`)) +
  geom_point(position = position_dodge(0.5)) +
  geom_pointrange(position = position_dodge(0.5)) +
  facet_wrap(~Country) +
  labs(y = "Estimated AMCE") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = viridis(6)[c(1,4)]) +
  theme_bw(base_family = "Fira Sans") %+replace%
  theme(legend.position = "bottom")

ggsave("2020 surveys/figures/amces~gen.png", height = 6, width = 9)
