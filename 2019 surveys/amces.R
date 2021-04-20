library(tidyverse)
library(ggplot2)
library(cregg)
library(gtools)
library(viridis)
library(extrafont)

f1 <- selected ~ Age + `Political Experience` + Sex + `Legislative checks` + 
`Judicial checks` + `Welfare spending` + Abortion

# Loading our data from 2019
load("cleaned_data/conjoint_2019_pooled.RData")

# Custom color scale for results by generation
gen_cols <- c(viridis(5)[1:4], "#D53992")

# AMCEs by generation and country
conjoint_pooled %>% 
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
    level %in% c("Shut down legislature","Not be bound by courts")
  ) %>% 
  # Extracting 3 subgroup variables from concatenations
  mutate(
    Country = str_split(subgroups, "_") %>% lapply("[", 1) %>% unlist(),
    Generation = str_split(subgroups, "_") %>% 
      lapply("[", 2) %>% 
      unlist() %>% 
      factor(levels = c("Pre-1950s", "1950s", "1960s",
                        "1970s", "1980s", "Post-1980s"))
  ) %>% 
  ggplot(aes(x = Generation, y = estimate, 
             ymin = lower, ymax = upper, color = level)) +
  geom_point(position = position_dodge(0.5)) +
  geom_pointrange(position = position_dodge(0.5)) +
  facet_wrap(~Country) +
  labs(y = "Estimated AMCE") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(name = "Antidemocratic attribute",
                     values = viridis(6)[c(1,4)]) +
  theme_bw(base_family = "Fira Sans") %+replace%
  theme(legend.position = "bottom")

ggsave("2019 surveys/figures/amces~gen.png", height = 6, width = 9)

# AMCEs by generation, democratic knowledge, and country
gen_dem <- conjoint_pooled %>% 
  filter(!is.na(dem_knowledge) & !is.na(generation)) %>% 
  mutate(
    # Collapsing two oldest generations for stat power 
    generation_collapse = recode(
      generation,"Pre-1950s" = "Pre-1960s", "1950s" = "Pre-1960s"),
    # Dem knowledge quartiles based on pooled data
    dem_knowledge = quantcut(dem_knowledge, q = 2) %>% 
      as.character() %>% 
      recode("[-1.62,0.875]" = "Low dem. know.", 
             "(0.875,2]" = "High dem. know."),
    # Variable that contains all three subgroup variables
    subgroups = paste(Country, dem_knowledge, generation_collapse, sep = "_")
  ) %>% 
  # Running conjoint analysis
  cj(
    formula = f1,
    id = ~id,
    by = ~subgroups
  ) %>% 
  # Only the attributes we're intersted in
  filter(
    level %in% c("Shut down legislature","Not be bound by courts")
  ) %>% 
  # Extracting 3 subgroup variables from concatenations
  mutate(
    Country = str_split(subgroups, "_") %>% lapply("[", 1) %>% unlist(),
    `Democratic knowledge` = str_split(subgroups, "_") %>% 
      lapply("[", 2) %>% 
      unlist() %>% 
      factor(levels = c("Low dem. know.", "High dem. know.")),
    Generation = str_split(subgroups, "_") %>% 
      lapply("[", 3) %>% 
      unlist() %>% 
      factor(levels = c("Pre-1960s", "1960s",
                        "1970s", "1980s", "Post-1980s"))
  )

# I add AMCEs by generation and country to the same df
gen_overall <- conjoint_pooled %>% 
  filter(!is.na(generation)) %>% 
  mutate(
    # Collapsing two oldest generations for stat power 
    generation_collapse = recode(
      generation,"Pre-1950s" = "Pre-1960s", "1950s" = "Pre-1960s"),
    # Variable that contains all three subgroup variables
    subgroups = paste(Country, generation_collapse, sep = "_")
  ) %>% 
  # Running conjoint analysis
  cj(
    formula = f1,
    id = ~id,
    by = ~subgroups
  ) %>% 
  # Only the attributes we're intersted in
  filter(
    level %in% c("Shut down legislature","Not be bound by courts")
  ) %>% 
  # Extracting 3 subgroup variables from concatenations
  mutate(
    Country = str_split(subgroups, "_") %>% lapply("[", 1) %>% unlist(),
    Generation = str_split(subgroups, "_") %>% 
      lapply("[", 2) %>% 
      unlist() %>% 
      factor(levels = c("Pre-1960s", "1960s",
                        "1970s", "1980s", "Post-1980s")),
    `Democratic knowledge` = "Overall"
  )

# Merging the two and reordering to have overall at the left 
gen_dem <- rbind(gen_dem, gen_overall) %>% 
  mutate(`Democratic knowledge` = factor(`Democratic knowledge`,
                                         levels = c("Overall",
                                                    "Low dem. know.",
                                                    "High dem. know.")))

ggplot(data = gen_dem,
       aes(x = `Democratic knowledge`, y = estimate, 
           ymin = lower, ymax = upper, col = Generation)) +
  geom_point(position = position_dodge(0.5)) +
  geom_pointrange(position = position_dodge(0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(cols = vars(Country),
             rows = vars(level)) +
  theme_minimal(base_family = "Fira Sans") +
  scale_color_manual(values = gen_cols) +
  labs(y = "Estimated AMCE")

ggsave("2019 surveys/figures/amces~gen+dem.png", height = 7, width = 11)    


