library(tidyverse)
library(ggplot2)
library(cregg)
library(gtools)
library(viridis)
library(extrafont)

# Loading our data from 2019
load("cleaned_data/survey_2019_pooled.RData")

# Custom color scale for results by generation
gen_cols <- c(viridis(5)[1:4], "#D53992")

# Importance, by age
survey_pooled %>% 
  filter(age <= 75) %>% 
  group_by(age, Country) %>% 
  summarise(mean = mean(democracy_imp, na.rm = T),
            lwr = lwr_conf(democracy_imp),
            upr = upr_conf(democracy_imp),
            n = n()) %>% 
  ggplot(aes(x = age, y = mean, 
             ymin = lwr, ymax = upr, size = n)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~Country) +
  geom_smooth(aes(weight = n), linetype = "dashed", col = "red") +
  guides(size = FALSE) +
  labs(y = "Importance of democracy (0-10)",
       x = "Age",
       caption = "Data from original 2019 survey; only ages 18-75") +
  scale_x_continuous(breaks = seq(20, 70, 10)) +
  theme_minimal(base_family = "Fira Sans")

ggsave("2019 surveys/figures/imp~age.png", height = 7, width = 9)

#---------------------------------------------------
# Does lack of democratic knowledge explain support for a strong leader?
# Model interacts generation with democratic knowledge and importance of democracy
m1_df_can <- filter(survey_pooled, Country == "Canada") %>% 
  mutate(sl_dummy=ifelse(strong_leader %in% c("Very good", "Fairly good"),1,0))
m1_can <- lm(
  sl_dummy ~ generation*democracy_imp*dem_knowledge,
  data = m1_df_can
)

m1_pred_can <- expand.grid(
  generation = c("Pre-1950s", "Post-1980s"),
  democracy_imp = 0:10,
  dem_knowledge = quantile(m1_df_can$dem_knowledge, probs = c(0.25, 0.75),
                           na.rm = T)
)

m1_pred_can[,c("fit", "lwr", "upr")] <- 
  predict(m1_can, m1_pred_can, interval = "confidence")

ggplot(data = m1_pred_can,
       aes(x = democracy_imp, y = fit, fill = factor(dem_knowledge),
           col = factor(dem_knowledge))) +
  geom_line() +
  facet_wrap(~generation) +
  labs(y = "Predicted probability of endorsing strong leader",
       x = "Stated importance of democracy",
       caption = "Data from original 2019 survey (Canada)") +
  theme_bw(base_family = "Fira Sans") %+replace%
  theme(legend.position = "bottom")

ggsave("2019 surveys/figures/sl~imp_can.png", height = 7, width = 10)

# Same, in the US
m1_df_usa<- filter(survey_pooled, Country == "United States") %>% 
  mutate(sl_dummy=ifelse(strong_leader %in% c("Very good", "Fairly good"),1,0))
m1_usa <- lm(
  sl_dummy ~ generation*democracy_imp*dem_knowledge,
  data = m1_df_usa
)

m1_pred_usa <- expand.grid(
  generation = c("Pre-1950s", "Post-1980s"),
  democracy_imp = 0:10,
  dem_knowledge = quantile(m1_df_usa$dem_knowledge, probs = c(0.25, 0.75),
                           na.rm = T)
)

m1_pred_usa[,c("fit", "lwr", "upr")] <- 
  predict(m1_usa, m1_pred_usa, interval = "confidence") %>% 

ggplot(data = m1_pred_usa,
       aes(x = democracy_imp, y = fit, fill = factor(dem_knowledge),
           col = factor(dem_knowledge))) +
  geom_line() +
  facet_wrap(~generation) +
  labs(y = "Predicted probability of endorsing strong leader",
       x = "Stated importance of democracy",
       caption = "Data from original 2019 survey (USA)") +
  theme_bw(base_family = "Fira Sans") %+replace%
  theme(legend.position = "bottom")

ggsave("2019 surveys/figures/sl~imp_usa.png", height = 7, width = 10)
