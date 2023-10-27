# #TidyTuesday | 2023-10-24 | Patient Risk Profiles
# Data Source comes Jenna Reps

# libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(janitor)
library(ggforce)
library(ggtext)
library(scales)

# add font ----------------------------------------------------------------
font_add_google(name = "Roboto Serif", family = "Roboto Serif")
font <- "Roboto Serif"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999) 

# load data --------------------------------------------------------------
patient_risk_profiles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-24/patient_risk_profiles.csv')

# wrangle data and create df ----------------------------------------------
df <- patient_risk_profiles %>% 
  clean_names() %>% 
  select(2:20, predicted_risk_of_dementia) %>%
  pivot_longer(cols = -predicted_risk_of_dementia, names_to = "age", values_to = "value") %>%
  filter(value == 1) %>%
  group_by(age) %>%
  mutate(age =  gsub('age_group_', '', age)) %>%
  mutate(age = case_when(age == "0_4" ~ "00_04",
                         age == "5_9" ~ "05_09",
                         TRUE ~ age)) %>%
  mutate(count = n()) %>% 
  mutate(mean = mean(predicted_risk_of_dementia)) %>% 
  # summarize(mean = mean(predicted_risk_of_dementia)) %>% 
  mutate(age_label = gsub('_', ' to ', age)) %>%
  ungroup() %>% 
  select(age, age_label, mean, count) %>% 
  arrange(age) %>% 
  unique() %>%
  mutate(id = row_number()) %>% 
  filter(id >= 11) %>% 
  ungroup()


# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  geom_arc(aes(x0 = 0, y0 = 0, r = 1, start = 0, end = 2 * pi * (1 - mean)), size = 2, color = "#000000") +
  geom_arc(aes(x0 = 0, y0 = 0, r = 1.08, start = 0 * pi * mean, end = 2 * pi * -mean), size = 3, color = "#dc372c") +
  geom_text(aes(x = 0, y = 0, label = percent(mean)), family = font, fontface = "bold", size = 3, hjust = 0.5, color = "#dc372c") +
  facet_wrap(~ age_label) +
  coord_equal(clip = "off") +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 28, hjust = 0.5, face = "bold", color = "#000000"),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(family = font, size = 9, hjust = 0.5, color = "#000000", margin = margin(t = 5, b = 25), lineheight = 1.4),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 8, family = font, color = "#000000", hjust = 0.5, margin = margin(t = 25)),
        strip.text = element_text(size = 8, family = font, color = "#000000", hjust = 0.5, margin = margin(b = 5)),
        panel.spacing = unit(1.25, "lines"),
        legend.position = "none",
        plot.margin = unit(c(0.75, 0, 0.75, 0), "cm"),
        plot.background = element_rect(fill = "#FFFFFF", color = NA)) +
  labs(title = "DEMENTIA",
       subtitle = "The average <span style='color: #dc372c;'><b>1-year predictability of dementia</b></span> by age group for a simulation<br>of patients 50 years of age and older based on their individual medical history",
       caption = "#TidyTuesday | Data: Prepared by Jenna Reps | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("dementia_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


