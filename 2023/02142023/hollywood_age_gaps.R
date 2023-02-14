# TidyTuesday | February 14, 2023 | Hollywood Age Gap
# Data Source is Data Is Plural

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(tidytext)

# add fonts ---------------------------------------------------------------
font_add_google(name = "News Cycle", family = "News Cycle")
font <- "News Cycle"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 7)
age_gaps <- tuesdata$age_gaps

# wrangle data  -----------------------------------------------------------
df <- age_gaps %>% 
  mutate(decade = (release_year - release_year %% 10)) %>% 
  filter(decade %in% c("1980", "1990", "2000", "2010")) %>%
  filter(age_difference >= 10) %>% 
  group_by(actor_1_name, decade) %>%
  summarise(count = n()) %>%
  arrange(decade, desc(count)) %>%
  group_by(decade) %>%
  top_n(n = 3, wt = count)

df$decade <- factor(df$decade, labels = c("1980s", "1990s", "2000s", "2010s"))

# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(y = reorder_within(actor_1_name, count, decade), x = count)) +
  geom_col(fill = "#000000") +
  facet_wrap(~ decade, scales = "free") +
  scale_y_reordered() +
  theme_minimal() +
  theme(plot.title = element_text(family = font, size = 19, hjust = 0, face = "bold", color = "#000000"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 11, hjust = 0, color = "#000000"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 7, family = font, color = "#000000", hjust = 0),
        axis.title.x = element_text(size = 8, family = font, color = "#000000"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8, family = font, color = "#000000"),
        axis.line.x = element_line(linewidth = 0.4, color = "#ebebeb"),
        axis.ticks.x = element_line(linewidth = 0.4, color = "#ebebeb"),
        strip.text = element_text(size = 8, family = font, color = "#000000"),
        panel.grid = element_blank(),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "Age Gaps of Love Interests in the Movies",
       subtitle = "List of the top 3 actors (and ties) cast in roles who are more than ten years older than\ntheir on-screen love interests in each decade.\n",
       caption = "\n\n\n\n#TidyTuesday | Data: Hollywood Age Gap via Data Is Plural | Design: Ryan Hart",
       x = "\nCount of on-screen love interests (>10 year age gap)")

# save plot ---------------------------------------------------------------
ggsave(paste0("hollywood_age_gaps_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


