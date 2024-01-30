# #TidyTuesday | 2024-01-30 | Groundhog Predictions
# Data source comes from the from groundhog-day.com

library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(ggfx)
library(ggtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Roboto", family = "Roboto")
font <- "Roboto"

font_add_google(name = "Lilita One", family = "Lilita One")
font2 <- "Lilita One"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data --------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2024-01-30')
groundhogs <- tuesdata$groundhogs
predictions <- tuesdata$predictions

# wrangle and format data -------------------------------------------------
df <- predictions %>%
  left_join(groundhogs) %>% 
  filter(region == "Pennsylvania" & year > 2013 & (slug == "punxsutawney-phil" | type == "Person in a groundhog suit")) %>%
  group_by(year, shortname, name) %>%
  filter(all(shadow)) %>%
  distinct(year) %>%
  filter(shortname %in% c("Patty", "Doug")) %>%
  arrange(desc(year)) %>%
  group_by(name) %>%
  mutate(Value = 1 + (row_number() - min(row_number())) %% 5,
         max = max(Value))

# change levels -----------------------------------------------------------
df$name <- factor(df$name, levels = c("Patty Pagoda", "Dover Doug"))

# create plot -------------------------------------------------------------
ggplot(df, aes(x = ifelse(Value == 5, 4.75, Value), y = ifelse(Value == 5, 0.9, 1))) +
  with_shadow(geom_segment(aes(xend = ifelse(Value == 5, 0.25, Value), yend = ifelse(Value == 5, 0.1, 0)), color = "#000000", linewidth = 3, lineend = "round"), sigma = 10, x_offset = 35, y_offset = 35, colour = "#999999") +
  geom_text(aes(x = 2.5, y = - 0.15, label = paste0(max, " out of 10 years")), family = font, size = 3.5, fontface = "bold") +
  facet_wrap(~ name) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.title = element_text(family = font2, hjust = 0, size = 26, color = "#000000", margin = margin(b = 10)),
        plot.subtitle = element_textbox_simple(family = font, hjust = 0, size = 8.75, color = "#000000", margin = margin(b = 55), lineheight = 1.5),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0.5, size = 8, color = "#000000", margin = margin(t = 55)),
        plot.caption.position = "plot",legend.position = "none",
        strip.text = element_text(family = font2, size = 18, color = "#000000", margin = margin(b = 10)),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "GROUNDHOG IMITATORS",
       subtitle = "Two Pennsylvanians sporting furry groundhog costumes challenge the legendary prognosticator and groundhog, Punxsutawney Phil, in the winter-ending prediction game. <b>From 2014 to 2023, how often did Phil and the imitators call for six more weeks of winter?",
       caption = "#TidyTuesday | Data: groundhog-day.com | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("groundhog_imitators_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

