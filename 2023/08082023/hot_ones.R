# #TidyTuesday | 2023-08-08 | Hot Ones
# Data Source is from Wikipedia articles: Hot Ones and List of Hot Ones episodes. 

# libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(ggtext)
library(scales)

# add font ----------------------------------------------------------------
font_add_google(name = "Rock Salt", family = "Rock Salt")
font_add_google(name = "Lato", family = "Lato")
font_t <- "Rock Salt"
font <- "Lato"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# # load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 32)

episodes <- tuesdata$episodes
sauces <- tuesdata$sauces
seasons <- tuesdata$seasons

# wrangle data and create df ----------------------------------------------
sauces$sauce_name[sauces$sauce_name == "Da' Bomb Beyond Insanity"] <- "Da' Bomb – Beyond Insanity"

df_sauces <- sauces %>% 
  arrange(season) %>% 
  group_by(sauce_number) %>% 
  # mutate(SoS = scoville - lag(scoville)) +
  mutate(sos = (scoville - lag(scoville)) / lag(scoville) * 100) %>% 
  mutate(sos = ifelse(is.na(sos), 0, sos))
  
# create plot -------------------------------------------------------------
df_sauces %>%
  ggplot(aes(y = sos, x = season)) +
  geom_line(aes(group = sauce_number), color = "#DDDDDD") +
  geom_line(data = df_sauces %>% filter(sauce_number == 2), color = "#ff3a22") +
  geom_point(data = df_sauces %>% filter(sauce_number == 2), size = 1.3, color = "#ff3a22") +
  geom_text(aes(x = 10, y = 400, label = "Hot Ones"), family = font_t, size = 9, hjust = 0) +
  geom_richtext(aes(x = 9.75, y = 340, label = "The wing sauces and their heat levels saw the<br>wildest changes in the show's early seasons.<br>Notably, the <span style='color: #ff3a22;'>path for sauce #2</span> swung from an<br>81.7% drop in Scoville scale in season 5 to a<br>scorching 627.3% spike in season 6."), family = font, size = 3, hjust = 0, vjust = "top", fill = NA, label.color = NA, lineheight = 1.3) +
  geom_text(data = df_sauces %>% filter(sauce_number == 2, season == 6), aes(label = paste0(sauce_name, " @ ", scoville, " Scovilles")), family = font, size = 2.25, nudge_x = 0.5, hjust = 0, color = "#ff3a22") +
  geom_text(data = df_sauces %>% filter(sauce_number == 2, season == 5), aes(label = paste0(sauce_name, " @ ", scoville, " Scovilles")), family = font, size = 2.25, nudge_x = 0.5, hjust = 0, color = "#ff3a22") +
  scale_color_identity() +
  scale_y_continuous(limits = c(-100, 650), breaks = c(-100, 0, 100, 200, 300, 400, 500, 600), labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(breaks = c(1:21)) +
  theme_void() +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(size = 8, family = font, color = "#000000", hjust = 0.5, margin = margin(t = 20)),
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.text = element_text(size = 7, family = font, color = "#000000", hjust = 0.5),
        axis.title.y = element_text(size = 8, family = font, color = "#000000", hjust = 0.5, angle = 90, margin = margin(r = 10)),
        axis.title.x = element_text(size = 8, family = font, color = "#000000", hjust = 0.5, margin = margin(t = 10)),
        panel.background = element_rect(color = NA, fill = "#ffffff"),
        plot.background = element_rect(color = NA, fill = "#ffffff")) +
  labs(y = "% Change in Scoville Scale (Season over Season)",
       x = "Seasons",
       caption = "#TidyTuesday | Data: Wikipedia | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("hot_ones_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)




