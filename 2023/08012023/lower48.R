# #TidyTuesday | 2023-08-01 | US States
# Data Source is Wikipedia

# libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(maps)
library(sf)
library(ggfx)

# add font ----------------------------------------------------------------
font_add_google(name = "Bitter", family = "Bitter")
font_add_google(name = "Lato", family = "Lato")
font_t <- "Bitter"
font <- "Lato"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# # load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 31)

states <- tuesdata$states
state_name_etymology <- tuesdata$state_name_etymology

# wrangle data and create df ----------------------------------------------
df <- states %>% 
  filter(!state %in% c("Hawaii", "Alaska")) %>% 
  arrange(desc(admission)) %>% 
  slice(1) %>% 
  mutate(admission = ymd(admission))
  
# create plot -------------------------------------------------------------
ggplot() + 
  as_group(
    geom_polygon(data = map_data("state"), aes(x = long - 0.2, y = lat - 0.2, group = group), linewidth = 0.4, color = "#454545", fill = NA),
    geom_polygon(data = map_data("state"), aes(x = long - 0.175, y = lat - 0.175, group = group), linewidth = 0.4, color = "#454545", fill = NA),
    geom_polygon(data = map_data("state"), aes(x = long - 0.15, y = lat - 0.15, group = group), linewidth = 0.4, color = "#454545", fill = NA),
    geom_polygon(data = map_data("state"), aes(x = long - 0.1, y = lat - 0.1, group = group), linewidth = 0.4, color = "#454545", fill = NA),
    geom_polygon(data = map_data("state"), aes(x = long - 0.075, y = lat - 0.075, group = group), linewidth = 0.4, color = "#454545", fill = NA),
    geom_polygon(data = map_data("state"), aes(x = long - 0.05, y = lat - 0.05, group = group), linewidth = 0.4, color = "#454545", fill = NA), 
    geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), linewidth = 0.4, color = "#B08D57", fill = NA), 
    id = "map_group") +
  with_shadow("map_group", sigma = 4, x_offset = -40, y_offset = 40, colour = "#D3D3D3") +
  with_outer_glow(
    geom_polygon(data = map_data("state") %>% filter(region == "arizona"), aes(x = long, y = lat, group = group), color = "#FFC72C", linewidth = 0.4, fill = NA),
    colour = "#FFC72C", sigma = 15, expand = 5) +
  geom_text(data = df, aes(x = -116, y = 24.5, label = state), hjust = "left", vjust= "top", family = font_t, fontface = "bold", size = 5, color = "#000000") +
  geom_text(data = df, aes(x = -116, y = 22, label = paste0("joined the union on ", format(admission, "%B %d, %Y"), "\nbecoming the final state to form the\ncontiguous United States, otherwise\nknown as the lower 48 states.")), hjust = "left", vjust= "top", family = font, size = 2.5, color = "#000000") +
  scale_y_continuous(expand = c(0,0), limits = c(8, 55)) +
  scale_x_continuous(expand = c(0,0)) +
  coord_map(clip = "off") +
  theme_void() +
  theme(
        plot.title = element_text(family = font_t, size = 26, hjust = 0.5, face = "bold", color = "#000000", margin = margin(b = 0)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(size = 7, family = font, color = "#000000", hjust = 1, margin = margin(t = 5)),
        legend.position = "none",
        plot.margin = unit(c(1, 0, 1, 0), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "The Lower Forty-Eight States",
       caption = "#TidyTuesday | Data: Wikipedia | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("lower48_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


