# TidyTuesday | November 29, 2022 | World Cup
# Data Source is from FIFA World Cup

# load libraries ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(tidygeocoder)
library(rnaturalearth)
library(sf)
library(ggtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
worldcups <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv')

# some date wrangling and create geo codes for countries ------------------
host_countries_df <- worldcups %>% 
  select(year, host) %>% 
  mutate(host = strsplit(as.character(host), ", ")) %>% 
  unnest(host) %>% 
  geocode(host, method = "osm", lat = y, long = x)

#n_distinct(host_countries_df$host)

# robinson projection -----------------------------------------------------
robinson <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# transform to sf points --------------------------------------------------
host_points_sf <- st_as_sf(host_countries_df, coords = c("x", "y"), crs = 4326) %>% 
  st_transform(host_points_sf, crs = robinson)

# create base map --------------------------------------------------------
map <- ne_countries(scale = 50, returnclass = 'sf') %>% 
  st_transform(graticules, crs = robinson)

# create the robinson graticules ------------------------------------------
graticules <- st_graticule(lat = seq(-80, 80, 10), lon = seq(-180, 180, 10)) %>% 
  st_transform(graticules, crs = robinson)

# create plot -------------------------------------------------------------
ggplot() +
  geom_sf(data = map, size = 0.1, color = "#28282B") +
  geom_sf(data = graticules$geometry, size = 0.1, color = "#7B7B7C") +
  geom_sf(data = host_points_sf, size = 1, aes(color = ifelse(year == 2002, "yes", "no"))) +
  scale_color_manual(values = c("#28282B", "#e84393")) +
  coord_sf() +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 20, hjust = 0.5, color = "#28282B"),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(family = font, size = 8, hjust = 0.5, color = "#28282B", lineheight = 1.3),
        plot.caption = element_text(family = font, hjust = 0.5, size = 8, color = "#28282B"),
        plot.caption.position = "plot",
        legend.position = "none",
        plot.margin = margin(0, 0, 0, 0,"cm"),
        plot.background = element_rect(color = NA, fill = "#F2F2F2"),
        panel.background = element_rect(color = NA, fill = "#F2F2F2")) +
  labs(title = "FIFA WORLD CUP",
       subtitle = "Seventeen countries have hosted the World Cup tournament<br>between 1930 and 2018, with <span style='color:#e84393;'><b>2002</b></span> being the only time two<br>countries (Japan & South Korea) co-hosted the tournament.<br>",
       caption = "\n\n#TidyTuesday | Data: FIFA World Cup | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("fifa_worldcup_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 5)

