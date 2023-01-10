# TidyTuesday | January 10, 2023 | Project FeederWatch
# Data Source is Project FeederWatch

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(geojsonio)
library(sf)
library(broom) 
library(rgeos)
library(scico)


# add font ----------------------------------------------------------------
font_add_google(name = "Chilanka", family = "Chilanka")
font_add_google(name = "Gafata", family = "Gafata")
font2 <- "Chilanka"
font1 <- "Gafata"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
feederwatch <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv')

# wrangle and create df ---------------------------------------------------
# used for the states
df <- feederwatch %>% 
  filter(Year == 2021) %>% 
  group_by(Year, subnational1_code) %>% 
  summarise(count = n_distinct(species_code)) %>% 
  ungroup() %>% 
  filter(grepl("US-", subnational1_code)) %>% 
  mutate(state = gsub("US-", "", subnational1_code)) %>% 
  mutate(state = state.name[match(state, state.abb)]) %>% 
  mutate(state = replace(state, subnational1_code == "US-DC", "District of Columbia"))

# total
df_total <- feederwatch %>% 
  #filter(Year == 2021) %>% 
  filter(grepl("US-", subnational1_code)) %>% 
  summarise(count = n_distinct(species_code)) 
  
# load hex bin shapes -----------------------------------------------------
hex_states <- geojson_read("us_states_hexgrid.geojson", what = "sp")

# clean up state names ----------------------------------------------------
hex_states@data = hex_states@data %>% 
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# create data frame for hex plot ------------------------------------------
hex_states_fortify <- tidy(hex_states, region = "google_name")

df_temp <- hex_states_fortify %>% 
  left_join(. , df, by = c("id" = "state")) %>% 
  mutate(id = state.abb[match(id, state.name)])
  
# fix DC ------------------------------------------------------------------
df_temp$id[df_temp$group == "District of Columbia.1"] <- "DC"

# create labels for state -------------------------------------------------
labels <- cbind.data.frame(data.frame(gCentroid(hex_states, byid = TRUE), id = hex_states@data$iso3166_2))

# combine hex and labels for df -------------------------------------------
df_final <- df_temp %>% 
  left_join(. , labels, by = "id") 

# create plot -------------------------------------------------------------
df_final %>% 
  ggplot () +
  geom_polygon(aes(x = long, y = lat, group = group, fill = count), color="#000000", linewidth = 0.5) +
  scale_fill_scico(palette = "grayC", direction = 1) +
  coord_map(clip = "off") +
  geom_text(aes(x = x, y = y, label = id, color = count <= 70), family = font1, size = 3, show.legend = FALSE) +
  scale_color_manual(values = c("#FFFFFF", "#000000")) +
  scale_y_continuous(limits = c(25, 57)) +
  #annotate(geom = "text", x = -104, y = 54, label = "331 unique species of birds observed, which is less\nthan half of the potential bird species found in the U.S.", hjust = "center", family = font2, size = 3, color = "#000000") +
  annotate(geom = "text", x = -136, y = 37, label = "6 bird\nspecies", hjust = "left", family = font2, size = 3, color = "#000000") +
  annotate(geom = "segment", x = -136, y = 35, xend = -136, yend = 33, size = 0.4,  arrow = arrow(length = unit(1.5, "mm")), color = "#000000") +
  annotate(geom = "text", x = -122, y = 27, label = "139 bird\nspecies", hjust = "left", family = font2, size = 3, color = "#000000") +
  annotate(geom = "segment", x = -116, y = 28.5, xend = -113, yend = 28.5, size = 0.4,  arrow = arrow(length = unit(1.5, "mm")), color = "#000000") +
  theme_void() +
  theme(plot.title = element_text(family = font1, size = 29, hjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font1, size = 11, hjust = 0.5, lineheight = 1.1),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font1, color = "#000000", hjust = 0.5),
        legend.position = c(0.5, 0.85),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = 8, family = font1, color = "#000000"),
        legend.key.height = unit(0.25, 'cm'),
        legend.key.width = unit(1.25, 'cm'),
        plot.margin = unit(c(0, 0.25, 0, 0.25), "cm"),
        plot.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF")) +
  labs(title = "BIRD SPECIES",
       subtitle = "The total number of unique bird species observed by volunteers\nin 2021 by state (and DC) as a part of the FeederWatch Program.\n",
       caption = "\n\n\n#TidyTuesday | Data: FeederWatch.org | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("feederwatch_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)
