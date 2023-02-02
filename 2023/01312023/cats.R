# TidyTuesday | January 31, 2023 | Pet Cats UK
# Data Source is movebank.org

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(sf)


# add fonts ---------------------------------------------------------------
font_add_google(name = "Corben", family = "Corben")
font_add_google(name = "Inter", family = "Inter")
font1 <- "Corben"
font2 <- "Inter"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2023-01-31')

cats_uk <- tuesdata$cats_uk
cats_uk_reference <- tuesdata$cats_uk_reference

# wrangle and create df ---------------------------------------------------

# create list of cat tag_ids to use in the loop ---------------------------
names <- cats_uk %>% 
  inner_join(cats_uk_reference) %>% 
  select(tag_id) %>% 
  unique()

list <- names$tag_id
  
# loop to get colors for each image in list -------------------------------
# sadly I cannot find the link to the post where I found the sample code
# to create the distance matrix...

lapply(list, function(x) {
  
  cats_uk %>% 
    filter(algorithm_marked_outlier == "FALSE", manually_marked_outlier == "FALSE") %>% 
    filter(tag_id == x) %>%
    st_as_sf(coords = c("location_long", "location_lat"), crs="EPSG:4326") %>% 
    distinct() %>% 
    st_distance() %>% 
    as.data.frame() %>% 
    gather(point_id, dist) %>% 
    select(dist) %>% 
    mutate(distance = as.numeric(max(dist))) %>% 
    distinct(distance) %>% 
    mutate(name = paste0(x))
  
}) -> data

# create data frame -------------------------------------------------------
df <- do.call(rbind,  data)

# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  annotate("segment", x = 0, xend = 2400, y = 0, yend = 0, linewidth = 0.3, color = "#FFFFFF") +
  annotate("segment", x = 2238.1411, xend = 2238.1411, y = 0, yend = 3, size = 0.4, color = "#29cdcb", linetype = "dotted") +
  annotate("text", x = 2238.1411, y = 3.1, size = 2.5, color = "#29cdcb", hjust = "right", label = "Dexter2 (1 year old, male) leads the group with a range of 2,238 meters.") +
  annotate("segment", x = 103.9321, xend = 103.9321, y = 0, yend = 2, size = 0.4, color = "#29cdcb", linetype = "dotted") +
  annotate("text", x = 103.9321, y = 2.1, size = 2.5, color = "#29cdcb", hjust = "left", label = "Neva (4 year old, female) had the shortest range of 104 meters.") +
  geom_curve(aes(x = 0, y = 0, xend = distance, yend = 0), size = 0.5, alpha = 0.7, curvature = -0.6, ncp = 500, color = "#fb605c") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, 4)) +
  scale_x_continuous(expand = c(0, 0), limits = c(NA, 2400)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.title = element_text(family = font1, size = 20, hjust = 0, face = "bold", color = "#fb605c"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font2, size = 9, hjust = 0, color = "#FFFFFF"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 7, family = font2, color = "#FFFFFF", hjust = 0),
        axis.title.x = element_text(size = 7, family = font2, color = "#FFFFFF", vjust = -2.5),
        axis.text.x = element_text(size = 7, family = font2, color = "#FFFFFF", vjust = -5),
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = NA, fill = "#000000")) +
  labs(title = "Range of Cats",
       subtitle = "Range is the distance between the two farthermost GPS points within the movement\ntracking data for each of the 101 cats from a UK ecological study.",
       caption = "\n\n\n\n#TidyTuesday | Data: movebank.org | Design: Ryan Hart",
       x = "\nDistance (meters)")

# save plot ---------------------------------------------------------------
ggsave(paste0("cats_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)
