# #TidyTuesday | 2024—03-12 | Fiscal Sponsor
# Data source comes from the Fiscal Sponsor Directory

library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(ggforce)

# add font ————————————————————————————————————————————————————————————————
font_add_google(name = "Luckiest Guy", family = "Luckiest Guy")
font2 <- "Luckiest Guy"

font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

# turn on showtext ————————————————————————————————————————————————————————
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# load data ——————————————————————————————————————————————————————————————
tuesdata <- tidytuesdayR::tt_load(2024, week = 11)

fiscal_sponsor_directory <- tuesdata$fiscal_sponsor_directory

# create df ---------------------------------------------------------------
df <- fiscal_sponsor_directory %>% 
  mutate(type = ifelse(grepl("arts and culture", project_types, ignore.case = TRUE), "AC", "REST")) %>% 
  group_by(type) %>% 
  summarise(test2 = sum(n_sponsored, na.rm = TRUE)) %>% 
  mutate(perc = test2/sum(test2)) %>% 
  filter(type == "AC")


# inspiration for polygon circle code taken from here ---------------------
# https://stackoverflow.com/questions/74297714/ggplot2-function-fo --------

area_percentage_to_remove <- 1 - df$perc

# calculate the desired area percentage to remove
radius_to_remove <- sqrt(area_percentage_to_remove)

# create a data frame with the points of the circle
circle_points <- tibble(deg = seq(0, 360, length.out = 1000),
                        r = 1,
                        x = r * cos((deg * pi) / 180), 
                        y = r * sin((deg * pi) / 180))

# calculate the corresponding y-coordinate for the cut
y_cut <- radius_to_remove

# filter the points based on the y-coordinates
cut_points <- circle_points %>%
  filter(y <= y_cut)


# create the plot ---------------------------------------------------------
ggplot() +
  geom_polygon(data = cut_points, aes(x = x, y = y), fill = "#000000") +
  annotate("text", x = 0, y = 0.6, label = "Arts and Culture", family = font2, size = 9, vjust = "bottom") +
  geom_text(data = df, aes(x = 0, y = 0, label = scales::percent(perc, accuracy = 0.01)), family = font2, size = 20, color = "#FFFFFF") +
  annotate("text", x = 0, y = -0.45, label = "of the reported projects receiving fiscal sponsorship from\n1957 to 2023 are from 501(c)(3) groups focused\non the arts and culture, according to\nfiscalsponsordirectory.org", family = font, size = 3, color = "#FFFFFF") +
  scale_y_continuous(limits = c(-1.1, 1.1)) +
  scale_x_continuous(limits = c(-1.1, 1.1)) +
  coord_equal() +
  theme_void() +
  theme(plot.caption = element_text(family = font, hjust = 0.5, size = 7, color = "#000000", margin = margin(t = 25)),
        plot.caption.position = "plot",
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(caption = "#TidyTuesday | Data: Fiscal Sponsor Directory | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("ac_fiscal_sponsorship_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

