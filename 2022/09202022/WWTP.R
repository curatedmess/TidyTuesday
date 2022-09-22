# TidyTuesday | September 20, 2022 | Week 38
# Data source is Macedo et al, 2022 by way of Data is Plural


# load libraries ---------------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(scales)
library(scico)


# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()

# load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2022-09-20')
HydroWASTE_v10 <- tuesdata$HydroWASTE_v10


# create df ---------------------------------------------------------------
df <- HydroWASTE_v10 %>% 
  filter(!STATUS %in% c("Closed", "Projected", "Non-Operational", "Under Construction", "Decommissioned", "Proposed")) %>%
  select(COUNTRY, POP_SERVED, LEVEL) %>% 
  filter(POP_SERVED >= 1000000)#filter on population greater than 1M


# set factors -------------------------------------------------------------
df$LEVEL <- factor(df$LEVEL, levels = c("Primary", "Secondary", "Advanced"))

  
# create plot -------------------------------------------------------------
df %>%
  ggplot(aes(x = POP_SERVED, color = LEVEL, fill = LEVEL)) +
  geom_density(alpha = 0.3) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_color_scico_d(palette = "hawaii") +
  scale_fill_scico_d(palette = "hawaii") +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(text = element_text(size = 9, family = font, color = "#000000"),
        plot.title = element_text(family = font, size = 22, hjust = 0, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 9, hjust = 0),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 7, family = font, color = "#000000", hjust = 0),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 8, family = font, color = "#000000"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, family = font, color = "#000000"),
        legend.position = "top",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#000000", size = 0.25, linetype = "dotted"),
        axis.line.x.bottom = element_line(color = "#000000", size = 0.4),
        plot.margin = unit(c(1.3, 1.3, 1.3, 1.3), "cm"),
        plot.background = element_rect(color = "#F2F2F2", fill = "#F2F2F2")) +
  labs(title = "Wastewater Treatment Plants",
       subtitle = "By the three levels, the distribution of the estimated population\nserved (greater than one million) from 239 wastewater treatment\nplants globally.\n",
       caption = "\n#TidyTuesday | Data:  Macedo et al, 2022 by way of Data is Plural | Design: Ryan Hart",
       x = "\nEstimated Population Served\n")


# save plot ---------------------------------------------------------------
ggsave(paste0("WWTP_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)



  
  
  