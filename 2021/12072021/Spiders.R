# tidytuesday Week 12-07-2021 | Spiders
# Data: World Spider Database

#libraries
library(tidytuesdayR)
library(tidyverse)
library(showtext)

#add font
font_add_google(name = "Cabin", family = "Cabin")

#turn on showtext
showtext_auto()

#load data
tuesdata <- tidytuesdayR::tt_load('2021-12-07')
spiders <- tuesdata$spiders

#view data
view(spiders)

#wrangle data (create new df with decades)
decade <- spiders %>%
  mutate(decade = floor(year/10) * 10) 
  
#view data
view(decade)

#plot data
decade %>%
  ggplot(aes(x=decade)) +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    geom_bar(stat="count", fill = "#000000") +
    #geom_text(stat="count", aes(label = ..count..)) +
    scale_x_continuous(breaks=c(1750, 1800,1850,1900,1950,2000), 
                       labels=c("1750s", "1800s", "1850s", "1900s","1950","2000s")) +
    labs (x= "Decades", y = "Number of Spiders", 
          title = "Spider Taxonomy", 
          subtitle = "Rise of new spider species described by researchers in the last few decades.\n",
          caption = "\n#tidytuesday | Data: World Spider Database | Design: Ryan Hart") +
    theme_classic() +
    theme(text = element_text(family = "Cabin", color = "#000000"),
          plot.title = element_text(size=20, hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(0,0,20,0)),
          plot.caption = element_text(size = 9, hjust = 0.5, margin=margin(20,0,0,0)),
          plot.margin = unit(c(1.25,1.5,1.25,1.5), "cm"),
          axis.title = element_text(size = 11),
          axis.text = element_text(size = 10, color ="#000000"),
          axis.line = element_line(color = "#000000")) +
      annotate(geom = "curve", x = 1960, y = 8500, xend = 2000, yend = 8000,
         curvature = -0.2, arrow = arrow(length = unit(2, "mm"))) +
      annotate(geom = "text", x = 1950, y = 8500, label = "8,188 spiders in the 2010s", hjust = "right", family = theme_get()$text[["family"]], 
           size = 4)

#save plot
  ggsave(paste0("spiders_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 6)
                  