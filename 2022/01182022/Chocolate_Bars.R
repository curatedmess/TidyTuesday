# TidyTuesday | January 18 - Cocoa Beans
# Data source is Flavors of Cacao

# install libraries -------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(patchwork)

# add font ----------------------------------------------------------------
font_add_google(name = "Open Sans", family = "Open Sans")

# turn on showtext --------------------------------------------------------
showtext_auto()

# get data ----------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2022-01-18')
chocolate <- tuesdata$chocolate

# wrangle data for bar plot 1 ---------------------------------------------------
# there is likely cleaner and shorter way to execute here...
bar1_df <- chocolate %>%
    group_by(company_location) %>%
    mutate(group = ifelse(company_location == "U.S.A.", "U.S.A.", "Rest of the World")) %>% 
    ungroup %>% 
    group_by(group, company_manufacturer) %>%
    select (group, company_manufacturer) %>% 
    unique() %>% 
    group_by(group) %>% 
    summarise(cnt = n()) %>% 
    mutate(perc = round(cnt / sum(cnt) *100, 1 )) %>% 
    mutate(colortext = case_when(
      group == "U.S.A." ~ "light",
      group == "Rest of the World" ~ "dark"))

# wrangle data for bar plot 2 ---------------------------------------------------
bar2_df <- chocolate %>%
  group_by(company_location) %>%
  mutate(group = ifelse(company_location == "U.S.A.", "U.S.A.", "Rest of the World")) %>%
  ungroup %>% 
  group_by(group) %>%
  summarise(cnt = n()) %>% 
  mutate(perc = round(cnt / sum(cnt) *100, 1 )) %>% 
  mutate(colortext = case_when(
    group == "U.S.A." ~ "light",
    group == "Rest of the World" ~ "dark"))

# wrangle data for boxplot ---------------------------------------------------
box_df <- chocolate %>%
  group_by(company_location) %>%
  mutate(group = ifelse(company_location == "U.S.A.", "U.S.A.", "Rest of the World")) %>% 
  group_by(group) %>% 
  mutate(colortext = case_when(
    group == "U.S.A." ~ "light",
    group == "Rest of the World" ~ "dark"))


# create plot bar1 (manufacturer location) -------------------------------------------------------------
bar1 <-  bar1_df %>%
    ggplot(aes(x = group, y = cnt, fill = group)) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = c("#c39f7a", "#5e3900")) +
    geom_text(aes(label = paste0(perc, "%")), color = "#ffffff", fontface = "bold", vjust = 2.5) +
    scale_y_continuous(expand = c(0,0)) +
    theme_minimal() +
    theme(legend.title = element_blank(),
        axis.line.x = element_line(color = "#000000"),
        axis.title = element_blank(),
        axis.text.x = element_text(family = "Open Sans", color = "#000000", size = 10),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
      labs(subtitle = "Manufacturers by Region")

# create plot pie 2 (bar location) -------------------------------------------------------------
bar2 <-  bar2_df %>%
  ggplot(aes(x = group, y = cnt, fill = group)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("#c39f7a", "#5e3900")) +
  geom_text(aes(label = paste0(perc, "%")), color = "#ffffff", fontface = "bold", vjust = 2.5) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.line.x = element_line(color = "#000000"),
        axis.title = element_blank(),
        axis.text.x = element_text(family = "Open Sans", color = "#000000", size = 10),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  labs(subtitle = "Chocolate Bars by Region")

# create box plot -------------------------------------------------------------
box <- box_df %>%
  ggplot(aes(x = group, y = rating)) +
  geom_boxplot(aes(color = colortext), show.legend = FALSE, size = 0.8) +
  scale_fill_manual(values = c("#c39f7a", "#5e3900")) +
  geom_jitter(aes(color = colortext), size = 0.4, alpha = 0.9, show.legend = FALSE) +
  stat_summary(aes(color = colortext), fun = mean, geom = "point", shape = 20, size = 10) +
  scale_color_manual(values = c("light" = "#5e3900", "dark" = "#c39f7a"), guide = "none") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.line.x = element_line(color = "#000000"),
        axis.title.y = element_text(family = "Open Sans", color = "#000000", size = 10),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Open Sans", color = "#000000", size = 10),
        axis.text.x = element_text(family = "Open Sans", color = "#000000", size = 10),
        axis.ticks = element_blank()) +
  labs(subtitle = "The ratings for chocolate bars from the U.S.A. are comparable to the\nRest of the World with a slightly lower mean average.",
       y = "Rating Score\n")

# create patchwork plot ---------------------------------------------------
plot <- (bar1 + bar2) / box

plot + plot_annotation(
    title = 'CHOCOLATE BARS',
    subtitle = 'How do chocolate bars from the U.S.A. compare to the Rest of the World?',
    caption = '\n#TidyTuesday | Data: Flavors of Cacao | Design: Ryan Hart') &
    theme(text = element_text(family = "Open Sans", color = "#000000", size = 12),
          plot.title = element_text(size = 26, hjust = 0.5, face = "bold"),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(10,0,20,0)),
          plot.caption = element_text(size = 11, hjust = 0.5, margin=margin(20,0,0,0)),
          plot.margin = unit(c(1, 1, 1.25, 1), "cm"),
          plot.background = element_rect(color = "#ffffff", fill = "#ffffff"))

# save plot
ggsave(paste0("Chocolate_Bars_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 12)

    
