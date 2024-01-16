#TidyTuesday | 2024-01-16 | U.S. Polling Places
# Data source comes from The Center for Public Integrity

# libraries ---------------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(ggchicklet)
# library(ggforce)

# add font ----------------------------------------------------------------
font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

font_add_google(name = "Overpass", family = "Overpass")
font_s <- "Overpass"


# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999) 

# load data --------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2024-01-16')
polling_places <- tuesdata$polling_places

# wrangle data ------------------------------------------------------------
df <- polling_places %>%
  filter(election_date == "2020-11-03") %>% 
  mutate(search_term = case_when(
      # grepl("Main\\ Street|Main\\ St", address, ignore.case = TRUE) ~ "Main Street",
      grepl("First\\ Street|First\\ St|1st\\ Street|1st\\ St|First\\ Avenue|First\\ Ave|1st\\ Avenue|1st\\ Ave", address, ignore.case = TRUE) ~ "First/1st",
      grepl("Second\\ Street|Second\\ St|2nd\\ Street|2nd\\ St|Second\\ Avenue|Second\\ Ave|2nd\\ Avenue|2nd\\ Ave", address, ignore.case = TRUE) ~ "Second/2nd",
      grepl("Third\\ Street|Third\\ St|3rd\\ Street|3rd\\ St|Third\\ Avenue|Third\\ Ave|3rd\\ Avenue|3rd\\ Ave", address, ignore.case = TRUE) ~ "Third/3rd",
      grepl("Fourth\\ Street|Fourth\\ St|4th\\ Street|4th\\ St|Fourth\\ Avenue|Fourth\\ Ave|4th\\ Avenue|4th\\ Ave", address, ignore.case = TRUE) ~ "Fourth/4th",
      grepl("Fifth\\ Street|Fifth\\ St|5th\\ Street|5th\\ St|Fifth\\ Avenue|Fifth\\ Ave|5th\\ Avenue|5th\\ Ave", address, ignore.case = TRUE) ~ "Fifth/5th",
      grepl("Sixth\\ Street|Sixth\\ St|6th\\ Street|6th\\ St|Sixth\\ Avenue|Sixth\\ Ave|6th\\ Avenue|6th\\ Ave", address, ignore.case = TRUE) ~ "Sixth/6th",
      grepl("Seventh\\ Street|Seventh\\ St|7th\\ Street|7th\\ St|Seventh\\ Avenue|Seventh\\ Ave|7th\\ Avenue|7th\\ Ave", address, ignore.case = TRUE) ~ "Seventh/7th",
      grepl("Eighth\\ Street|Eighth\\ St|8th\\ Street|8th\\ St|Eighth\\ Avenue|Eighth\\ Ave|8th\\ Avenue|8th\\ Ave", address, ignore.case = TRUE) ~ "Eighth/8th",
      grepl("Ninth\\ Street|Ninth\\ St|9th\\ Street|9th\\ St|Ninth\\ Avenue|Ninth\\ Ave|9th\\ Avenue|9th\\ Ave", address, ignore.case = TRUE) ~ "Ninth/9th",
      grepl("Tenth\\ Street|Tenth\\ St|10th\\ Street|10th\\ St|Tenth\\ Avenue|Tenth\\ Ave|10th\\ Avenue|10th\\ Ave", address, ignore.case = TRUE) ~ "Tenth/10th")) %>% 
  group_by(search_term) %>%
  summarize(count = n()) %>% 
  na.omit()



# wrangle data ------------------------------------------------------------
df_main <- polling_places %>%
  filter(election_date == "2020-11-03") %>% 
  mutate(search_term = case_when(grepl("Main\\ Street|Main\\ St|Main\\ Avenue|Main\\ Ave", address, ignore.case = TRUE) ~ "Main")) %>% 
  group_by(search_term) %>%
  summarize(count = n()) %>% 
  na.omit()


# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(y = count, x = reorder(search_term, count))) +
  geom_chicklet(fill = "#308446", color = "#000000", radius = grid::unit(2, "mm"), size = 1.2, width = 0.75) +
  geom_text(aes(y = 25, x = search_term, label = search_term), color = "#FFFFFF", family = font_s, hjust = 0, size = 4) +
  geom_text(aes(label = scales::comma(count)), color = "#000000", family = font, fontface = "bold", hjust = 0, size = 4, nudge_y = 10) +
  # street pole
  geom_segment(aes(y = -30, yend = -30, x = 0, xend = 11), linewidth = 7.5) +
  geom_point(aes(x = 11.1, y = -30), size = 12) +
  # annotations
  annotate("text", y = 625, x = 2, label = "2020 Election Polling Locations", color = "#000000", family = font, fontface = "bold", hjust = 0, size = 4) +
  annotate("text", y = 625, x = 1.65, label = 'Counting the voting spots by numbered\n"avenues and streets" from the last major\nU.S. election (First/1st through Tenth/10th)', color = "#000000", family = font, hjust = 0, vjust = "top", size = 3.2, lineheight = 1.2) +
  coord_flip(clip = "off") +
  scale_y_continuous(limits = c(-35, 1200), expand = c(0, 0)) +
  theme_void() +
  theme(plot.caption = element_text(hjust = 0.5, size = 8, color = "#000000", margin = margin(t = 10)),
        plot.caption.position = "plot",
        legend.position = "none",
        panel.grid = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(caption = "#TidyTuesday | Data: The Center for Public Integrity | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("polling_spots_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


