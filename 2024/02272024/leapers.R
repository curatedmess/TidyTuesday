# #TidyTuesday | 2024—02—27 | Leap Day
# Data source comes from the February 29 article on Wikipedia

library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(ggsvg)

# add font ————————————————————————————————————————————————————————————————
font_add_google(name = "Concert One", family = "Concert One")
font <- "Concert One"

font_add_google(name = "Open Sans", family = "Open Sans")
font2 <- "Open Sans"

# turn on showtext ————————————————————————————————————————————————————————
showtext_auto()
showtext_opts(dpi = 320)

# load data ——————————————————————————————————————————————————————————————
tuesdata <- tidytuesdayR::tt_load(2024, week = 9)

# events <- tuesdata$events
births <- tuesdata$births
# deaths <- tuesdata$deaths


# get emoji icon from https://openmoji.org --------------------------------
# get person image -------------------------------------------------------
svg_url <- 'https://openmoji.org/data/color/svg/1F464.svg'
svg_txt <- paste(readLines(svg_url), collapse = "\n")

# create data frame -------------------------------------------------------
df <- births %>% 
  filter(is.na(year_death)) %>% 
  select(!c(description, year_death)) %>% 
  # add_row(year_birth = 1960, person = "Tony Robbins") %>% #Tony Robbins oddly missing from this data set, decided to not manually add him
  filter(!person %in% c("Aileen Wuornos", "Bob Speller")) %>%  #These two people are reported as having passed away even though the dataset has date of death as NA
  mutate(birthday = as.Date(paste0(year_birth, "-02-29")),
         age = year(as.Date("2024-02-29")) - year(birthday)) %>% 
         # no_of_leap_day_celebrations = sapply(birthday, function(bd) sum(format(seq(bd, as.Date("2024-02-29"), by = "years"), "%m-%d") == "02-29")) - 1) %>% #did not use
  group_by(age) %>% 
  mutate(count = n()) %>% 
  mutate(y = seq_along(age))

# age labels --------------------------------------------------------------
df_dates <- data.frame(age = seq(20, 96, by = 4))

# icon legend -------------------------------------------------------------
df_icon <- data.frame(age = 84, y = 9)

# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  geom_point_svg(aes(x = age, y = y), svg = svg_txt, size = 8) +
  geom_point_svg(data = df_icon, aes(x = age, y = y), svg = svg_txt, size = 8) +
  geom_text(data = df_icon, aes(x = age + 2, y = y, label = " = 1 Leaper"), family = font2, size = 3, hjust = 0) +
  geom_text(data = df_dates, aes(x = age, y = -0.25, label = age), color = "#000000", family = font2, size = 3) +
  geom_hline(yintercept = 0.25, linewidth = 2, color = "#000000") +
  geom_text(aes(x = 60, y = -1.25, label = "AGE ON FEBRUARY 29, 2024"), family = font, size = 4, check_overlap = T) +
  scale_color_identity() +
  scale_y_continuous(limits = c(-1.5, 9.5)) +
  theme_void() +
  theme(plot.title = element_text(family = font, hjust = 0.5, size = 33, color = "#000000", margin = margin(b = 5)),
        plot.subtitle = element_text(family = font2, hjust = 0.5, size = 8.25, color = "#000000", margin = margin(b = 0), lineheight = 1.2),
        plot.title.position = "plot",
        plot.caption = element_text(family = font2, hjust = 0.5, size = 7, color = "#000000", margin = margin(t = 25)),
        plot.caption.position = "plot",
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "HAPPY BIRTHDAY LEAPERS!",
       subtitle = "Age distribution of 63 notable people born on a leap day listed on Wikipedia's February 29 page",       
       caption = "#TidyTuesday | Data: Wikipedia | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("leapers_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


