# #TidyTuesday | 2023-12-5 | Life Expectancy
# Data Source comes from Our World in Data

# libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(scales)

# add font ----------------------------------------------------------------
font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999) 

# load data --------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 49)

life_expectancy <- tuesdata$life_expectancy
# life_expectancy_different_ages <- tuesdata$life_expectancy_different_ages
# life_expectancy_female_male <- tuesdata$life_expectancy_female_male

# wrangle data and create data frame --------------------------------------
data <- life_expectancy %>%
  filter(Entity %in% c("High-income countries", "Low-income countries", "Middle-income countries")) %>% 
  filter(Year == 2021) %>% 
  mutate(RoundedValue = round(LifeExpectancy * 365)) %>% 
  mutate(MaxValue = max(RoundedValue)) %>% 
  select(Entity, MaxValue, RoundedValue) %>% 
  mutate(Entity = toupper(Entity))

# create points -----------------------------------------------------------
num <- max(data$MaxValue)

x <- runif(num)
y <- runif(num)

grid <- data.frame(x = x, y = y) %>% 
  arrange(x)

# triplicate the grid and mutate the entity columns -----------------------
triplicated_grid <- bind_rows(replicate(3, grid, simplify = FALSE), .id = "replicate_id") %>%
  mutate(Entity = rep(data$Entity, each = num))

# join to df --------------------------------------------------------------
triplicated_grid <- left_join(triplicated_grid, data[, c("Entity", "RoundedValue")], by = "Entity")

# add binary color flag ---------------------------------------------------
df <- triplicated_grid %>%
  group_by(Entity) %>%
  mutate(Color = ifelse(row_number() <= first(RoundedValue), 1, 0)) %>%
  ungroup()
  

# create df for the text --------------------------------------------------
df2 <- data %>% 
  mutate(delta = MaxValue - RoundedValue) %>% 
  slice(2:3) %>% 
  mutate(x = ifelse(delta == 6503, 0.89, 0.94))
  
# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x, y, color = as.factor(Color))) +
  geom_point(size = 0.06) +
  geom_text(data = data, x = 0.05, y = mean(y), aes(label = paste0(comma(RoundedValue), "\ndays")), family = font, fontface = "bold", size = 10, color = "#FFFFFF", hjust = 0, lineheight = 0.8) +
  geom_text(data = df2, aes(x = x, y = mean(y), label = paste0(comma(delta), "\nfewer\ndays")), family = font, fontface = "bold", size = 4, color = "#FF1744", vjust = 0.5, lineheight = 0.9) +
  scale_color_manual(values = c("0" = "#DDDDDD", "1" = "#000000")) +
  scale_x_continuous(limits = c(NA, 1.05)) +
  facet_wrap(~ factor(Entity, levels=c("HIGH-INCOME COUNTRIES", "MIDDLE-INCOME COUNTRIES", "LOW-INCOME COUNTRIES")), ncol = 1) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(text = element_text(size = 9, family = font, color = "#000000"),
      plot.title = element_text(family = font, size = 18, hjust = 0.5, face = "bold"),
      plot.title.position = "plot",
      plot.subtitle = element_text(family = font, size = 8, hjust = 0.5, margin = margin(t = 5, b = 15)),
      plot.caption = element_text(family = font, size = 7.5, hjust = 0.5, margin = margin(t = 15)),
      strip.text = element_text(size = 10, family = font, color = "#000000", margin = margin(t = 1, b = 1)),
      axis.title = element_blank(),
      legend.position = "none",
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      plot.background = element_rect(fill = "#FFFFFF")) +
  labs(title = "LIFE EXPECTANCY GIVEN BIRTH YEAR 2021",
       subtitle = "Visualized as days (each point equals one day and assumes 365 days per year)",
       caption = "#TidyTuesday | Data: Our World in Data | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("life_expectancy_days_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

