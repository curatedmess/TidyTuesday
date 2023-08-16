# #TidyTuesday | 2023-08-15 | Spam E-mail
# Data Source comes from Vincent Arel-Bundock's Rdatasets package(https://vincentarelbundock.github.io/Rdatasets/index.html).

# code is not efficient, get vector memory issues if run too many times

# libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(ggtext)
library(ggsvg)
library(reshape2)

# add font ----------------------------------------------------------------
font_add_google(name = "Lato", family = "Lato")
font <- "Lato"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# get email images --------------------------------------------------------
svg_txt_y <- paste(readLines("mail-17-svgrepo-com-3.svg"), collapse = "\n")
svg_txt_n <- paste(readLines("mail-17-svgrepo-com.svg"), collapse = "\n")

# # load data --------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 33)
spam <- tuesdata$spam

# wrangle data and create df ----------------------------------------------
df <- spam %>% 
  filter(dollar != 0) %>% 
  select(dollar, yesno) %>%
  group_by(yesno) %>% 
  summarise(total = n())

# create a grid based on the total number of points -----------------------
num_points_side <- sqrt(1400)

grid_data <- expand.grid(x = seq(0, 1, length.out = num_points_side),
                         y = seq(0, 1, length.out = num_points_side)) %>% 
  arrange(desc(x))

# load excel file used to create the dollar sign pattern ------------------
csv_file <- "path/to/your/csv/file.csv"
data <- read.csv('dollar.csv', header = FALSE)

# wrangle the excel/csv data to xy points ---------------------------------
data_long <- data %>%
  rownames_to_column(var = "X") %>%
  pivot_longer(cols = -X, names_to = "Y", values_to = "value") %>% 
  select(value)

# combine the two data frames ---------------------------------------------
df_final <- cbind(grid_data, data_long)

# create plot -------------------------------------------------------------
df_final %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point_svg(mapping = aes(svg = ifelse(value == "y", svg_txt_y, svg_txt_n)), size = 3.25) +
  geom_richtext(aes(x = -0.10, y = 0.5, label = "You've got <span style='color: #FF0000;'>SPAM</span>"), family = font, color = "#FFFFFF", hjust = 0.5, size = 14, fill = NA, label.color = NA) +
  geom_text(aes(x = -0.20, y = 0.5, label = "79% of 1,400 emails with a $ symbol are spam"), family = font, color = "#FFFFFF", hjust = 0.5, size = 5) +
  scale_x_continuous(limits = c(-0.25, NA)) +
  coord_flip() +
  theme_void() +
  theme(plot.caption.position = "plot",
          plot.caption = element_text(size = 9, family = font, color = "#FFFFFF", hjust = 0.5, margin = margin(t = 10)),
          legend.position = "none",
          plot.margin = unit(c(1, 1, 1, 1), "cm"),
          panel.background = element_rect(color = NA, fill = "#000000"),
          plot.background = element_rect(color = NA, fill = "#000000")) +
  labs(caption = "#TidyTuesday | Data: Rdatasets | Design: Ryan Hart")
  
# save plot ---------------------------------------------------------------
ggsave(paste0("spam_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 8)
  
  
