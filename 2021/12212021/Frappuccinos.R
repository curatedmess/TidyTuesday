# tidytuesday Week 12-21-2021 | Starbucks
# Data: PythonCoderUnicorn and Starbucks

# libraries
library(tidytuesdayR)
library(tidyverse)
library(showtext)

# add font
font_add_google(name = "Open Sans", family = "Open Sans")

# turn on showtext
showtext_auto()

# load data
tuesdata <- tidytuesdayR::tt_load('2021-12-21')
starbucks <- tuesdata$starbucks

#view data
view(starbucks)


# wrangle the data
sbux_data <- starbucks %>%
  filter(size == 'venti') %>%
  filter(grepl('Frappuccino', product_name)) %>%
  group_by(product_name) %>%
  summarise_at(vars(calories), list(mean_calories = mean))

# plot data
sbux_data %>%
  mutate(product_name = fct_reorder(product_name, mean_calories)) %>%
  ggplot(aes(x = product_name, y = mean_calories)) +
  geom_bar(stat = "identity", fill = "black") +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(expand = c(0,0)) +
  theme(text = element_text(family = "Open Sans", color = "#000000"),
        plot.title = element_text(size=20, hjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.subtitle = element_text(size = 10, hjust = 0.5, margin = margin(0,0,20,0)),
        plot.caption = element_text(size = 8, hjust = 0.5, margin=margin(20,0,0,0)),
        plot.margin = unit(c(1.25,2,2,1.5), "cm"),
        axis.text = element_text(family = "Open Sans", color = "#000000"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_rect(color = "white", fill = "white")) +
  labs(title = "BATTLE OF THE FRAPPUCCINOS",
        subtitle = "The mean number of calories for a Venti drink (includes milk options and choice of whip)",
        caption = "\n#tidytuesday | Data: PythonCoderUnicorn and Starbucks | Design: Ryan Hart")

  
# save plot
ggsave(paste0("Frappucinos_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 6)

