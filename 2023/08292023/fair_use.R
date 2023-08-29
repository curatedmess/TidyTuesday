# #TidyTuesday | 2023-08-29 | Fair Use
# Data Source comes from U.S. Copyright Office Fair Use Index

# libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(waffle)

# add font ----------------------------------------------------------------
font_add_google(name = "Inter", family = "Inter")
font <- "Inter"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999) 

# # load data --------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 35)
fair_use_cases <- tuesdata$fair_use_cases
fair_use_findings <- tuesdata$fair_use_findings

# wrangle data and create df ----------------------------------------------
df <- fair_use_cases %>%
  filter(grepl("Circuit", jurisdiction)) %>% 
  mutate(outcome = str_to_title(outcome)) %>% 
  mutate(outcome = case_when(outcome == "Fair Use Found; Second Circuit Affirmed On Appeal" ~ "Fair Use Found",
                             outcome == "Fair Use Found; Mixed Result" ~ "Mixed Result or Remand",
                             outcome == "Preliminary Ruling, Mixed Result, Or Remand" ~ "Mixed Result or Remand",
                             outcome == "Preliminary Ruling, Fair Use Not Found" ~ "Fair Use Not Found",
                             outcome == "Fair Use Not Found; Preliminary Ruling" ~ "Fair Use Not Found",
                             outcome == "Preliminary Finding; Fair Use Not Found" ~ "Fair Use Not Found",
                             outcome == "Preliminary Ruling, Fair Use Not Found" ~ "Fair Use Not Found",
                             outcome == "Preliminary Ruling, Fair Use Not Found, Mixed Result" ~ "Mixed Result or Remand",
                             outcome == "Fair Use Not Found, Preliminary Ruling" ~ "Fair Use Not Found",
                             outcome == "Preliminary Ruling; Fair Use Not Found" ~ "Fair Use Not Found",
                             outcome == "Preliminary Ruling, Remand" ~ "Mixed Result or Remand",
                             outcome == "Mixed Result" ~ "Mixed Result or Remand",
                             TRUE ~ outcome)) %>% 
  group_by(jurisdiction, outcome) %>% 
  summarise(count = n()) %>% 
  mutate(ymax = sqrt(count), xmax = sqrt(count), xmin = 0, ymin = 0) %>% 
  group_by(jurisdiction) %>% 
  mutate(total = sum(count)) %>% 
  ungroup()

df$jurisdiction[df$jurisdiction == "District of Columbia Circuit"] <- "District of Columbia\nCircuit"

  
# create plot -------------------------------------------------------------
df %>% 
  arrange(desc(count)) %>% 
  ggplot(aes(fill = outcome, values = count)) +
  geom_waffle(color = "#D3D3D3", sixe = 0.1, n_rows = 10, flip = TRUE, na.rm = TRUE) +
  facet_wrap(~ fct_reorder(paste0(jurisdiction), total, .desc = TRUE), ncol = 4, strip.position = "bottom") +
  geom_text(aes(x = 5, y = 5, label = total), family = font, size = 7, color = "#FFFFFF") +
  scale_fill_manual(values = c("#390099", "#ff0054", "#ffbd00")) +
  scale_color_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_equal(clip = "off") +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 30, hjust = 0.5, color = "#0d1821", face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 9, hjust = 0.5, color = "#0d1821", lineheight = 1.3, margin = margin(t = 5)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 8, family = font, color = "#0d1821", hjust = 0.5, margin = margin(t = 30)),
        strip.text = element_text(size = 7.5, family = font, face = "bold", color = "#0d1821", hjust = 0, vjust = 1, margin = margin(t = 3)),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 7.5, family = font, color = "#0d1821"),
        legend.box.margin = margin(t = 15, b = 15),
        legend.key.size = unit(0.5, 'cm'),
        plot.margin = unit(c(1, 0, 1, 0), "cm"),
        panel.spacing = unit(1.5, "lines"),
        panel.background = element_rect(color = NA, fill = "#D3D3D3"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "FAIR USE",
       subtitle = "Examples of U.S. copyright enforcement included in the Fair Use Index summarised\nby the Circuit Court and Court Determination of case. Preliminary findings included.",
    caption = "#TidyTuesday | Data: U.S. Copyright Office Fair Use Index | Design: Ryan Hart")
  

# save plot ---------------------------------------------------------------
ggsave(paste0("fair_use_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 8)
  

