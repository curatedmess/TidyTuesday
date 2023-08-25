# #TidyTuesday | 2023-08-22 | Refugees
# Data Source comes from PopulationStatistics {refugees} R package


# libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(scales)

# add font ----------------------------------------------------------------
font_add_google(name = "Inter", family = "Inter")
font <- "Inter"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999) 

# # load data --------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 34)
population <- tuesdata$population

# wrangle data and create df ----------------------------------------------
df <- population %>% 
  select(year, coo_name, coa_name, refugees) %>%
  filter(year >= 2013) %>%
  filter(coo_name == "Syrian Arab Rep.") %>%
  filter(!refugees == 0) %>% 
  group_by(year, coa_name) %>% 
  mutate(total_by_period = sum(refugees)) %>% 
  ungroup() %>% 
  group_by(coa_name) %>% 
  mutate(avg_by_period = mean(total_by_period)) %>% #rank the countries by the avg over period
  ungroup() %>% 
  arrange(avg_by_period) %>% 
  mutate(rank = dense_rank(desc(avg_by_period))) %>% 
  mutate(group = case_when(rank > 3 ~  "Other Countries",
                           coa_name == "Türkiye" ~ "Turkey",
                              TRUE ~ coa_name)) %>% 
  ungroup() %>% 
  select(year, refugees, rank, group) %>% 
  group_by(year, group) %>% 
  mutate(total = sum(refugees)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(year_total = sum(refugees)) %>% 
  ungroup()

# total -------------------------------------------------------------------
df_total <- df %>% 
  summarise(total_num = sum(refugees))

# total -------------------------------------------------------------------
df_total <- df %>% 
  group_by(year) %>% 
  summarise(total_num = sum(refugees))

# labels ------------------------------------------------------------------
labels <- df %>%
  filter(year == 2022) %>%
  select(year, group, total) %>% 
  unique() %>% 
  mutate(y = cumsum(total) - 0.5 * total)

# create stack order ------------------------------------------------------
stack_order <- c("Turkey", "Lebanon", "Jordan", "Germany", "Iraq", "Other Countries")

# create plot -------------------------------------------------------------
df %>% 
  mutate(group = factor(group, levels = stack_order)) %>% 
  ggplot(aes(x = year, y = total, fill = group)) +
  geom_text(data = labels %>% mutate(group = factor(group, levels = stack_order)), aes(x = 2022.1, y = y, label = group, color = group), size = 3, family = font, fontface = "bold", hjust = 0, check_overlap = T) +
  geom_text(aes(x = year, y = year_total, label = scales::unit_format(unit = "M", scale = 1e-6)(year_total)), size = 2.5, family = font, vjust = -2.1, hjust = 0.9, check_overlap = T) +
  geom_area() +
  geom_point(aes(x = year, y = year_total), color = "#000000") +
  geom_segment(aes(x = year, xend = year, y = 0, yend = year_total), linewidth = 0.5, linetype = "dotted", color = "#000000") +
  geom_segment(aes(x = 2012.75, y = 0, yend = 0, xend = 2022.25), linewidth = 0.8, color = "#000000") +
  annotate("text", x = 2013.1, y = 6650000, label = "Total #\nof refugees\nby year", hjust = 0, vjust = 0.5, family = font, size = 2.5, color = "#000000") +
  annotate(geom = "curve", x = 2014.1, y = 6650000, xend = 2015, yend = 5900000, linewidth = 0.3,  curvature = -0.3, arrow = arrow(length = unit(1.25, "mm")), color = "#000000") +
  scale_fill_manual(values = c("#88498f", "#779fa1", "#ff6542", "#564154")) +
  scale_color_manual(values = c("#88498f", "#779fa1", "#ff6542", "#564154")) +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, 8000000), breaks = c(2000000, 4000000, 8000000, 12000000, 16000000), labels = scales::unit_format(unit = "M", scale = 1e-6, sep = "")) +
  scale_x_continuous(limits = c(2012.75, 2023.25), breaks = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 30, hjust = 0, color = "#000000", face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 9, hjust = 0, color = "#000000", lineheight = 1.3, margin = margin(t = 10, b = 10)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 8.5, family = font, color = "#000000", hjust = 0.5, margin = margin(t = 30)),
        axis.text.x = element_text(size = 8, family = font, color = "#000000", hjust = 0.5, margin = margin(t = 5)), 
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#f5f1e8"),
        plot.background = element_rect(color = NA, fill = "#f5f1e8")) +
  labs(title = "Syrian Refugees",
       subtitle = "Refugees identified by UNHCR leaving Syria for other countries for safety between 2013 and 2022. This chart\nillustrates the breakdown of the top three destination countries ranked according to the average number of\nrefugees during these ten years, plus the remaining countries.",
    x = "Change in number of refugees received from 2013 and 2022",
    caption = "#TidyTuesday | Data: {refugees} R package | Design: Ryan Hart")
  
# save plot ---------------------------------------------------------------
ggsave(paste0("refugees_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 6)
  

