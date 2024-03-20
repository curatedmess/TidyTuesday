# #TidyTuesday | 2024—03-19 | X-Men Mutant Moneyball
# Data source comes from Rally's Mutant moneyball: a data driven ultimate X-men by Anderson Evans

library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(ggtext)
library(patchwork)
library(ggsvg)

# add font ————————————————————————————————————————————————————————————————
font_add_google(name = "Bangers", family = "Bangers")
font2 <- "Bangers"

font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

# turn on showtext ————————————————————————————————————————————————————————
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# load data ——————————————————————————————————————————————————————————————
tuesdata <- tidytuesdayR::tt_load(2024, week = 12)

mutant_moneyball <- tuesdata$mutant_moneyball

# get callout shape -------------------------------------------------------
svg_url <- 'https://www.svgrepo.com/download/398208/right-anger-bubble.svg'
svg_txt <- paste(readLines(svg_url), collapse = "\n")

# create data frame -------------------------------------------------------
df_80 <- mutant_moneyball %>% 
  select(c(Member, `80s_Appearance_Percent`, PPI80s_ebay)) %>% 
  mutate(across(everything(), ~ sub("%$", "", .x))) %>%
  type.convert(as.is = TRUE) %>% 
  mutate(perc_100 = 1) %>% 
  rename('perc' = "80s_Appearance_Percent") %>% 
  mutate(perc = perc / 100) %>% 
  filter(Member %in% c("scottSummers", "jeanGrey", "rachelSummers")) %>% 
  mutate(amount = as.numeric(stringr::str_replace(PPI80s_ebay, "[$,]", ""))) %>% 
  mutate(name = case_when(Member == "scottSummers" ~ "Scott Summers", 
                         Member == "jeanGrey" ~ "Jean Grey",
                         Member == "rachelSummers" ~ "Rachel Summers")) %>%
  arrange(desc(perc)) %>% 
  mutate(x = cumsum(amount) - 0.5 * amount)


df_90 <- mutant_moneyball %>%
  select(c(Member,`90s_Appearance_Percent`, PPI90s_ebay)) %>%
  mutate(across(everything(), ~ sub("%$", "", .x))) %>%
  type.convert(as.is = TRUE) %>%
  mutate(perc_100 = 1) %>%
  rename( 'perc' = "90s_Appearance_Percent") %>%
  mutate(perc = perc / 100) %>%
  filter(Member %in% c("scottSummers", "jeanGrey", "rachelSummers")) %>%
  mutate(amount = as.numeric(stringr::str_replace(PPI90s_ebay, "[$,]", ""))) %>%
  mutate(name = case_when(Member == "scottSummers" ~ "Scott Summers",
                          Member == "jeanGrey" ~ "Jean Grey",
                          Member == "rachelSummers" ~ "Rachel Summers")) %>%
  arrange(desc(perc)) %>%
  mutate(x = cumsum(amount) - 0.5 * amount)

df <- data.frame(x = 45, y = .40)

# create plot -------------------------------------------------------------
p1 <- df_80 %>% 
  ggplot() +
  geom_hline(yintercept = df_80$perc, linewidth = 0.5, linetype = "dotted", color = "#000000") +
  geom_tile(aes(x = x, y = 0.5 * perc, width = amount, height = perc), colour = "#FFFFFF", fill = "#000000") +
  geom_text(aes(x = x, y = perc, label = scales::dollar(amount)), hjust = 0.5, vjust = -0.45, family = font2, size = 5, color = "#168118") +
  geom_text(aes(x = x, y = 0.5 * perc, label = name, angle = ifelse(amount < 20, 90, 0)), family = font2, size = 3.5, color = "#FFFFFF", fontface = "bold") +
  annotate("richtext", x = 0, y = 0.7, label = "Family <span style = 'color: #168118;'>Values</span> over two Decades", family = font2, size = 8.5, color = "#000000", hjust = 0, label.color = NA, label.padding = unit(0, "lines")) +
  annotate("text", x = 0.1, y = 0.62, label = "The average price per comic issue for these X-Men members (calculated\nusing historical eBay sales data from 2022), compared to the percentage\nof issues the member appeared during that decade.", family = font, size = 2.75, color = "#000000", hjust = 0) +
  scale_y_continuous(expand = c(0, 0), breaks = df_80$perc, limits = c(NA, 0.7), labels = scales::percent_format(accuracy = 1L)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.caption = element_markdown(family = font, hjust = 0.5, size = 7, color = "#000000", margin = margin(t = 35)),
        plot.caption.position = "plot",
        legend.position = "none",
        axis.line.x = element_line(linewidth = 0.5, color = "#000000", lineend = "square"),
        axis.text.y = element_text(family = font, color = "#000000", size = 8, margin = margin(r = 5))) +
  labs(caption = "<span style = 'font-family:Bangers; font-size:12pt;'>1980s</span> - issues published between 1980 to 1989")

p2 <- df_90 %>%
  ggplot() +
  geom_hline(yintercept = df_90$perc, linewidth = 0.5, linetype = "dotted", color = "#000000") +
  geom_tile(aes(x = x, y = 0.5 * perc, width = amount, height = perc), colour = "#FFFFFF", fill = "#000000") +
  geom_text(aes(x = x, y = perc, label = scales::dollar(amount)), hjust = 0.5, vjust = -0.45, family = font2, size = 5, color = "#168118") +
  geom_text(aes(x = x, y = 0.5 * perc, label = name, angle = ifelse(amount < 20, 90, 0)), family = font2, size = 3.5, color = "#FFFFFF", fontface = "bold") +
  geom_point_svg(data = df, aes(x = x, y = y), svg = svg_txt, size = 35) +
  annotate("text", x = 45, y = 0.415, label = "Rachel is the\ndaughter of\nScott and Jean", family = font2, size = 3.25, color = "#000000", hjust = "center") +
  scale_y_continuous(expand = c(0, 0), breaks = df_90$perc, limits = c(NA, 0.7), labels = scales::percent_format(accuracy = 1L)) +
  theme_void() +
  theme(plot.caption = element_markdown(family = font, hjust = 0.5, size = 7, color = "#000000", margin = margin(t = 50)),
        plot.caption.position = "plot",
        legend.position = "none",
        axis.line.x = element_line(linewidth = 0.5, color = "#000000", lineend = "square"),
        axis.text.y = element_text(family = font, color = "#000000", size = 8, margin = margin(r = 5))) +
  labs(caption = "<span style = 'font-family:Bangers; font-size:12pt;'>1990s</span> - issues published between 1990 to 1992")


# create combined plot ----------------------------------------------------
final <- p1 + p2

final + plot_layout(widths =c(5, 5)) + plot_annotation(caption = "#TidyTuesday | Data: Rally's Mutant Moneyball | Design: Ryan Hart") &
  theme(plot.margin = margin(1, 0.5, 1, 0.5,"cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.caption = element_markdown(size = 8, hjust = 0.5, family = font, color = "#000000", margin = margin(t = 10)))

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("xmen_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 10, height = 6)


