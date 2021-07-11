#-----
# 2021 Week 28 TidyTuesday
# Author: Sarah H
# Date: 11 Jul 2021
#-----

# Load libraries ------------------------------------------
extrafont::loadfonts(device = "win", quiet=T)
library(tidyverse)
library(stringr)
library(patchwork)
library("wesanderson")
library(ggtext)
library(showtext)
showtext_auto()
font_add_google("Bebas Neue", "Bebas Neue")
font_add_google("Signika", "Signika")
font_add_google("Oswald", "Oswald")
font_add_google("Raleway", "Raleway")

# Load data ------------------------------------------
holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv')

# Webscraping exercise to get asian countries as inspired by the provided cleaning script ;) ------------------------------------------
url <- 'https://www.worldometers.info/geography/how-many-countries-in-asia/'
url_bow <- polite::bow(url)

table_list <- polite::scrape(url_bow) %>%
  rvest::html_nodes('table') %>%
  rvest::html_table(fill=TRUE)

asia <- table_list[[1]] %>%
  janitor::clean_names() %>%
  select(country)

# Cleaning ------------------------------------------

# get duplicated countries
dups_country <- holidays$country[duplicated(holidays$country)]

no_na <- holidays %>%
  filter(!is.na(year_of_event)) %>% # remove NAs in year_of_event col
  group_by(country) %>%
  mutate(year = if_else(country %in% dups_country, min(year), year)) %>%
  distinct(country, .keep_all = TRUE) %>% # get distinct rows for all country
  ungroup() %>%
  mutate(century_ceil = year - (year %% 100)) # get century

ind_asia <- no_na %>%
  filter(country %in% asia$country) %>% # filter for Asian countries only
  filter(!is.na(independence_from)) %>% # remove NAs in independence_from col
  mutate(independence_from=str_replace_all(independence_from, '[^a-zA-Z|\\s]', '')) %>% # remove anything other than alphabets and space
  mutate(independence_from=if_else(independence_from=='United Kingdom and the British Mandate for Palestine', 'United Kingdom', independence_from)) # renaming

title <- tibble(
  label=c("<span style='font-size:40pt; color:darkgray'>
          YEAR OF</span>
          <br><span style='font-size:50pt; color:black'>
          INDEPENDENCE ACROSS ASIAN COUNTRIES</span>
          <span style='font-size:40pt; color:darkgray'><br>FROM</span>
          </style>
          "))

legend <- ind_asia %>%
  add_count(independence_from) %>%
  group_by(independence_from) %>%
  slice(1L) %>%
  summarise(n) %>%
  ungroup() %>%
  mutate(rank = rank(-n, ties='random')) # rank by desc(n), and break ties arbitrarily

# Palettes and font ---------------------------------------

label_pal <- c('#FCEFEF', '#F5FBEF', 'black', 'white', "#696969")
bg_pal <- c('#fcf7f4')
font <- c('Bahnschrift', 'Bebas Neue', 'Signika', 'Oswald', 'Raleway')

# get distinct count of independence_from to expand pallete
colourCount <- length(unique(ind_asia$independence_from))


# Plots ---------------------------------------

# bottom plot
p2 <- ggplot() +

  # columns
  geom_col(data=ind_asia, aes(x=year, y=reorder(country, year), fill=factor(independence_from)), color=bg_pal, alpha=0.8, width = 1, position = position_dodge(0.6)) +
  
  # 2021 segment & label
  geom_segment(data=NULL, aes(x=2021, xend=2021, y=0.5, yend=30), color=label_pal[5], linetype='dashed', size=0.3, alpha=0.5) +
  geom_text(data=NULL, aes(x=2021, y=31, label='2021'), size=15, family=font[5], color=label_pal[5]) +
  
  # column labels with some shadowing ;)
  geom_text(data=ind_asia, aes(x=year-2, y=reorder(country, year), label=paste0(year, " ", country)), hjust=1, size=12, family=font[2], color=label_pal[4], fontface='bold') +
  geom_text(data=ind_asia, aes(x=year-1, y=reorder(country, year), label=paste0(year, " ", country)), hjust=1, size=12, family=font[2], color=label_pal[3], fontface='bold', alpha=0.8) +
  
  scale_fill_manual(values = colorRampPalette(wes_palette("Royal2"))(colourCount), name='Independence From') +  # expand discrete colour palettes up to colourCount value
  scale_y_discrete(limits = rev) + # reverse discrete axis
  labs(
    caption="#TidyTuesday    |    Data: Wikipedia    |    @saraahannes</span>") +
   
  coord_cartesian(xlim=c(min(ind_asia$year)-100, max(ind_asia$year)+100), ylim = c(-1, 39)) +
  theme_void(base_family=font[1]) +
  theme(
    legend.position="none",
    plot.caption = element_markdown(color=label_pal[5], family=font[5], hjust = 0.5, vjust = 0.5, size=28, margin = margin(0, 0, 0.3, 0, "cm")),
    plot.background = element_rect(fill=bg_pal[1], color=bg_pal[1]),
    plot.margin = margin(-2, 0, 0, 0, "cm"),
    panel.background = element_rect(fill=bg_pal[1], color=bg_pal[1])
    )

# legend plot
p1 <- ggplot() +

  # legend point and label, just because circles are prettier than squares :)
  geom_point(data=legend, aes(x=rank, y=0.1, color=independence_from), size=10, alpha=0.8) +
  geom_text(data=legend, aes(x=rank, y=-0.4, label=str_wrap(independence_from, 10)), lineheight=0.3, family=font[5], size=8, color=label_pal[5], vjust=1) +
  
  # title
  geom_richtext(data=title, aes(x=7.25, y=1.8, label=str_squish(label)), hjust=0.5, vjust=0.5, family=font[4], lineheight=0.5,  color=NA, fill=NA, size=13) +
  
  scale_color_manual(values = colorRampPalette(wes_palette("Royal2"))(colourCount), name='Independence From') + 
  scale_x_continuous(limits = c(1,13)) +
  coord_cartesian(ylim=c(-2, 3)) +
  theme_void() +
  theme(
    legend.position = 'none',
    plot.background = element_rect(fill=bg_pal[1], color=bg_pal[1]),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.background = element_rect(fill=bg_pal[1], color=bg_pal[1])
  )

# merge plot
plot <- p1 + p2 + plot_layout(heights = c(2,5))

# save plot
ggsave('2021_wk28_independence.png', plot=plot, type = 'cairo', width = 9, height = 7, dpi = 300, units = "in", bg = bg_pal[1])
