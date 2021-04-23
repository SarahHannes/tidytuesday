#-----
# 2021 Week 13 TidyTuesday
# Author: Sarah H
# Date: 27 Mar 2021
#-----

# Import libraries
extrafont::loadfonts(device = "win", quiet=T) # do this everytime before loading ggplot
library(tidyverse)
library(maps)
library(mapproj)
library(ggtext)
library(magick)
library(showtext)
showtext_auto()
font_add_google("Signika", "Signika")
font_add_google("Roboto", "roboto")

# Load data
unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')

# Preprocessing
tea_all <- unvotes %>%
  filter(rcid==9085) %>%
  mutate(
    vote=factor(vote, level=c("yes", "abstain", "no")),
    # mutate some of the countries name to match with library(maps)
    country=ifelse(country=='Bosnia & Herzegovina', 'Bosnia and Herzegovina', country),
    country=ifelse(country=='Czechia', 'Czech Republic', country),
    country=ifelse(country=='North Macedonia', 'Macedonia', country),
    country=ifelse(country=='United Kingdom', 'UK', country),
    country=ifelse(country=='United States', 'USA', country),
    country=ifelse(country=='Antigua & Barbuda', 'Antigua', country),
    country=ifelse(country=='Congo - Brazzaville', 'Democratic Republic of the Congo', country),
    country=ifelse(country=="Côte d'Ivoire", 'Ivory Coast', country),
    country=ifelse(country=="St. Kitts & Nevis", 'Saint Kitts', country),
    country=ifelse(country=='St. Lucia', 'Saint Lucia', country), 
    country=ifelse(country=='St. Vincent & Grenadines', 'Saint Vincent', country), 
    country=ifelse(country=='Trinidad & Tobago', 'Trinidad', country)
  ) %>%
  add_row(country=c("Barbuda", "Nevis", "Grenadines", "Tobago"), vote="yes") %>%
  as.data.frame() %>%
  identity()

# Retrieve map data
mapall <- map_data(map='world', region=tea_all$country)

# Choose centroid for each country (for labelling)
regionmap_all <- mapall %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(long = mean(long), lat = mean(lat))

# Combine votes with map
join_map_all <- left_join(x=tea_all, y=mapall, by=c("country" = "region"))

# Read image file
cup <- image_read("cup_crop.png") %>%
  image_background("none") %>%
  image_fill("#e4dcdc", point = "+100+200", fuzz = 100)

# Info
tea_info <- tibble(
  label = c(
    "<span style='color:darkgray'> 
    <br>Recognizing the long history and deep cultural and economic significance of tea
    <br>around the world, the United Nations General Assembly proclaimed
    <br><span style='color:#f7c704'>21 May</span> as International Tea Day, calling on
    <br>FAO to lead the observance.
    <br>The goal of this day focuses on
    <br>promoting better trade practices and working conditions,
    <br>as well as <span style='color:#f7c704'>ethical</span> and <span style='color:#f7c704'>sustainable</span> production of tea.
    <br>This day also aims to raise awareness on<span style='color:#f7c704'> fighting  hunger</span> and <span style='color:#f7c704'>poverty</span>. 
    <br>
    <br>Take a sip
    </style>"
  ))


p1 <- ggplot() +
  # map
  geom_polygon(data=join_map_all, aes(x=long, y=lat, group=group, fill=vote), color='white') +
  
  # annotation for 'abstain'
  geom_curve(data=regionmap_all %>% filter(region=="Japan"), aes(x=long, xend=long+10, y=lat, yend=lat-8), size=0.5, curvature = -0.3, arrow = arrow(length = unit(0.1, "inches")), lineend = "round", color='gainsboro') +
  geom_text(data=regionmap_all %>% filter(region=="Japan"), aes(x=long+10, y=lat-10, label="no comment"), size=10, color="#6c6c6c", family="Signika") +
  
  # annotation for 'no'
  geom_curve(data=regionmap_all %>% filter(region=="USA"), aes(x=long, xend=long-35, y=lat-10, yend=lat-15), size=0.5, curvature = -0.5, arrow = arrow(length = unit(0.1, "inches")), lineend = "round", color='gainsboro') +
  geom_text(data=regionmap_all %>% filter(region=="USA"), aes(x=long-45, y=lat-10, label="no thanks"), size=10, hjust=0, vjust=0.5, color="#e8acb3", family="Signika") +
  
  # annotation for 'yes'
  geom_curve(data=regionmap_all %>% filter(region=="Brazil"), aes(x=long, xend=long-35, y=lat-10, yend=lat-15), size=0.5, curvature = -0.5, arrow = arrow(length = unit(0.1, "inches")), lineend = "round", color='gainsboro') +
  geom_text(data=regionmap_all %>% filter(region=="Brazil"), aes(x=long-50, y=lat-15, label="yay!"), size=10, hjust=0, vjust=0.5, color="#76ceb0", family="Signika") +

  # cup image
  annotation_raster(cup, xmin = 120, xmax = 140, ymin = -170, ymax = -185) +
  
  # title
  geom_text(aes(x=-180, y=-180, label="INTERNATIONAL\nTEA DAY"), angle=90, hjust=0, size=40, lineheight=0.35, family="Signika", fontface='bold', color='#a8c4cc') +
  # info
  geom_richtext(data=tea_info, aes(x=180, y=-80, label=label), size=13, position='identity', hjust=1, vjust=1, fill='transparent', color='transparent', lineheight=0.4, family="roboto") +
  # caption
  geom_richtext(aes(x=199, y=-192, label="<span style='color:darkgray'>#TidyTuesday<br>Data: Harvard Dataverse<br>Text: fao.org; thebridge.in<br>Plot: Twitter @saraahannes| Git @SarahHannes</span>"), family = "roboto", size=8, fill='transparent', color='transparent', hjust=1, vjust=1, position='identity', lineheight=0.2) +
  
  scale_fill_manual(values=c("gainsboro", "#f4a1aa", "#8dbfa4")) +
  coord_cartesian(xlim=c(-200, 200), ylim=c(-200,100)) +
  labs(title=NULL, x=NULL, y=NULL) +
  theme_nothing(base_family = "Signika", base_size = 14) +
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "null"),
    panel.spacing = unit(c(0, 0, 0, 0), "null"),
    plot.background = element_rect(fill="#fafafc", color="transparent"),
    legend.position = "none"
  )

# Save plot
ggsave("2021_wk13_unvotes.png", plot = p1, type = 'cairo', width = 9, height = 7, dpi = 300, units = "in", bg = "#fcf7f4")
