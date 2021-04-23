#-----
# 2021 Week 16 TidyTuesday
# Author: Sarah H
# Date: 21 Apr 2021
#-----

# Load libraries
extrafont::loadfonts(device = "win", quiet=T)
library(tidyverse)
library(ggmap)
library(ggtext)
library("wesanderson")
library(showtext)
showtext_auto()
font_add_google("Signika", "Signika")
font_add_google("Exo 2", "Exo 2")

# Read data
post_offices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv')

# Pre-processing
by_state <- post_offices %>%
  filter(gnis_feature_class=='Post Office', !is.na(latitude), !is.na(longitude)) %>%
  group_by(state) %>%
  summarise(orig_name, latitude, longitude, established, discontinued) %>%
  ungroup() %>%
  mutate(totalyr=ifelse(is.na(discontinued), 2021-established, 2021-discontinued))

po_alaska <- by_state %>%
  filter(state=='AK')

map_alaska <- map_data(map='world', region='USA') %>%
  filter(subregion=='Alaska', long<100)

# Palettes and font
col <- c('#ce9287', '#3a9cbc')
annoscol <- 'darkgray'
titlecol <- '#a8c4cc'
bgcol <- "#fafafc"
font <- c('Signika', 'Exo 2')

# Title & Subtitles labels
annos <- tibble(
  label=c("<span style='color:darkgray'>
          ALASKA Post Offices", 
          
          "<span style='color:darkgray'>
          <span style='color:#ce9287'>Discontinued</span>
          & <span style='color:#3a9cbc'>Still in operation</span><br>since 1867</span>"
          )
)

# Plot
p1 <- ggplot() +
  # Title
  geom_richtext(data=annos, aes(x=-130, y=49, label=label[1]), hjust=1, vjust=0, size=50, family=font[2], lineheight=0.3, fontface='bold', color=NA, fill=NA, alpha=0.2) +
  geom_richtext(data=annos, aes(x=-130, y=47, label=label[2]), hjust=1, vjust=0, size=20, family=font[1], lineheight=0.3, fontface='italic', color=NA, fill=NA) +
  
  # Base Map
  geom_polygon(data=map_alaska, aes(x=long, y=lat, group=group), fill='gainsboro') +
  geom_count(data=po_alaska, aes(x=longitude, y=latitude, size=totalyr, alpha=totalyr, color=totalyr)) +
  
  # Still in operation
  geom_count(data=po_alaska %>% filter(is.na(discontinued)), aes(x=longitude, y=latitude, size=totalyr, alpha=totalyr), color=col[2]) +
  
  scale_alpha_continuous(range=c(0.09, 0.5), guide='none') +
  scale_size_continuous(range=c(0.1, 7), name='Total Years\nIn Operation', guide='bins') +
  scale_colour_binned(low='white', high=col, guide = "none") +
  theme_void(base_family=font[2]) +
  labs(caption="Data: Cameron Blevins & Richard W. Helbock | @saraahannes") +
  theme(
    legend.position= 'bottom',
    legend.justification = 'right',
    legend.margin = margin(-2,0.3,0,0), 
    legend.key.size = unit(0.1,"line"),
    legend.spacing.x = unit(0.3, 'cm'),
    legend.spacing.y = unit(0.15, 'cm'),
    legend.title = element_text(color=annoscol, size=30, lineheight=0.2, hjust=1, vjust=0.5, face='bold'),
    legend.text = element_text(color=annoscol, size=30),
    plot.caption = element_text(hjust = 1, vjust = 0.5, size=30, color=annoscol, margin = margin(0.5, 1, 0.3, 0, "cm"))
  ) +
  guides(
    size=guide_legend(override.aes = list(color = annoscol))
  )

# Save plot
ggsave("2021_wk16_postoffice.png", plot = p1, type = 'cairo', width = 8.5, height = 6, dpi = 300, units = "in", bg =bgcol)
