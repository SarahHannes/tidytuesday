#-----
# 2021 Week 15 TidyTuesday
# Author: Sarah H
# Date: 10 Apr 2021
#-----

# Load libraries
extrafont::loadfonts(device = "win", quiet=T)
library(tidyverse)
library(ggrepel)
library("wesanderson")

# Load data
vegetable_oil <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/vegetable_oil.csv')

# Palettes and font
annos_col <- 'darkgray'
bgcol <- "#fafafc"
pal <- c(wes_palettes$GrandBudapest2[1],wes_palettes$GrandBudapest2[2])
font <- 'Bahnschrift'

# Pre-processing
palm <- vegetable_oil %>%
  group_by(year, entity, crop_oil) %>%
  filter(entity %in% c('Malaysia', 'Indonesia'), crop_oil=='Palm') %>%
  mutate(total = sum(production, na.rm = T)) %>%
  slice(1L) %>%
  summarise(total) %>%
  ungroup()

# Plot
p1 <- ggplot() +
  geom_line(data=palm, aes(x=year, y=total, color=entity), size=1) +
  
  # Highlight 1997
  geom_rect(aes(xmin=1997, xmax=1998, ymin=-Inf, ymax=Inf), fill='gray', alpha=0.2) +
  geom_text_repel(aes(x=1997, y=25000000, label='1997 Asian\nFinancial Crisis'),
                  nudge_x = -3,
                  box.padding = 0.5,
                  nudge_y = 1,
                  hjust=1,
                  min.segment.length = 0,
                  segment.curvature = -1,
                  segment.ncp = 8,
                  segment.angle = 30,
                  lineheight=0.6,
                  family=font,
                  color=annos_col,
                  alpha=0.3) +

  # Annotate 1961
  geom_point(data=palm %>% filter(year==1961), aes(x=1961, y=total, color=entity), size=1.7) +
  geom_text(data=palm %>% filter(year==1961), aes(x=1961, y=total+1000000, label=year), color=annos_col) +
   
  # Annotate 1998
  geom_point(data=palm %>% filter(year==1998), aes(x=1998, y=total, color=entity), size=1.7) +
  geom_text_repel(data=palm %>% filter(year==1998, entity=='Malaysia'), aes(x=1998, y=total, label=paste0('1998\n',entity,'\n',scales::label_number_si(accuracy=0.1)(total)), color=entity), 
                  box.padding = 0.5,
                  max.overlaps = Inf,
                  min.segment.length = 0, # always draw segment with text
                  force=1,
                  nudge_x=-5,
                  nudge_y = 1000000,
                  seed=1, # to get the same position each time
                  hjust=1,
                  lineheight=0.7,
                  segment.linetype = 2, # dotted segment
                  family=font) +
  geom_text_repel(data=palm %>% filter(year==1998, entity=='Indonesia'), aes(x=1998, y=total, label=paste0('1998\n',entity,'\n',scales::label_number_si(accuracy=0.1)(total)), color=entity),
                  box.padding = 0.5,
                  max.overlaps = Inf,
                  min.segment.length = 0,
                  force=1,
                  nudge_x=2,
                  nudge_y = -1500000,
                  seed=1,
                  hjust=0,
                  lineheight=0.7,
                  segment.linetype = 2,
                  family=font) +
  
  # Annotate 2006
  geom_point(data=palm %>% filter(year==2006), aes(x=2006, y=total, color=entity), size=1.7) +
  geom_text_repel(data=palm %>% filter(year==2006), aes(x=2006, y=total, label=paste0('2006\n', entity,'\n',scales::label_number_si(accuracy=0.1)(total)), color=entity),
                  box.padding = 0.5,
                  max.overlaps = Inf,
                  min.segment.length = 0,
                  force=1,
                  nudge_x=-3,
                  nudge_y = 1000000,
                  seed=1,
                  hjust=1,
                  lineheight=0.7,
                  family=font,
                  segment.linetype = 2) +
 
  # Annotate 2014
  geom_point(data=palm %>% filter(year==2014), aes(x=2014, y=total, color=entity), size=1.7) +
  geom_text_repel(data=palm %>% filter(year==2014), aes(x=2014, y=total, label=paste0('2014\n', entity,'\n',scales::label_number_si(accuracy=0.1)(total)), color=entity),
                  box.padding = 0.5,
                  max.overlaps = Inf,
                  min.segment.length = Inf, # Never draw segment with text
                  force_pull=1,
                  nudge_x=1.5,
                  nudge_y = 100,
                  seed=1,
                  hjust=0,
                  lineheight=0.7,
                  family=font,
                  segment.linetype = 2) + 
  
  # Additional annotations 1998 and 2006
  annotate("text", x = 1975, y = 10000000, label = "Malaysia suffered a dip in 1998", size=3, family=font, color=annos_col) +
  geom_curve(aes(x=1975, xend=1998, y=11000000, yend=10000000), size=0.5, curvature = -0.5, arrow = arrow(length = unit(0.1, "inches")),
             lineend = "round",
             alpha=0.3,
             color=annos_col) +
  annotate("text", x = 2008, y = 10000000,
           label = str_wrap("Indonesia exceeded Malaysia in palm oil production. This could be due to the increase in independent small-holder plots in response to a series of policy provided by the Indonesian Government for the development of 'community plantations'. Resulting in a total of 5.45 M hectares of palm oil cultivation area.", 35),
           hjust=0, vjust=1, size=3, family=font, color=annos_col) +
  geom_segment(aes(x=2015, xend=2006, y=10000000, yend=15300000),
               size=0.5,
               arrow = arrow(length = unit(0.1, "inches")),
               lineend = "round",
               alpha=0.3,
               color=annos_col) +
  
  scale_color_manual(values = pal) +
  scale_y_continuous(limits = c(0,max(palm$total)+80000), labels = scales::comma) +
  scale_x_continuous(limits = c(1960,2025), breaks = seq(1960, 2015, 15))+
  theme_void(base_family = 'Bahnschrift') +
  labs(title=str_wrap("Palm Oil Production (M Tonnes) by Top 2 World Leading Producers",33),
       caption="Data: Our World in Data | @saraahannes") +
  theme(
    plot.title = element_text(color='darkgray', size=25),
    plot.caption = element_text(hjust = 1, vjust = 1, size=8, color=annos_col, margin = margin(1, 0, 0.3, 0, "cm")),
    legend.position='none'
  )

# Save plot
ggsave("2021_wk15_deforestation.png", plot = p1, type = 'cairo', width = 8, height = 6, dpi = 300, units = "in", bg =bgcol)
