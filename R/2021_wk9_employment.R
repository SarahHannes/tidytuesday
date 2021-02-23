#-----
# 2021 Week 9 TidyTuesday
# Author: Sarah H
# Date: 23 Feb 2021
#-----

# Import Libraries
library(tidyverse)
install.packages("tidytuesdayR")
library(tidytuesdayR)
library(scales) # for wrap_format

# Read Data
employed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')

# Data Pre-processing
minor_occ <- employed %>%
    dplyr::filter(race_gender == 'Black or African American', year==2020) %>%
    dplyr::group_by(minor_occupation) %>%
    dplyr::mutate(total=sum(employ_n, na.rm=T), minor_occupation=toupper(minor_occupation))  %>% # na.rm=T is to ignore NA when summing
    dplyr::filter(!industry %in% c(NA, 'Men', 'Women', 'Asian', 'Black or African American ')) %>%
    dplyr::summarise(total) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup()

difftop1 <- employed %>%
    dplyr::filter(race_gender == 'Black or African American', year==2020) %>%
    dplyr::group_by(minor_occupation) %>%
    dplyr::mutate(total=sum(employ_n, na.rm=T), minor_occupation=toupper(minor_occupation))  %>% # na.rm=T is to ignore NA when summing
    dplyr::filter(!industry %in% c(NA, 'Men', 'Women', 'Asian', 'Black or African American ')) %>%
    dplyr::summarise(total) %>%
    dplyr::slice(1L) %>%
    dplyr::arrange(desc(total)) %>%
    plyr::mutate(diff = total - lag(total, default=0, order_by=total)) %>% # to get the differences current total - total next row
    dplyr::ungroup() %>%
    dplyr::filter(minor_occupation=='PROFESSIONAL AND RELATED OCCUPATIONS')

# Plot
options(repr.plot.width=30,repr.plot.height=15)

ggplot() +
    geom_col(data=minor_occ, aes(x=reorder(minor_occupation, total), y=total),fill='#dc143c', position = position_dodge(0.1), width=0.3) +
    geom_rect(data=NULL, aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=20000), fill='floralwhite') + # add whitespace between y axis text and plot
    geom_text(data=minor_occ, aes(x=reorder(minor_occupation, total), y=-3, label=prettyNum(total, big.mark=",")), group=1, size=6, hjust=1, vjust=0.5, fontface='bold', family='Courier') +
    geom_segment(data=difftop1, aes(x=10.5, xend=10.5, y=total-diff, yend=total-diff-diff), stat='identity', position=position_dodge(width=1), lineend = "butt", linejoin = "round", size=10, color='#dc143c') + # the diff between top and second top bars
    geom_segment(data=difftop1, aes(x=11, xend=11, y=total-diff, yend=Inf), stat='identity', position=position_dodge(width=1), size=10, color='floralwhite') + # cover the rest of top bar
    geom_curve(data=difftop1, aes(x=10.99, xend=10.499, y=total-diff, yend=total-diff),  lineend="round", size=9, color='#dc143c', curvature=-0.9) + # the curve at the edge of the top bar. yay!
    coord_flip() +
    scale_x_discrete(labels = wrap_format(25), expand=c(0,0)) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.01))) +
    labs(title='OCCUPATIONS OF BLACK OR AFRICAN AMERICAN IN THE USA (2020).', subtitle='MEN AND WOMEN\n', x='', y='', caption='\nInspiration: W. E. B Du Bois\nData: BLS\nVisualization: (Twitter @saraahannes | Git SarahHannes)') +
    theme_void() +
    theme(
        panel.background = element_rect(fill = "floralwhite", color='floralwhite'),
        plot.background=element_rect(fill = "floralwhite", color='floralwhite'),
        plot.margin = unit(c(50,10,25,40), "pt"),
        plot.title=element_text(hjust=0.5, vjust=0.5, face='bold', family='sans', size=30), 
        plot.subtitle=element_text(hjust=0.5, vjust=0.5, color='gray60', face='bold', family='Courier', size=27),
        plot.caption=element_text(size=15, color='gray60', family='Courier'),
        axis.text.y=element_text(hjust=0.5, vjust=0.5, color='gray60', face='bold', family='Courier', size=16)
    )