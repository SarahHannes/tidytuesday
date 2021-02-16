## Import Libraries ##
library(tidyverse)
library(dplyr)
# install.packages("tidytuesdayR")
library(tidytuesdayR)
library(cowplot) # for ggdraw() to add drawing layer
library(magick) # for draw_image(), to add image layer to using ggdraw

## Import Data ##
georgia_pop <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/georgia_pop.csv')

## View Data ##
georgia_pop

## Separating indv subsamples ##
popC <- georgia_pop %>%
    dplyr::group_by(Year) %>%
    dplyr::mutate(Year = factor(Year), perc=Colored, race='Colored') %>%
    dplyr::summarise(perc, race) %>%
    dplyr::ungroup()

popW <- georgia_pop %>%
    dplyr::group_by(Year) %>%
    dplyr::mutate(Year = factor(Year), perc=White, race='White') %>%
    dplyr::summarise(perc, race) %>%
    dplyr::ungroup()

## Plot ##
options(repr.plot.width=8,repr.plot.height=10)

final_plot <- ggplot() +
    geom_line(data=popW, aes(x=Year, y=perc, group=1, linetype=race)) +
    geom_line(data=popC, aes(x=Year, y=perc, group=1, linetype=race)) +
    scale_y_reverse(limits = c(100, 0), breaks=seq(100, 0, by=-5), expand=c(0,0)) + # limits: to reverse continuous y axis # breaks=seq: to set axis breaks. by -5 length
    scale_x_discrete(expand=c(0,0)) + # to cut white space before 1790 and after 1890
    scale_linetype_manual(values=c('solid', 'longdash'), name=' ') + # manually change the linetype, name=' ' is to remove legend name
    coord_flip() +
    labs( title= 'COMPARATIVE INCREASE OF WHITE AND COLORED\nPOPULATION OF GEORGIA.', subtitle='',
            x = '', y='PERCENTS') +
    theme_bw() +
    theme(
        legend.key=element_blank(), # remove legendkey background
        legend.background = element_rect(fill = "transparent"), # get rid of legend background
        legend.box.background = element_rect(fill = "transparent", color=NA), # get rid of legend background
        legend.justification=c(0, 0),
        legend.position=c(-0.019, -0.15),
        legend.direction='horizontal',
        legend.text=element_text(margin = margin(r = 0.8, unit = "inch"), size=13, color='gray42'),
        panel.background = element_rect(fill = "transparent"), # remove background of the panel
        panel.grid.minor=element_blank(), # remove minor grid
        panel.grid.major=element_line(color='salmon'),
        plot.margin=unit(c(4,8,5,5), 'lines'),
        plot.title=element_text(vjust=0.5, hjust=0.5, size=15.8, face='bold', family='sans'),
        plot.background = element_rect(fill = "transparent", color = NA), # remove background of the plot
        axis.ticks=element_blank(),
        axis.text=element_text(family='Courier', color='gray42'),
        axis.text.y=element_text(size=13, margin=margin(0,10,0,0)),
        axis.text.x=element_text(size=10),
        axis.title.x=element_text(margin=margin(10,0,20,0), family='Courier', color='gray42')
    ) +
    guides(
        linetype=guide_legend(
                 keywidth=0.8,# adjust width of the legend key
                 keyheight=0,
                 default.unit="inch")
      )

## Read in background image ##
img5 <- image_read('../input/background5/Picture5.png') %>%
    image_resize("1000x1000") %>%
    image_colorize(50, "white")

## Combine plot + background ##
draw <- ggdraw() +
    draw_image(img5) +
    draw_plot(final_plot)

draw
