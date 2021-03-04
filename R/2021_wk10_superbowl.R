#-----
# 2021 Week 10 TidyTuesday
# Author: Sarah H
# Date: 4 Mar 2021
#-----

# Import libraries
library(tidyverse)
install("tidytuesdayR")
library(tidytuesdayR)
library(reshape2) # for melt()
library(ggridges)

# Read data
youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')

# Data Pre-processing
all <- youtube %>%
    dplyr::mutate(year=factor(year)) %>%
    dplyr::group_by(year, brand) %>%
    dplyr::mutate(funny=replace(funny, funny==TRUE, 1)) %>%
    dplyr::mutate(prod=replace(show_product_quickly, show_product_quickly==TRUE, 1)) %>%
    dplyr::mutate(patrc=replace(patriotic, patriotic==TRUE, 1)) %>%
    dplyr::mutate(celeb=replace(celebrity, celebrity==TRUE, 1)) %>%
    dplyr::mutate(danger=replace(danger, danger==TRUE, 1)) %>%
    dplyr::mutate(animals=replace(animals, animals==TRUE, 1)) %>%
    dplyr::mutate(use_sex=replace(use_sex, use_sex==TRUE, 1)) %>%
    dplyr::summarise(sum_like=sum(like_count, na.rm=T), funny, prod, celeb, danger, animals, use_sex) %>%
    dplyr::arrange(desc(sum_like)) %>%    
    dplyr::slice(1L) %>%
    dplyr::slice(which.max(sum_like)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year) %>%
    dplyr::ungroup()

# Group by year, brand then get the max(sum_like) for each year
max_like_10yr <- all %>%
    dplyr::group_by(year) %>%
    dplyr::top_n(n=1, wt=sum_like) %>% # get the top row (after arranged in desc order)-get the max sum_like
    dplyr::ungroup() %>%
    dplyr::arrange(desc(year)) %>%
    dplyr::slice(1:10)

# Melt brands that has max like for the past 10 years into long format
melted10yr <- max_like_10yr %>%
    dplyr::group_by(year, brand) %>%
    dplyr::arrange(desc(year)) %>%
    dplyr::slice(1:10) %>%
    dplyr::summarise(year, funny, prod, celeb, danger, animals, use_sex) %>%
    reshape2::melt() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(label=factor(paste(year, max_like_10yr$brand, sep=" ")))

# Create dummy df for petals in legend
dummy <- melted10yr %>%
    dplyr::mutate(value=replace(value,value==0,1)) %>%
    dplyr::add_row(year=factor(0), brand='Example', variable=c('funny', 'prod', 'celeb', 'danger', 'animals', 'use_sex'), value=c(1,1,1,1,1,1), label='Legend')

# Create dummy df for legend title
dummy_legend <- melted10yr %>%
    dplyr::mutate(value=replace(value,value==0,1)) %>%
    dplyr::add_row(year=factor(0), brand='Example', variable=c('funny', 'product < 10 min', 'celebrity', 'danger', 'animal', 'nudity'), value=c(1,1,1,1,1,1), label='Legend') %>%
    dplyr::filter(year==0) %>%
    identity()

# Create dummy df for legend axis labels
dummy_axis <- dummy %>%
    subset(year==0) %>%
    dplyr::mutate(new_var=variable) %>%
    dplyr::mutate(variable=factor(variable)) %>%
    dplyr::mutate(new_var=replace(new_var, variable=='prod', 'show product\n<10s')) %>%
    dplyr::mutate(new_var=replace(new_var, variable=='celeb', 'celebrity')) %>%
    dplyr::mutate(new_var=replace(new_var, variable=='use_sex', 'nudity'))

# Create df for total features
totalbar <- max_like_10yr %>%
    tidyr::gather("features", "value", 4:9) %>%
    dplyr::mutate(new_var=features, features=factor(features)) %>%
    dplyr::group_by(features) %>%
    dplyr::mutate(total=sum(value)) %>%
    dplyr::slice(1L) %>%
    dplyr::summarise(features, new_var, total) %>%
    dplyr::mutate(new_var=replace(new_var, features=='prod', 'show product\n<10s')) %>%
    dplyr::mutate(new_var=replace(new_var, features=='celeb', 'celebrity')) %>%
    dplyr::mutate(new_var=replace(new_var, features=='use_sex', 'nudity'))

dummy_joined <- merge(dummy_axis, totalbar)

# Plot
options(repr.plot.width=15, repr.plot.height=15)

ggplot() +
    geom_spoke(data=melted10yr, aes(x=variable, y=value, angle=-1, radius=(value*pi), color=variable), group=1) +
    geom_spoke(data=melted10yr, aes(x=variable, y=value, angle=1, radius=(1-value*pi-1), color=variable), group=1) +
    geom_text(data=max_like_10yr, aes(x=-Inf, y=1, label=year, size=sum_like),inherit.aes = FALSE, nudge_y=0.3, nudge_x=0.3) + # label for year
    geom_text(data=max_like_10yr, aes(x=-Inf, y=0.5, label=paste(brand," (",prettyNum(sum_like, big.mark=","),")", sep=""), size=sum_like), inherit.aes = FALSE) + # label for sum_like

    geom_spoke(data=dummy, aes(x=variable, y=value, angle=-1, radius=(value*pi)), color='gray', group=1, alpha=0.2) +
    geom_spoke(data=dummy, aes(x=variable, y=value, angle=1, radius=(1-value*pi-1)),color='gray', group=1, alpha=0.2) +
    geom_spoke(data=dummy, aes(x=variable, y=value, angle=1, radius=(1-value*pi-1)),color='gray', group=1, alpha=0.2) +
    geom_text(data=dummy_legend, aes(x=-Inf, y=1, label='Ads features'), inherit.aes=FALSE, color='dim gray', size=6) + # label for legend      
    geom_text(data=dummy_joined, aes(x=variable, y=value, label=new_var, color=variable, size=total^6), nudge_x=0.4, nudge_y=0.1, check_overlap=FALSE) + # label for each petals

    scale_size_continuous(range=c(5,10)) +
    scale_color_brewer(palette="Dark2") +
    coord_polar() +
    facet_wrap(reorder(year, desc(year))~., nrow=3) + # plot in separate grid, arrange grid by desc(year)
    labs(title='Top Liked Brands for Super Bowl Ads', subtitle='Highlighted petals indicate features appeared in ads\n(Total Likes in parenthesis)\n', x='', y='', caption='\nData: FiveThirtyEight\nVisualization: (Twitter @saraahannes | Git SarahHannes)') +
    theme_ridges() +
    theme(
        plot.background=element_rect(fill='mint cream'),
        plot.title=element_text(size=30),
        plot.subtitle=element_text(size=25, color='gray42'),
        panel.grid.major=element_blank(),
        legend.position='none',
        strip.text.x = element_blank(), # remove strip label
        axis.title=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank()
    )
