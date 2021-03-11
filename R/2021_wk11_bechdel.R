# To update Rstudio version
installr: install.packages("installr")
library(installr)
updateR()

# Import Libraries --------------------------
extrafont::loadfonts(device = "win") # To use custom font
library(tidyverse)
library(ggplot2)
library(gggibbous)
library(ggthemes)
library(ggtext)
library(maps)
library(mapproj)

# Load Data --------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 11)
bechdel <- tuesdata$raw_bechdel
movie <- tuesdata$movies

# Data Pre-processing --------------------------

# group by year, round up to decades
moonmovie <- movie %>%
  dplyr::distinct(imdb, .keep_all=TRUE) %>%
  dplyr::mutate(year2=plyr::round_any(year, 10, ceiling)) %>%
  dplyr::group_by(year2, binary) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year2) %>%
  dplyr::mutate(totalyr=sum(n), prop=n/totalyr)%>%
  dplyr::ungroup() %>%
  dplyr::filter(year2>1970, year2<2020) %>%
  as.data.frame() %>%
  identity()

# group by countries, select only top 5 countries
byyrcountry <- movie %>%
  separate(country, into = 'country1', sep=",", extra = 'drop', remove = FALSE) %>% # split column by comma and get only the first country
  dplyr::filter(!is.na(country1)) %>%
  dplyr::mutate(year2=plyr::round_any(year, 10, ceiling)) %>%
  dplyr::group_by(year2, country1, binary) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year2, country1) %>%
  dplyr::mutate(totalyrc=sum(n), prop=n/totalyrc)%>%
  dplyr::ungroup() %>%
  dplyr::filter(year2 >= 1980, country1 %in% c('USA', 'UK', 'Canada', 'France', 'Germany')) %>%
  as.data.frame() %>%
  identity()

# Select top 5 countries
top5country <- c('USA', 'UK', 'Canada', 'France', 'Germany')

# Retrieve map data for top 5 countries
map1 <- map_data(map='world', region=top5country)

# Choose centroid for each country (for labelling)
regionmap1 <- map1 %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(long = mean(long), lat = mean(lat))

# Combine regionmap1 with byyrcountry 
joinyrmap <- merge(x=byyrcountry, y=regionmap1, by.x='country1', by.y='region')

# Additional annotation
bechdel_info <- tibble(
  label = c(
    "<span style='color:darkgray'>Bechdel test is created by Alison Bechdel in 1985
    as a measure to evaluate the representation of women in fiction.
    <br>In order to pass the test, a work must *feature at least two women*,
    these women must *talk to each other*,
    <br>and their conversation must *concern something other than a man*.
    <br>
    <br>This essentially helps open up conversations on the
    <br><span style='color:#EDCC8B'>**portrayal of females**</span> not only in fictional works
    <br>but also <span style='color:#EDCC8B'>**in the society** </span>as a whole.</span><br>"
  ))

# Plot --------------------------

# First Plot
p1 <- ggplot() +
  # moons for all 4 decades
  geom_moon(data=moonmovie, aes(x=year2, y=-0.4, ratio=prop, size=totalyr), fill = "darkgray", color = "darkgray", right=TRUE) +
  geom_moon(data=moonmovie, aes(x=year2, y=-0.4, ratio=prop, size=totalyr), fill = "gold", color = "gold", right = FALSE) +
  
  # year and % labels
  geom_text(data=moonmovie, aes(x=year2, y=0.3, label=year2), size=4, hjust=0.5, vjust=0.5, color='darkgray') +
  geom_text(data=moonmovie %>% dplyr::filter(binary=='PASS'), aes(x=year2, y=0, label=prettyNum(scales::percent(prop,accuracy=1)), size=n), hjust=0.5, vjust=0.5, color='#EDCC8B') +
  
  scale_size_continuous(range=c(5,10)) +
  coord_cartesian(xlim=c(1975,2015), ylim=c(-1, 1)) +
  labs(title="<span style='color:darkgray'>On a Global Scale, <span style='color:#EDCC8B'>Gender Neutrality </span>Awareness had significantly improved...</span>", subtitle="<span style='color:darkgray'>As the % of films </span><span style='color:#EDCC8B'>**Passing**</span><span style='color:darkgray'> the Bechdel Test continue to increased in the last 4 decades</span>", x='', y='') +
  theme_void(base_family = "Bahnschrift", base_size = 14) +
  theme(
    legend.position = 'none',
    plot.title = element_markdown(vjust=-100, hjust=0.5), # to enable markdown formatting in labs
    plot.subtitle = element_markdown(hjust=0.5),
    plot.background = element_rect(fill='#F4EEED', color='#F4EEED'),
    panel.background = element_rect(fill='#F4EEED', color='#F4EEED'),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Second Plot
p2 <- ggplot() +
  # base map
  geom_polygon(data=map1, aes(x=long, y=lat, group=group, fill=region)) +
 
  # labeling for Germany, Canada
  geom_point(data=regionmap1 %>% dplyr::filter(region %in% c('Germany', 'Canada')), aes(x=long, y=lat), size = 0.75, color = "gray20") +
  geom_text(data=joinyrmap %>% dplyr::filter(year2==2020, country1 %in% c('Germany', 'Canada')), aes(x=long+28, y=lat+10, label=country1), size=3, color='darkgray') +
  geom_text(data=joinyrmap %>% dplyr::filter(year2==2020, binary=='PASS', country1 %in% c('Germany', 'Canada')), aes(x=long+28, y=lat+5, label=prettyNum(scales::percent(prop,accuracy=1))), size=3,  color='#EDCC8B',  hjust=0.5, vjust=0.5, fontface='bold') +
  geom_segment(data=regionmap1 %>% dplyr::filter(region %in% c('Germany', 'Canada')), aes(x=long+20, xend=long, y=lat+8, yend=lat), color = "darkgray") +
  
  # labeling for UK, USA, France
  geom_point(data=regionmap1 %>% dplyr::filter(region %in% c('UK', 'USA', 'France')), aes(x=long, y=lat), size = 0.75, color = "gray20") +
  geom_text(data=joinyrmap %>% dplyr::filter(year2==2020, country1 %in% c('UK', 'USA', 'France')), aes(x=long-10, y=lat-10, label=country1), size=3, color='darkgray') +
  geom_text(data=joinyrmap %>% dplyr::filter(year2==2020, binary=='PASS', country1 %in% c('UK', 'USA', 'France')), aes(x=long-10, y=lat-15, label=prettyNum(scales::percent(prop,accuracy=1))), size=3,  color='#EDCC8B', hjust=0.5, vjust=0.5, fontface='bold') +
  geom_segment(data=regionmap1 %>% dplyr::filter(region %in% c('UK', 'USA', 'France')), aes(x=long-8, xend=long, y=lat-8, yend=lat), color = "darkgray") +
  
  # moon for UK, USA, France
  geom_moon(data=joinyrmap %>% dplyr::filter(year2==2020, country1 %in% c('UK', 'USA', 'France')), aes(x=long-30, y=lat-10, ratio=prop, size=totalyrc), fill = "darkgray", color = "darkgray", right=TRUE) +
  geom_moon(data=joinyrmap %>% dplyr::filter(year2==2020, country1 %in% c('UK', 'USA', 'France')), aes(x=long-30, y=lat-10, ratio=prop, size=totalyrc), fill = "gold", color = "gold", right = FALSE) +
  
  # moon for Canada
  geom_moon(data=joinyrmap %>% dplyr::filter(year2==2020, country1 %in% c('Canada')), aes(x=long+45, y=lat+10, ratio=prop, size=totalyrc), fill = "darkgray", color = "darkgray", right=TRUE) +
  geom_moon(data=joinyrmap %>% dplyr::filter(year2==2020, country1 %in% c('Canada')), aes(x=long+45, y=lat+10, ratio=prop, size=totalyrc), fill = "gold", color = "gold", right = FALSE) +
  
  # moon for Germany
  geom_moon(data=joinyrmap %>% dplyr::filter(year2==2020, country1 %in% c('Germany')), aes(x=long+45, y=lat+10, ratio=prop, size=totalyrc), fill = "darkgray", color = "darkgray", right=TRUE) +
  geom_moon(data=joinyrmap %>% dplyr::filter(year2==2020, country1 %in% c('Germany')), aes(x=long+45, y=lat+10, ratio=prop, size=totalyrc), fill = "gold", color = "gold", right = FALSE) +

  # add annotation
  geom_richtext(data=bechdel_info, aes(x=200, y=100, label=label), size=3, position='identity', hjust=1, vjust=1, fill='transparent', color='transparent') +
  
  scale_fill_grey() +
  scale_size_continuous(range=c(5,18)) +
  coord_cartesian(expand=TRUE) +
  labs(title="<span style='color:darkgray'>On a Country-specific Scale,<span style='color:#EDCC8B'> Germany</span> had scored<span style='color:#EDCC8B'> #1 on Bechdel Test</span><br>despite its low film productions in 2020...</span>",
       subtitle="<span style='color:darkgray'>Plot showing Top 5 Film Production Countries and their respective passing % in 2020</span>")+
  theme_void(base_family = "Bahnschrift", base_size = 14) +
  theme(
    legend.position = 'none',
    plot.title = element_markdown(vjust=-100, hjust=0.5), # to enable markdown formatting in labs
    plot.subtitle = element_markdown(hjust=0.5),
    plot.background = element_rect(fill='#F4EEED', color='#F4EEED'),
    panel.background = element_rect(fill='#F4EEED', color='#F4EEED'),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Combine plot --------------------------
plot <- p1 + p2 + plot_layout(heights = c(1,2))
plot_final <- plot + plot_annotation(
  caption = "#TidyTuesday<br>Data: FiveThirtyEight<br>Plot: Twitter @saraahannes| Git @SarahHannes",
  theme = theme(plot.caption = element_markdown(family = "Bahnschrift", colour = "gray60", size=5))) &
  theme(rect = element_rect(fill = "transparent", color='transparent'))

# Save plot --------------------------
ggsave("2021_wk11_bechdel.png", plot = plot_final, type = 'cairo', width = 9, height = 7, dpi = 300, units = "in", bg = "#F4EEED")
