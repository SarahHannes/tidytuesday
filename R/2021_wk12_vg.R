# extrafont::font_import() # do this whenever you install new font, then restart r

# Import libraries ---------------------------------
extrafont::loadfonts(device = "win", quiet=T) # do this everytime before loading ggplot
library(tidyverse)
library(ggalt)
#remotes::install_github("davidsjoberg/ggstream")
library(ggstream)
library(magick)
library(showtext)
showtext_auto()
font_add_google("Press Start 2P", "press") # add custom font from google font

# Load data ---------------------------------
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

# Pre-processing ---------------------------------
yrgame <- games %>%
  filter(gamename %in% c("Dota 2", "Counter-Strike: Global Offensive", "PLAYERUNKNOWN'S BATTLEGROUNDS", "Team Fortress 2", "Grand Theft Auto V")) %>%
  mutate(month=factor(month, levels=c("January", "February", "March", "April","May", "June","July", "August", "September", "October", "November", "December"), ordered = T),
        monyr=paste0(month," ", year),
        monyr=zoo::as.yearmon(monyr),
        gamename = factor(gamename, levels = c("Dota 2", "Counter-Strike: Global Offensive", "PLAYERUNKNOWN'S BATTLEGROUNDS", "Team Fortress 2", "Grand Theft Auto V"), ordered = T)
                ) %>%
  group_by(monyr, gamename) %>%
  summarise(month, year, avg) %>%
  ungroup() %>%
  as.data.frame() %>%
  identity()

totalavg <- yrgame %>%
  group_by(gamename) %>%
  mutate(total=sum(avg)) %>%
  summarise(total) %>%
  slice(1L) %>%
  ungroup() %>%
  as.data.frame()


released <- data.frame(row_names=c("Dota 2", "Counter-Strike: Global Offensive", "PLAYERUNKNOWN'S BATTLEGROUNDS", "Team Fortress 2", "Grand Theft Auto V"), val=c("July 9 2013", "August 21 2012", "March 23 2017", "October 10 2007", "September 17 2013")) %>%
  mutate(rel_date = as.Date(val, format = "%B %d %Y"),
         rel_date = zoo::as.yearmon(rel_date))

merged_rel_total <- merge(totalavg, released, by.x="gamename", by.y="row_names")

green <- matrix(hcl(seq(120, 100, length.out = 33), 80, 70), nrow = 3)

mario3 <- image_read("mario_blank.png") %>%
  image_transparent(color='red') %>%
  image_background("none") %>%
  image_rotate(-15) %>%
  image_resize("70x70")

# Plot ---------------------------------
p <- ggplot() +
  
  # Mario
  annotation_raster(mario3, xmin = 2010, xmax = 2010.9, ymin = 700000, ymax = -100000) +
  
  # dialogue box (using geom_paths just to get the outer black border with rounded corners lol)
  geom_polygon(aes(x=c(2012, 2019, 2019, 2012), y=c(2100000, 2100000, 3500000, 3500000)), color="#fff8e8", fill='#1a79bb', size=3,  inherit.aes = F) +
  geom_path(aes(x=c(2011.99, 2019.02, 2019.02, 2011.99), y=c(2050000, 2050000, 3550000, 3550000)), color='black', lineend='round', size=2,  inherit.aes = F) +
  geom_path(aes(x=c(2011.99, 2011.99), y=c(2050000, 3550000)), color='black', lineend='round', size=2,  inherit.aes = F) +
  # dialogue text
  geom_text(aes(x=2012.2, y=3000000, label="When Does Video Games Gained Traction ?\nSee Top 5 Games with\nMost Concurrent Players\nfrom 2012 ~ 2021 below ...\n"), color="#fff8e8", family="press", hjust=0, vjust=0.8, size=5) +
  
  # bottom warp pipe (without fill, just black border)
  geom_polygon(aes(x=c(2010.2, 2011.3, 2011.3, 2010.2), y=c(-Inf, -Inf, -1000000,-1000000)), color='black', fill='NA', size=1.2,  inherit.aes = F) +
  # bottom warp pipe (with gradient fill) because I dont know how to add border directly here lol
  annotation_raster(green, 2010.21, 2011.28, -Inf, -999999, interpolate = F) +
  # top warp pipe
  geom_polygon(aes(x=c(2010, 2011.5, 2011.5, 2010), y=c(-1000000,-1000000, -650000,-650000 )), color='black', fill='#90d441', size=2.3,  inherit.aes = F) +
  
  # main stream plot
  geom_stream(data=yrgame, aes(x=monyr, y=avg, fill=gamename, group=gamename), true_range = "both", color="#fff8e8", size=0.35) +
  
  # Dota annotations
  geom_segment(data=merged_rel_total %>% filter(gamename == "Dota 2"), aes(x=rel_date, xend=rel_date, y=-80000, yend=total/30), color="#b2fc20") +
  geom_point(data=merged_rel_total %>% filter(gamename == "Dota 2"), aes(x=rel_date, y=total/30), color="#b2fc20") +
  geom_text(data=merged_rel_total %>% filter(gamename == "Dota 2"), aes(x=rel_date, y=total/30+180000, label=paste0("#1\n", gamename, "\nreleased on ", rel_date)), color="#343434", size=3, hjust=1, vjust=0.5, family="press") +
  
  # Counterstrike annotations
  geom_segment(data=merged_rel_total %>% filter(gamename == "Counter-Strike: Global Offensive"), aes(x=rel_date, xend=rel_date, y=-20000, yend=total/30), color="#0adf13") +
  geom_point(data=merged_rel_total %>% filter(gamename == "Counter-Strike: Global Offensive"), aes(x=rel_date, y=total/30), color="#0adf13") +
  geom_text(data=merged_rel_total %>% filter(gamename == "Counter-Strike: Global Offensive"), aes(x=rel_date, y=total/30+180000, label=paste0("#2\n", gamename, "\nreleased on ", rel_date)), color="#343434", size=3, hjust=1, vjust=0.5, family="press") +
  
  # Playerunkowns annotations
  geom_segment(data=merged_rel_total %>% filter(gamename=="PLAYERUNKNOWN'S BATTLEGROUNDS"), aes(x=rel_date, xend=rel_date, y=-500000, yend=-800000), inherit.aes = F, color="#366700") +
  geom_point(data=merged_rel_total %>% filter(gamename=="PLAYERUNKNOWN'S BATTLEGROUNDS"), aes(x=rel_date, y=-800000), color="#366700") +
  geom_text(data=merged_rel_total %>% filter(gamename=="PLAYERUNKNOWN'S BATTLEGROUNDS"), aes(x=rel_date, y=-1000000, label=paste0("#3\n", gamename, "\nreleased on ", rel_date)), color="#343434", size=3, hjust=1, vjust=0.5, family="press") +
  
  # Team Fortress annotations and dotted line
  geom_segment(data=merged_rel_total %>% filter(gamename=="Team Fortress 2"), aes(x=zoo::as.yearmon("2012-01-01"), xend=zoo::as.yearmon("2012-01-01"), y=-50000, yend=-(total/25)), inherit.aes = F, color="#f9e254") +
  geom_point(data=merged_rel_total %>% filter(gamename=="Team Fortress 2"), aes(x=zoo::as.yearmon("2012-01-01"), y=-(total/25)), color="#f9e254") +
  geom_text(data=merged_rel_total %>% filter(gamename=="Team Fortress 2"), aes(x=zoo::as.yearmon("2012-01-01"), y=(-total/25)-150000, label=paste0("#4\n", gamename, "\nreleased on ", rel_date)), color="#343434", size=3, hjust=1, vjust=0.5, family="press") +
  geom_segment(data=NULL, aes(x=zoo::as.yearmon("2012-01-01"), xend=zoo::as.yearmon("2012-07-01"), y=-50000, yend=-50000), color="#f9e254", linetype="dotted", inherit.aes = F) +
  
  # Grand theft annotation
  geom_segment(data=merged_rel_total %>% filter(gamename=="Grand Theft Auto V"), aes(x=rel_date, xend=rel_date, y=-210000, yend=-300000-(total/40)), inherit.aes = F, color="#e38c3b") +
  geom_point(data=merged_rel_total %>% filter(gamename=="Grand Theft Auto V"), aes(x=rel_date, y=-300000-(total/40)), color="#e38c3b") +
  geom_text(data=merged_rel_total %>% filter(gamename=="Grand Theft Auto V"), aes(x=rel_date, y=-450000-(total/40), label=paste0("#5\n", gamename, "\nreleased on ", rel_date)), color="#343434", size=3, hjust=1, vjust=0.5, family="press") +
  
  scale_fill_manual(values = c("#b2fc20", "#0adf13", "#366700", "#f9e254", "#e38c3b"))+
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(xlim=c(2010,2021)) +
  theme_void() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill="#aeecf7", color="transparent"),
    plot.background = element_rect(fill="#aeecf7", color="transparent"),
  )

plot_final <- p + patchwork::plot_annotation(
  caption = "#TidyTuesday<br>Data: Steam<br>Plot: Twitter @saraahannes | Git @SarahHannes",
  theme = theme(plot.caption = ggtext::element_markdown(family = "serif", colour = "darkgray", size=13))) &
  theme(rect = element_rect(fill = "transparent", color='transparent'))

# save plot ---------------------------------
ggsave("2021_wk12_vg.png", plot = plot_final, width = 18, height = 11, dpi = 80, units = "in", bg = "transparent")

# ---------------------------------
# Not in use but cool anyway :D
# Create streamgraph using streamgraph library

#devtools::install_github("hrbrmstr/streamgraph")
#library(streamgraph)

yrgame %>%
  dplyr::mutate(monyr = as.Date(monyr)) %>%
  streamgraph(key=gamename, value=avg, date=monyr) %>%
  # adjust x axis to only display year
  sg_axis_x(1, "year", "%Y") %>%
  # turn on interactive dropdown menu for key selection
  sg_legend(TRUE, "Game: ") %>%
  identity()
