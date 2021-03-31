# Import Libraries
extrafont::loadfonts(device = "win", quiet=T) # do this everytime before loading ggplot
library(tidyverse)
library(ggfx)
library(ggtext)
library(showtext)
showtext_auto()
font_add_google("Philosopher", "Philosopher")
font_add_google("Roboto", 'Roboto')
font_add_google("Quantico", 'Quantico')

# Load data
allNumbers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allNumbers.csv')

# Preprocessing
mt500 <- allNumbers %>%
  group_by(brand) %>%
  add_tally() %>%
  filter(numbers > 500) %>%
  slice(1L) %>%
  mutate(brand=ifelse(brand=="Rare Beauty by Selena Gomez", "Rare Beauty", brand),
         brand=ifelse(brand=="Marc Jacobs Beauty", "Marc Jacobs", brand),
         brand=ifelse(brand=="Anastasia Beverly Hills", "ABH", brand)) %>%
  ungroup()

# Plot
p1 <- allNumbers %>%
  group_by(brand) %>%
  add_tally() %>%
  mutate(brand=ifelse(brand=="Rare Beauty by Selena Gomez", "Rare Beauty", brand),
         brand=ifelse(brand=="Marc Jacobs Beauty", "Marc Jacobs", brand),
         brand=ifelse(brand=="Anastasia Beverly Hills", "ABH", brand)) %>%
  filter(brand %in% mt500$brand) %>%
  ungroup() %>%
  
  ggplot(aes(x=reorder(brand, n), y=numbers)) +
  with_blur(geom_bar(aes(fill=hex), color=NA, width=1.5, stat='identity', position = position_dodge(0.1))) +
  with_blur(geom_rect(aes(xmin=brand, xmax=brand, ymin=min(numbers), ymax=1, fill=hex), color=NA, stat='identity', position = position_dodge(0.3))) +
  
  # Brand names annotations
  geom_text(data=mt500, aes(x=brand, y=0, label=brand), color='darkgray', hjust=1.0, vjust=1, size=10, nudge_x = 0.2, nudge_y = -1, family='Quantico') +
  geom_text(data=mt500 %>% filter(lightToDark==FALSE),
            aes(x=brand, y=0, label=brand), color='#ca6c70', hjust=1.0, vjust=1, size=10, nudge_x = 0.2, nudge_y = -1, family='Quantico') +
  
  scale_fill_manual(values=allNumbers$hex) +
  scale_color_manual(values=allNumbers$hex) +
  labs(title="<span style='color:darkgray'>Bias in the Beauty Industry?</span>",
       subtitle="<span style='color:darkgray'>Only 2 out of 14 Major Beauty Brands organize their<br>foundation shades from <span style='color:#ca6c70'>**dark**</span> to light
                <span style='font-size: 50pt;'><span style='color:#462d1a'>.</span><span style='color:#a06742'>.</span><span style='color:#eac8a1'>.</span></span>
                <br>*Perhaps it is time we address the*<br>*subtle microaggressions*<br>*finely intertwined*<br>*in our culture.*</style>",
       caption="<span style='color:darkgray'>Data: The Pudding    |    @saraahannes</span>",
       x=NA, y=NA) +
  theme_void() +
  theme(
    legend.position = 'none',
    plot.background = element_rect(fill="#fafafc", color='NA'),
    plot.title.position = 'plot',
    plot.title = element_markdown(family='Philosopher', size=80),
    plot.subtitle = element_markdown(family='Roboto', size=40, lineheight = 0.2),
    plot.caption = element_markdown(family='Quantico', hjust = 0.5, vjust = 0.5, size=28)
  ) +
  coord_polar(theta = "y")

# Save plot
ggsave("2021_wk14_makeup.png", plot = p1, type = 'cairo', width = 6, height = 7, dpi = 300, units = "in", bg = "#fafafc")


#------------- Not used --------------

get_brand <- function(brand_name) {
  allNumbers %>%
    group_by(brand) %>%
    filter(brand==brand_name) %>%
    ungroup()
}

plot_swatch <- function(brand_name, geom) {
  
  filtered_brand <- get_brand(brand_name)
  
  ggplot(data=filtered_brand, aes(x=brand, y=reorder(numbers, numbers), fill=hex)) +
    geom(position=position_dodge(0.9)) +
    scale_fill_manual(values=filtered_brand$hex) +
    labs(title=brand_name, subtitle="", x=" ", y='swatches numbers') +     
    coord_flip() +
    theme(
      legend.position='none'
    )
}
