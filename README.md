# tidytuesday

Contributions made for #TidyTuesday, a weekly data project in R from the R4DS online learning community.
A new data is shared by <a href="https://github.com/rfordatascience/tidytuesday/blob/master/README.md">R4DS Community</a> each week.
By participating, I aim to improve my R language fluency especially in using {tidyverse} package, while learning, discovering and improving my data storytelling fundamentals.

## 2021 Week 28 International Independence <a href="R/2021_wk28_independence.R">[Code_1]</a> <a href="R/2021_wk28_independence_b.R">[Code_2]</a>
<img src="plot/2021_wk28_independence.png" width="900">
<img src="plot/2021_wk28_independence_b.png" width="900">

## 2021 Week 16 Post Office <a href="R/2021_wk16_postoffice.R">[Code]</a>
<img src="plot/2021_wk16_postoffice.png" width="900">

## 2021 Week 15 Deforestation <a href="R/2021_wk15_deforestation.R">[Code]</a>
<img src="plot/2021_wk15_deforestation.png" width="900">

## 2021 Week 14 Makeup Shades <a href="R/2021_wk14_makeup.R">[Code]</a>
<img src="plot/2021_wk14_makeup.png" width="900">

## 2021 Week 13 UN Votes <a href="R/2021_wk13_unvotes.R">[Code]</a>
<img src="plot/2021_wk13_unvotes.png" width="900">

## 2021 Week 12 Video Games <a href="R/2021_wk12_vg.R">[Code]</a>
<img src="plot/2021_wk12_vg.png" width="900">

## 2021 Week 11 Bechdel Test <a href="R/2021_wk11_bechdel.R">[Code]</a>
<img src="plot/2021_wk11_bechdel.png" width="900"></a>
<br>What I learnt this week:
- Had fun exploring `geom_🌖` and created map 🌎 for the first time! 
- Learnt how to import fonts yay!
- Finally figured out the best way to combine plots and save it using `ggsave()`

## 2021 Week 10 SuperBowl Ads <a href="R/2021_wk10_superbowl.R">[Code]</a>
<img src="plot/2021_wk10_superbowl.jpg" width="900"></a>
<br>What I learnt this week:
 - Using `dplyr::count(col)` as a shorthand to add frequency with grouped by `col`
 - Using `dplyr::top_n(n=1, wt=col)` to get top_n row using `col` as weight
 - Met with roadblock when trying to add image using `ggdraw()` to `coord_polar` plot.. *still figuring this out*
 - Originally wanted to produce something similar like this <a href="https://msucreativecomp.files.wordpress.com/2016/08/data_points.pdf#page=205">starplots by Nathan Yau</a>, but still couldn't figure out how, so this will have to do for now.. 😅
 - Wrestled a lot with angle and radius and arc and what not when I tried to adjust the arc? angle? of the 'petals' so that it doesn't overlap... *really need to brush up on my dusty trig soon. A very humbling experience indeed (lol)*

## 2021 Week 9 BLS Employment Data <a href="R/2021_wk9_employment.R">[Code]</a>
<img src="plot/2021_wk9_employment.jpg" width="900"></a>
<br>What I learnt this week:
 - `geom_bar(stat='identity')` and `geom_col()` essentially produce the same output
 - Adjusting upper and lower limit using <a href="https://ggplot2.tidyverse.org/reference/expand_scale.html">`scale_*_continuous(expand = expansion(mult = c( xx, xx )))`</a>
 - Filtering out rows using `%in%` ie `dplyr::filter(!industry %in% c(NA, 'Men', 'Women', 'Asian', 'Black or African American '))`
 - Add `na.rm=T` argument when doing arithmetic operation so that any rows with NA data will be ignored/ parsed as 0 (Otherwise all output with NA rows will be NAs). ie `dplyr::mutate(total=sum(employ_n, na.rm=T))`

## 2021 Week 8 W.E.B. Du Bois Challenge <a href="R/2021_wk8_dubois.R">[Code]</a>
<img src="plot/2021_wk8_dubois.jpg" width="900"></a>
<br>What I learnt this week:
 - Using <a href="https://cran.r-project.org/web/packages/magick/vignettes/intro.html">`library(magick)`</a> to magically✨✨ transform images 😍
 - Using `element_rect(fill = "transparent")` to set the background of plot/panel/legend to transparent
 - Using linetype as aes and setting manual values using `scale_linetype_manual(values=c())`
 - Came across various other customizable <a href="http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/#changing-the-position-of-the-legend">`scale_xxx_yyy*s*`</a> 🤩
 - Fiddled a lot using `legend.position=c()` to try to span the horizontal legend centered across the panel width
 - Combine ggplot object and image object using `ggdraw()`
 - Renewed appreciation for Black History Month!
 - Beautiful <a href="https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf">DuBois style</a> *Thank you @Anthony Starks*
 
 <br><i>Thank you and have a nice day!</i><br>Sarah🐱‍👤
