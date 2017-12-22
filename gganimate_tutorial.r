library(readr)
library(ggplot2)
library(gganimate)
library(scales)
library(dplyr)
library(extrafont)

nations <- read_csv("nations.csv")


glimpse(nations)

nations %>% 
  filter(year == 2014) %>% 
  ggplot(aes(gdp_percap, life_expect)) +
  geom_point(aes(size = population, color = region), alpha = 0.7) +
  xlab("GDP per capita") +
  ylab("Life expectancy at birth") +
  scale_size_area(max_size = 15, guide = FALSE) +
  stat_smooth(formula = y ~ log(x), se = FALSE, size = 0.5, color = "black", linetype = "dotted") +
  scale_x_continuous(label = dollar) +
  scale_color_brewer(palette = "Set1", name = "") +
  theme_minimal(base_family = "Georgia", base_size = 12) +
  theme(legend.position = c(0.8, 0.4))
  
# stat_smooth: specify formula for log curve
# scale_size_area: scaling of point sizes according to data. set guide == FALSE to delete legend

## GGanimate:

nations_chart <- nations %>% 
  ggplot(aes(x = gdp_percap, y = life_expect, frame = year)) +  # frame = year
  xlab("GDP per capita") +
  ylab("Life expectancy at birth") +
  theme_minimal(base_size = 16, base_family = "Georgia") +
  geom_point(aes(size = population, color = region), alpha = 0.7) +
  scale_size_area(guide = FALSE, max_size = 20) +
  scale_x_continuous(labels = dollar) +
  scale_color_brewer(name = "", palette = "Set2") +
  theme(legend.position=c(0.8,0.4))


#stat_smooth(aes(group = year), formula = y ~ log10(x), se = FALSE, size = 0.5, color = "black", linetype="dotted")

# similar to previous static. add frame == argument to main aes(), also add in grouping for stat_smooth

# gganimate(nations_chart, ani.options(convert = "C:/Program Files/ImageMagick-7.0.7-Q16/convert.exe"))


gganimate(nations_chart)


# optimization for animation::
# increase base_size of text, increase max_size for scaled circles
# frame = year   >>> chart for EACH year in data
# group = year in stat_smooth() >>> trend line for EACH year in data

# Save as GIF or video 

gganimate(nations_chart, "nations.gif", ani.width = 750, ani.height = 500, interval = 0.2)
# saved gif created in working directory!

gganimate(nations_chart, "nations.mp4", ani.width = 1600, ani.height = 900, interval = 0.1)
# saved video created in working directory!




## Historical global avg. temperature

# sometimes necessary to keep added data from previous frame in place.

warming <- read_csv("warming.csv")

# set color palette: 
pal <- c("#313695","#4575b4","#74add1","#abd9e9","#e0f3f8",
         "#ffffbf","#fee090","#fdae61","#f46d43","#d73027","#a50026")
vals <- seq(-2, 2, length = 11)


warming %>% 
  ggplot(aes(year, annual)) +
  geom_line(color = "black") +
  geom_point(shape = 21, color = "black", aes(fill = annual), size = 5, stroke = 1) +
  scale_x_continuous(limits = c(1880, 2015)) +
  scale_y_continuous(limits = c(-0.5, 1)) +
  theme_minimal() +
  scale_fill_gradientn(colors = pal, values = vals, 
                       rescaler = function(x, ...) x, guide = FALSE) +
  xlab("") +
  ylab("Difference 1951-1980 (Celsius)") +
  theme(text = element_text(size = 16, family = "Verdana"))


# animated:
warming_chart <- warming %>% 
  ggplot(aes(year, annual, frame = year, cumulative = TRUE)) +
  geom_line(color = "black") +
  geom_point(aes(fill = annual), shape = 21, color = "black", size = 5, stroke = 1) +
  scale_x_continuous(limits = c(1880, 2015)) +
  scale_y_continuous(limits = c(-0.5, 1)) +
  theme_minimal() +
  scale_fill_gradientn(colors = pal, values = vals, 
                       rescaler = function(x, ...) x, guide = FALSE) +
  xlab("") +
  ylab("Difference 1951-1980 (Celsius)") +
  theme(text = element_text(size = 16, family = "Verdana"))


gganimate(warming_chart, interval = 0.1)

# use cumulative = TRUE for leave previous data in place!

# save as GIF and video
gganimate(warming_chart, "warming.gif", ani.width = 750, ani.height = 500, interval = 0.1)
gganimate(warming_chart, "warming.mp4", ani.width = 1600, ani.height = 900, interval = 0.1)




## Combine chart with MAPS!

years <- c(1880:2015)

years

library(purrr)

map(years, print(years))

for (y in years) {
  print(y)
  Sys.sleep(1)
}


# Draw and save chart for EACH year >>> for loop

for (y in years) {
  tmp <- warming %>%
    filter(year <= y)
  chart <- ggplot(tmp, aes(x=year,y=annual)) 
    + geom_line(colour="black") 
    + geom_point(shape = 21, colour="black", aes(fill=annual), size=5, stroke=1) %>%
    + scale_x_continuous(limits=c(1880,2015)) 
    + scale_y_continuous(limits=c(-0.5,1)) 
    + theme_minimal() 
    + scale_fill_gradientn(colors = pal, values=vals, rescaler = function(x, ...) x, oob = identity, guide=FALSE) %>%
    + xlab("") 
    + ylab("Difference from 1951-1980 (ºC)") 
    + theme(text=element_text(size=16,family="Georgia"))
  ggsave(file=paste0("charts/",y,".jpg"), plot = chart, width = 8, height = 4.5, units = "in", dpi=300)
  print(paste0("processing: ",y))
}











