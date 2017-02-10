library(llamar)
library(ggplot2)
library(RColorBrewer)
limits = c(0.23, 0.65)

ggplot(mtcars, aes(x=cyl, y = mpg, colour = mpg)) +
  geom_point() +
  scale_color_gradientn(colours = brewer.pal(11,"Spectral")[6:11], limits = limits) +
  theme_basic()

save_plot('~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/mcu_palette.pdf')
