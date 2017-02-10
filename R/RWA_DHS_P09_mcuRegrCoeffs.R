library(RColorBrewer)
library(ggplot2)

colorScheme = c(brewer.pal(11, "PiYG")[1:5], llamar::grey30K, brewer.pal(11, "PiYG")[7:11])

# Regression coefficients for various livelihood zones.

valDF = data.frame(z = c(-0.0687, 0.0865, -0.148, -0.102, -0.0797, -0.137), 
                   y = c(rep('2014', 2), rep('2010', 4)), 
                   x = c("Kivu", "NW", "Kivu", "NW", "Banana", "EAgro"))

limits = c(-max(abs(valDF$z)), max(abs(valDF$z)))
ggplot(valDF, aes(x = x, y= y, color = z, label = z)) +
  geom_point(size = 10) +
  geom_text(size = 4, colour = llamar::grey90K, nudge_y = 0.2) +
  scale_color_gradientn(colours = colorScheme, limits = limits) +
  theme_bw() +
  theme(plot.background = element_rect(fill = '#f0f0f0'),
        panel.background = element_blank(),
        legend.direction = 'horizontal')
