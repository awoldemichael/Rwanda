library(RColorBrewer)
library(ggplot2)
colorScheme = brewer.pal(11, 'RdYlBu')

coolwarm = list(c(0.32860444731764704, 0.43971182997647057, 0.86958726254117646), 
                c(0.43581480630588237, 0.57070730315294116, 0.95171738128235295), 
                c(0.55431186991372539, 0.69009701121568623, 0.99551554823529409), 
                c(0.66725292433333339, 0.77917645699999993, 0.99295921300000001), 
                c(0.7727059486039215, 0.83897821723921562, 0.9493187599137255), 
                c(0.86742763508627452, 0.86437659977254899, 0.86260246201960789), 
                c(0.93832635633333328, 0.80891655203137258, 0.74116151502745098), 
                c(0.96820339899999996, 0.72084409999999999, 0.61229299133333337), 
                c(0.9566532109764706, 0.59803382271764705, 0.47730229235294119), 
                c(0.90578347801176473, 0.45518569216470589, 0.35533588384705883), 
                c(0.82040109838823527, 0.28676491263529408, 0.2451595198))
coolwarm = lapply(coolwarm, function(test) rgb(test[1], test[2], test[3], maxColorValue=1))
coolwarm = unlist(coolwarm)
colorScheme = coolwarm
colorScheme = c(llamar::grey30K, brewer.pal(9, "BuPu")[3:9])

# colorScheme = rev(brewer.pal(11, 'BrBG'))
pal = colorRamp(colorScheme)

# Regression coefficients for various livelihood zones.
vals = c(-0.02,	-0.065,
  -0.011,	-0.048,
-0.041,	0.004,
-0.048,	-0.029, 
0, 0)

# normalize values to go b/w 0-1, centered at 0.5

maxVal = max(abs(min(vals)), max(vals))
scaledVals = (vals + maxVal)/(2*maxVal)
lapply(scaledVals, function(x) pal(x))

valDF = data.frame(z = abs(vals), y = rep(c('2010', '2014'), 5), x = c("NW", "NW", "ECongo", "ECongo", "EPlat", "EPlat", "EAgro", "EAgro", "Kivu", "Kivu"))

ggplot(valDF, aes(x = x, y= y, color = z)) +
  geom_point(size = 10) +
  scale_color_gradientn(colours = colorScheme, limits = c(0, maxVal)) +
  theme_bw() +
  theme(plot.background = element_rect(fill = '#f0f0f0'),
        panel.background = element_blank(),
        legend.direction = 'horizontal')
