library(ggplot2)
library(data.table)
library(dplyr)
library(geocenter)
library(llamar)
library(RColorBrewer)

source("~/GitHub/Rwanda/R/RWA_WFP_05_importGeo.R")

df = read.csv('~/GitHub/rwanda-gc/data/stunting.csv')

df = df %>% mutate(diff = avg2014 - avg2010,
                   pct = (avg2014 - avg2010)/avg2010)

geo = full_join(RWA_LZ$df, df, by = c('livelihood_zone'))



ggplot(df, aes(x = livelihood_zone, y = diff, fill = diff)) +
  coord_flip() +
  geom_point(size = 10, shape=21) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "BrBG")), limits = c(min(df$diff), -min(df$diff))) +
  theme_xylab()

ggplot(df, aes(x = livelihood_zone, y = diff, fill = pct)) +
  coord_flip() +
  geom_point(size = 10, shape=21) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "BrBG")), limits = c(min(df$pct), -min(df$pct))) +
  theme_xylab()

ggplot(df, aes(x = livelihood_zone, y = diff, fill = pct)) +
  coord_flip() +
  geom_point(size = 10, shape=21) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")), breaks = seq(-0.45, 0.1, by = 0.05),
                       limits = c(min(df$pct), -min(df$pct))) +
  theme_xylab(legend.position = 'left')

save_plot('~/Desktop/plot.pdf', width=8)
