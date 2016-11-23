# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_P05_testStuntingDistrib.R: testing violin plots of stunting
#
# Data are from the 2015 Comprehensive Food Security and Vulnerability Analysis
# by the World Food Programme
# Available at http://microdata.statistics.gov.rw/index.php/catalog/70
# Report: https://www.wfp.org/content/rwanda-comprehensive-food-security-and-vulnerability-analysis-march-2016
#
# Laura Hughes, lhughes@usaid.gov, 5 October 2016
# with Tim Essam (tessam@usaid.gov) and Nada Petrovic (npetrovic@usaid.gov)
#
# Copyright 2016 by Laura Hughes via MIT License


# Dependencies ------------------------------------------------------------
setwd('~/GitHub/Rwanda/R/')
source('RWA_WFP_runAll.R')


# Quick test of violin plots + rug of stunting z-scores -------------------


x = ch %>% group_by(livelihood_zone) %>% summarise(x = mean(isStunted, na.rm = T)) %>% arrange(desc(x))

ch$livelihood_zone = fct_relevel(ch$livelihood_zone, as.character(x$livelihood_zone))


ggplot(ch, aes(x = livelihood_zone, y = stuntingZ)) +
  geom_hline(yintercept = -2, colour = 'blue') +
  geom_point(aes(colour = factor(isStunted)),
              alpha = 0.2, shape = 95, size = 6) +
  stat_summary(geom = 'point', fun.y = 'mean', 
               colour = 'red', shape = 95, size = 10, na.rm = TRUE) +
  geom_violin(fill = NA) +
  scale_colour_manual(values= c('0' = 'blue', '1' = 'red')) +
# gradientn(colours = brewer.pal(11, 'RdYlBu'), limits = c(-4, 2))+
  # coord_flip() + 
  theme_ygrid() + 
  theme(axis.text.y = element_text(size = 8)) +
  theme_stroke()

save_plot('~/GitHub/Rwanda/exported_img/violin.png', width = 5, height = 4)
