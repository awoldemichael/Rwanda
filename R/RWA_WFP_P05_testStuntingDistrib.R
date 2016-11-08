
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
  theme_ygrid() + theme(axis.text.y = element_text(size = 8))
