# impr toilet by livelihood_zone
hh %>% 
  group_by(livelihood_zone) %>% 
  summarise(pct = mean(impr_unshared_toilet, na.rm = T), n()) %>% 
  arrange(desc(pct))

# water seems not that bad...
hh %>% 
  group_by(livelihood_zone) %>% 
  summarise(pct = mean(impr_water_30min, na.rm = T), n()) %>% 
  arrange(desc(pct))

# diarrhea by livelihood_zone
ch  %>% group_by(livelihood_zone) %>% 
  summarise(pct = mean(diarrhea, na.rm = T), n()) %>% arrange(desc(pct))

# correlation b/w diarrhea...

cor_wash = data.frame(cor(ch %>% select(diarrhea, impr_toilet, impr_water, impr_unshared_toilet, 
                  wash_beforecook, wash_kidtoilet, wash_beforeeat, 
                  wash_ifdirty, wash_aftertoilet, wash_knowl), use = 'pairwise.complete.obs'))

cor_wash$x = row.names(cor_wash)

cor_wash = cor_wash %>% gather(y, corr, -x)
cor_wash = cor_wash %>% filter(corr < 1)

# Summary: diarrhea is only very weakly anti-correlated with either improved sanitation or sanitation knowledge.
# Impr. water and impr. toilets don't necessarily go hand in hand.
ggplot(cor_wash, aes(x = x, y = y, fill = corr)) +
  geom_tile(colour = 'white', size = 0.5) +
  geom_text(aes(colour = corr, label = percent(corr, 0))) +
  scale_colour_text(data_col = cor_wash$corr) +
  scale_fill_gradientn(colours = brewer.pal(11, 'RdYlBu'), values = c(-1, 1)) +
  theme_xylab()
