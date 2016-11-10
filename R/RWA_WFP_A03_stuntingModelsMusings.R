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
  scale_fill_gradientn(colours = brewer.pal(11, 'RdYlBu'), limits = c(-1, 1)) +
  theme_xylab()

plot_corr(ch %>% select(diarrhea, impr_toilet, impr_water, impr_unshared_toilet, 
                            wash_beforecook, wash_kidtoilet, wash_beforeeat,  
                            wash_ifdirty, wash_aftertoilet, wash_knowl))


# t-test of diarrhea and WASH vars ----------------------------------------

wash = ch %>% select(diarrhea, impr_toilet, impr_water, impr_unshared_toilet, 
              wash_beforecook, wash_kidtoilet, wash_beforeeat,  
              wash_ifdirty, wash_aftertoilet, wash_knowl)

# Consistently lower numbers for the ~12% kids w/ diarrhea in past 2 weeks.
View(wash %>% group_by(diarrhea) %>% summarise_each(funs(mean(., na.rm = TRUE), n())))

wash0 = wash %>% filter(diarrhea == 0) %>% select(-diarrhea)
wash1 = wash %>% filter(diarrhea == 1) %>% select(-diarrhea)

for(i in colnames(wash0)) {
  print(i)
  x = t.test(wash0[[i]], wash1[[i]])
  
  print(x)
}

# Results:
# All significantly different at 95% CI except wash_beforeeat, wash_kidtoilet
# So even though they aren't *that* correlated, people w/ diarrhea have worse WASH behavior.
# Comes in the form of having lower access to water/sanitation as well as lower WASH knowledge.
# More lags in (unshared) toilet coverage.
