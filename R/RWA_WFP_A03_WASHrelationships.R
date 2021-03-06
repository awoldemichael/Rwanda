# Rwanda stunting analysis -----------------------------------------
#
# RW_WFP_A03_WASHrelationships.R: exploring relationship b/w WASH variables
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



# import data -------------------------------------------------------------

source('~/GitHub/Rwanda/R/RWA_WFP_run2015.R')

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
                            wash_ifdirty, wash_aftertoilet, wash_knowl)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.85))


# t-test of diarrhea and WASH vars ----------------------------------------

wash = ch %>% select(diarrhea, impr_toilet, impr_water, impr_unshared_toilet, 
              wash_beforecook, wash_kidtoilet, wash_beforeeat,  
              wash_ifdirty, wash_aftertoilet, wash_knowl, mother_literate, education_groups, wealth_idx, FCS)

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

# Just for fun... throwing in education:
# AND literacy, education lower.
# AND wealth index lower, FCS lower.
# Not surprising.  Generally poorer, less educated.


