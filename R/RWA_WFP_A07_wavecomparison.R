
# import data -------------------------------------------------------------
source('~/GitHub/Rwanda/R/RWA_WFP_07_importHH2012.R')
source('~/GitHub/Rwanda/R/RWA_WFP_07_importHH2012.R')



# Monthly pc expend comparison.
# Similar densities; shift to higher values for 2015.  May just be b/c of inflation.
ggplot(ch_hh2012, aes(x = log10(pc_exp_year/12))) +
  geom_density(fill = '#abd9e9', alpha = 0.3) + 
  xlim(c(0,6)) + 
  geom_density(aes(x = log10(monthly_pc_expend)), data = ch_hh, fill = 'red', alpha  = 0.3 ) +
  theme_xygrid()


# Compare pc_exp with stunting relationship:
ggplot(ch_hh, aes(x = scale(log_pcexp), y = stuntingZ)) + 
  geom_smooth(colour = '#313695') +
  geom_smooth(colour = '#abd9e9', data = ch_hh2012) +
  facet_wrap(~rural_cat) +
  # geom_vline(xintercept = 0.4) + # location of knot.
  theme_xygrid() + xlim(c(-2.5, 2.5)) # clipping super small values

ggplot(ch_hh, aes(x = scale(log_pcexp), y = impr_unshared_toilet)) + 
  geom_smooth(colour = '#313695') +
  geom_smooth(colour = '#abd9e9', data = ch_hh2012) +
  theme_xygrid() + xlim(c(-2.5, 2.5)) + 
  ggtitle('toilets')

ggplot(ch_hh, aes(x = scale(log_pcexp), y = impr_water_30min)) + 
  geom_smooth(colour = '#313695') +
  geom_smooth(colour = '#abd9e9', data = ch_hh2012) +
  theme_xygrid() + xlim(c(-2.5, 2.5)) + 
  ggtitle('water')

ggplot(ch_hh, aes(x = scale(log_pcexp), y = FCS)) + 
  geom_smooth(colour = '#313695') +
  geom_smooth(colour = '#abd9e9', data = ch_hh2012) +
  theme_xygrid() + xlim(c(-2.5, 2.5)) +
  ggtitle('FCS')

ggplot(ch_hh, aes(x = scale(log_pcexp))) + 
  geom_smooth(aes(y = CSI.x), colour = '#313695') +
  geom_smooth(aes(y = CSI), colour = '#abd9e9', data = ch_hh2012) +
  theme_xygrid() + xlim(c(-2.5, 2.5)) +
  ggtitle('CSI')

ggplot(ch_hh, aes(x = scale(log_pcexp), y = months_food_access)) + 
  geom_smooth(colour = '#313695') +
  geom_smooth(colour = '#abd9e9', data = ch_hh2012) +
  theme_xygrid() + xlim(c(-2.5, 2.5)) +
  ggtitle('number of months with food access problems')

# 2015 


ggplot(ch_hh, aes(x = scale(log_pcexp), y = as.numeric(mother_education))) + 
  geom_smooth(colour = '#313695') +
  geom_smooth(colour = '#abd9e9', data = ch_hh2012) +
  # geom_vline(xintercept = 0.4) + # location of knot.
  theme_xygrid()  +
  ggtitle('mother education')

ggplot(ch_hh, aes(x = scale(log_pcexp), y = as.numeric(head_education_cat))) + 
  geom_smooth(colour = '#313695') +
  geom_smooth(colour = '#abd9e9', data = ch_hh2012) +
  # geom_vline(xintercept = 0.4) + # location of knot.
  theme_xygrid()  +
  ggtitle('head education')

# Wealth Idx comparison ---------------------------------------------------
ggplot(ch_hh, aes(x = wealth_idx_num, y = stuntingZ)) + 
  stat_summary(geom = 'point', fun.y  = 'mean', size = 4, colour = '#313695') +
  stat_summary(geom = 'point', fun.y  = 'mean', size = 4, colour = '#abd9e9', data = removeAttributes(ch_hh2012)) +
  # geom_vline(xintercept = 0.4) + # location of knot.
  theme_xygrid() 

ggplot(ch_hh, aes(x = wealth_idx_num, y = impr_unshared_toilet)) + 
  stat_summary(geom = 'point', fun.y  = 'mean', size = 4, colour = '#313695') +
  stat_summary(geom = 'point', fun.y  = 'mean', size = 4, colour = '#abd9e9', data = removeAttributes(ch_hh2012)) +
  # geom_vline(xintercept = 0.4) + # location of knot.
  theme_xygrid()  +
  ggtitle('toilets')




ggplot(ch_hh, aes(x = wealth_idx_num, y = impr_water_30min)) + 
  stat_summary(geom = 'point', fun.y  = 'mean', size = 4, colour = '#313695') +
  stat_summary(geom = 'point', fun.y  = 'mean', size = 4, colour = '#abd9e9', data = removeAttributes(ch_hh2012)) +
  # geom_vline(xintercept = 0.4) + # location of knot.
  theme_xygrid() +
  ggtitle('water')

ggplot(ch_hh, aes(x = wealth_idx_num, y = FCS)) + 
  stat_summary(geom = 'point', fun.y  = 'mean', size = 4, colour = '#313695') +
  stat_summary(geom = 'point', fun.y  = 'mean', size = 4, colour = '#abd9e9', data = removeAttributes(ch_hh2012)) +
  # geom_vline(xintercept = 0.4) + # location of knot.
  theme_xygrid() +
  ggtitle('FCS')

ggplot(ch_hh, aes(x = wealth_idx_num)) + 
  stat_summary(geom = 'point', fun.y  = 'mean', size = 4, aes(y = CSI.x), colour = '#313695') +
  stat_summary(geom = 'point', fun.y  = 'mean', size = 4, aes(y = CSI), colour = '#abd9e9', data = removeAttributes(ch_hh2012)) +
  theme_xygrid() +
  ggtitle('CSI')

ggplot(ch_hh, aes(x = wealth_idx_num, y = months_food_access)) + 
  stat_summary(geom = 'point', fun.y  = 'mean', size = 4, colour = '#313695') +
  stat_summary(geom = 'point', fun.y  = 'mean', size = 4, colour = '#abd9e9', data = removeAttributes(ch_hh2012)) +
  # geom_vline(xintercept = 0.4) + # location of knot.
  theme_xygrid() +
  ggtitle('number of months with food access problems')


# Model comparison --------------------------------------------------------

compare_models(list('2012' = stunting_fits2012$nogeo,
                    '2015' = stunting_fits$nogeo,
                    ' altitude' = stunting_fits2012$alt), 
filter_insignificant = F, sort_by_est = F, alpha_insignificant = 0.2)

# Most obvious difference is the change w/i the regions.
compare_models(list('2012' = stunting_fits2012$all,
                    '2015' = stunting_fits$all,
                    ' altitude' = stunting_fits2012$alt), 
               filter_insignificant = F, sort_by_est = F, alpha_insignificant = 0.2)


# kids under 5 ------------------------------------------------------------
kids2012 = ch_hh2012  %>% group_by(livelihood_zone) %>% 
  summarise(avg12 = mean(as.numeric(kids_under5), na.rm=T)) %>% arrange(desc(avg12))

kids2015 = ch_hh  %>% group_by(livelihood_zone) %>% 
  summarise(avg15 = mean(as.numeric(kids_under5), na.rm=T)) %>% arrange(desc(avg15))

kids = full_join(kids2015, kids2012)
ggplot(kids, aes(x = 2012, xend = 2015, y = avg12, yend = avg15, colour = livelihood_zone)) +
  geom_segment() +
  theme_xygrid() +
  theme(legend.position = 'left')

