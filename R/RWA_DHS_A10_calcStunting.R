library(haven)
library(dplyr)
library(llamar)

# Point estimates for DHS
# Data processed by Tim
df = read_dta('~/Documents/USAID/Rwanda/processeddata/DHS_2010_2015_analysis.dta')

df = df %>% filter(!is.na(eligChild))

df2014 = removeAttributes(df) %>% filter(year==2014)
df2010 = removeAttributes(df) %>% filter(year==2010)

df2010 = factorize(df2010, df, 'lvdzone', 'lvdzone2')
df2014 = factorize(df2014, df, 'lvdzone', 'lvdzone2')

# ! Note: stunted2 is the right one.  stunted is wrong.
# sans weights
st_dhs2014 = calcPtEst(df2014, 'stunted2', by_var = 'lvdzone2', use_weights = FALSE) %>% 
  mutate(livelihood_zone = case_when(st_dhs2014$lvdzone2 %like% 'Tea' ~ 'West Congo-Nile Crest Tea Zone',
                                     st_dhs2014$lvdzone2 %like% 'Wheat' ~ 'Northern Highland Beans and Wheat Zone',                          
                                     st_dhs2014$lvdzone2 %like% 'Eastern Congo' ~ 'East Congo-Nile Highland Subsistence Farming Zone',
                                     st_dhs2014$lvdzone2 %like% 'Volcanic' ~ 'Northwest Volcanic Irish Potato Zone',                            
                                     st_dhs2014$lvdzone2 %like% 'Mixed' ~ 'Eastern Plateau Mixed Agriculture Zone',
                                     st_dhs2014$lvdzone2 %like% 'Eastern Ag' ~ 'Eastern Agropastoral Zone',                                       
                                     st_dhs2014$lvdzone2 %like% 'Central-Northern' ~ 'Central-Northern Highland Irish Potato, Beans and Vegetable Zone',
                                     st_dhs2014$lvdzone2 %like% 'Kivu' ~ 'Lake Kivu Coffee Zone',
                                     st_dhs2014$lvdzone2 %like% 'Banana' ~ 'Southeastern Plateau Banana Zone',
                                     st_dhs2014$lvdzone2 %like% 'Bugesera' ~ 'Bugesera Cassava Zone',                                           
                                     st_dhs2014$lvdzone2 %like% 'Central Plateau' ~ 'Central Plateau Cassava and Coffee Zone',
                                     st_dhs2014$lvdzone2 %like% 'Semi-Arid' ~ 'Eastern Semi-Arid Agropastoral Zone',                             
                                     st_dhs2014$lvdzone2 %like% 'Urban' ~ 'Kigali city',
                                     TRUE ~ NA_character_))

write.csv(st_dhs2014, '~/Github/Rwanda/exported_data/DHS_stunted_unweighted2014.csv')

st_dhs2010 = calcPtEst(df2010, 'stunted2', by_var = 'lvdzone2', use_weights = FALSE) %>% 
  mutate(livelihood_zone = case_when(st_dhs2010$lvdzone2 %like% 'Tea' ~ 'West Congo-Nile Crest Tea Zone',
                                     st_dhs2010$lvdzone2 %like% 'Wheat' ~ 'Northern Highland Beans and Wheat Zone',                          
                                     st_dhs2010$lvdzone2 %like% 'Eastern Congo' ~ 'East Congo-Nile Highland Subsistence Farming Zone',
                                     st_dhs2010$lvdzone2 %like% 'Volcanic' ~ 'Northwest Volcanic Irish Potato Zone',                            
                                     st_dhs2010$lvdzone2 %like% 'Mixed' ~ 'Eastern Plateau Mixed Agriculture Zone',
                                     st_dhs2010$lvdzone2 %like% 'Eastern Ag' ~ 'Eastern Agropastoral Zone',                                       
                                     st_dhs2010$lvdzone2 %like% 'Central-Northern' ~ 'Central-Northern Highland Irish Potato, Beans and Vegetable Zone',
                                     st_dhs2010$lvdzone2 %like% 'Kivu' ~ 'Lake Kivu Coffee Zone',
                                     st_dhs2010$lvdzone2 %like% 'Banana' ~ 'Southeastern Plateau Banana Zone',
                                     st_dhs2010$lvdzone2 %like% 'Bugesera' ~ 'Bugesera Cassava Zone',                                           
                                     st_dhs2010$lvdzone2 %like% 'Central Plateau' ~ 'Central Plateau Cassava and Coffee Zone',
                                     st_dhs2010$lvdzone2 %like% 'Semi-Arid' ~ 'Eastern Semi-Arid Agropastoral Zone',                             
                                     st_dhs2010$lvdzone2 %like% 'Urban' ~ 'Kigali city',
                                     TRUE ~ NA_character_))


write.csv(st_dhs2010, '~/Github/Rwanda/exported_data/DHS_stunted_unweighted2010.csv')

# mit weights
st_dhs_2014_weighted = calcPtEst(df2014, 'stunted2', by_var = 'district', use_weights = TRUE,
                   psu_var = 'psu', strata_var = 'strata', weight_var = 'cweight')
write.csv(st_dhs_2014_weighted, '~/Github/Rwanda/exported_data/DHS_stunted_weighted.csv')


st_dhs_2014_all = calcPtEst(df2014, 'stunted2', use_weights = TRUE,
                                 psu_var = 'psu', strata_var = 'strata', weight_var = 'cweight')
st_dhs_2010_all = calcPtEst(df2010, 'stunted2', use_weights = TRUE,
                            psu_var = 'psu', strata_var = 'strata', weight_var = 'cweight')

st2010 = st_dhs2010 %>% 
  select(livelihood_zone, avg2010 = avg, N2010 = N, lb2010 = lb, ub2010 = ub) %>% 
  mutate(natl2010 = st_dhs_2010_all$stunted2)

st2014 = st_dhs2014 %>% 
  select(livelihood_zone, avg2014 = avg, N2014 = N, lb2014 = lb, ub2014 = ub) %>% 
  mutate(natl2014 = st_dhs_2014_all$stunted2)

st_dhs_wide = full_join(st2010, st2014)

write.csv(st_dhs_wide, '~/Github/Rwanda/exported_data/DHS_stunted_weighted.csv')

library(ggbeeswarm)

dhs = removeAttributes(df) %>% 
  filter(!is.na(stunting))



dhs = factorize(dhs, df, 'lvdzone', 'lvdzone')

lz_order = dhs %>% 
  filter(year == 2014) %>% 
  group_by(lvdzone) %>% 
  summarise(avg = mean(stunting, na.rm = TRUE)) %>% 
  arrange(desc(avg))

dhs$lvdzone = factor(dhs$lvdzone, levels = lz_order$lvdzone)

ggplot(dhs, aes(y = stunting, x = lvdzone, 
                group = lvdzone,
                colour = factor(year), fill = year)) + 

  geom_quasirandom(alpha = 0.6) +
  # geom_violin(alpha = 0.4, fill = NA)+
  # geom_boxplot(fill = NA, colour = grey75K, size = 0.2) +
  geom_hline(yintercept = -2) +
  
    # facet_wrap(~year) +
  coord_flip() +
  scale_color_manual(values = c('2010' = grey60K, '2014' = brewer.pal(11, 'Spectral')[1])) +
   # scale_colour_gradientn(colours = brewer.pal(11, 'Spectral')[1:6]) +
  theme_xgrid()
