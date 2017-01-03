# Point estimates for DHS
# Data processed by Tim
df = read_dta('~/Documents/USAID/Rwanda/processeddata/DHS_2010_2015_analysis.dta')

df2 = removeAttributes(df) %>% filter(year==2014)


df2 = factorize(df2, df, 'lvdzone', 'lvdzone2')

# sans weights
st_dhs = calcPtEst(df2, 'stunted', by_var = 'lvdzone2', use_weights = FALSE)
write.csv(st_dhs, '~/Github/Rwanda/exported_data/DHS_stunted_unweighted.csv')


# mit weights
st_dhs = calcPtEst(df2, 'stunted', by_var = 'lvdzone2', use_weights = TRUE,
                   psu_var = 'psu', strata_var = 'strata', weight_var = 'cweight')
write.csv(st_dhs, '~/Github/Rwanda/exported_data/DHS_stunted_weighted.csv')


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
