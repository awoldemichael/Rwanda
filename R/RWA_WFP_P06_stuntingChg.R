stunting_comb = full_join(stunting_lz_2012, stunting_lz_cfsva, by = 'livelihood_zone')


stunting_comb = stunting_comb %>% 
  select(`2012` = isStunted.x, `2015` = unweighted_avg, livelihood_zone) %>% 
  gather(year, avg, -livelihood_zone) 

stunting_comb = stunting_comb %>% 
  mutate(lvdzone_name = case_when(stunting_comb$livelihood_zone == "Lake Kivu Coffee Zone" ~"Lake Kivu Coffee",
                                  stunting_comb$livelihood_zone == "Central Plateau Cassava and Coffee Zone" ~ "Central Plateau Cassava and Coffee",                         
                                  stunting_comb$livelihood_zone == "East Congo-Nile Highland Subsistence Farming Zone" ~ "Eastern Congo-Nile Highland Subsistence Farming",               
                                  stunting_comb$livelihood_zone == "Southeastern Plateau Banana Zone" ~ "Southeastern Plateau Banana",                                
                                  stunting_comb$livelihood_zone == "Northwest Volcanic Irish Potato Zone" ~ "Northwestern Volcanic Irish Potato",                            
                                  stunting_comb$livelihood_zone == "Kigali city" ~ "Urban Area",                                                     
                                  stunting_comb$livelihood_zone == "West Congo-Nile Crest Tea Zone" ~ "Western Congo-Nile Crest Tea",                                  
                                  stunting_comb$livelihood_zone == "Central-Northern Highland Irish Potato, Beans and Vegetable Zone" ~ "Central-Northern Highland Irish Potato, Beans and Vegetables",
                                  stunting_comb$livelihood_zone == "Eastern Plateau Mixed Agriculture Zone" ~ "Eastern Plateau Mixed Agricultural",                          
                                  stunting_comb$livelihood_zone == "Eastern Agropastoral Zone" ~ "Eastern Agropastoral",                                       
                                  stunting_comb$livelihood_zone == "Bugesera Cassava Zone" ~ "Bugesera Cassava",                                           
                                  stunting_comb$livelihood_zone == "Northern Highland Beans and Wheat Zone" ~ "Northern Highland Beans and Wheat",                          
                                  stunting_comb$livelihood_zone == "Eastern Semi-Arid Agropastoral Zone" ~ "Eastern Semi-Arid Agropastoral",
                                  TRUE ~ NA_character_))

stunting2015 = stunting_comb %>% 
  filter(year == '2015') %>% 
  arrange((avg))

# Reorder factors
stunting_comb$livelihood_zone = factor(stunting_comb$livelihood_zone, 
                                       levels = stunting2015$livelihood_zone)
arrow_adj = 0.05
stunting_untidy = stunting_comb %>% 
  spread(year, avg) %>% 
  mutate(year1 = `2012`, 
         year2 = `2015`, 
         y2 = ifelse(`2015` < `2012`, 
                     `2015` * (1 + arrow_adj),
                     `2015` * (1 - arrow_adj)),
         diff = `2015` - `2012`)


df = factorize(dhs, dhs, 'lvdzone', 'lvdzone_name')



year_var = 'year'
group_var = 'lvdzone_name'
value_var = 'stunted'
dot_size = 6 
sort_on = 'year2' # options: year1, year2, diff

# Assumes data come in tidy form

# -- Calculate mean values --
df_summary = df %>% 
  filter_(paste0('!is.na(', value_var, ')')) %>% 
  group_by_(group_var, year_var) %>% 
  summarise_(.dots = list(avg = paste0('mean(', value_var, ')'),
                                       N = 'n()'))

# -- Spread wide for connector line / sorting --
df_wide = df_summary %>% 
  spread(year, avg) %>% #!!
  rename(year1 = `2010`, #!!
         year2 = `2014`) %>% 
  mutate(diff = year2 - year1) %>% 
  arrange_(sort_on)


# -- Relevel --
df_wide[[group_var]] = factor(df_wide[[group_var]],
                              levels = df_wide[[group_var]])

df_summary[[group_var]] = factor(df_summary[[group_var]],
                                 levels = df_wide[[group_var]])

ggplot(df_summary) +
  geom_segment(aes_string(x = 'year1', xend  = 'diff * 0.9 + year1',
                          y = group_var, yend = group_var),
               size = 0.5,
               arrow = arrow(length = unit(0.03, "npc")),
               colour = grey60K,
               data = df_wide) +
  geom_point(aes_string(x = 'avg', y = group_var,
                        color = paste0('as.factor(', year_var, ')'), 
                        shape = paste0('as.factor(', year_var, ')'), 
                        fill = 'avg'),
             # paste0('as.factor(', year_var, ')')),
             size = dot_size, colour = grey90K) +
  
  # CFSVA
  geom_segment(aes_string(x = 'year1', xend  = 'diff * 0.9 + year1',
                          y = group_var, yend = group_var),
               size = 0.5,
               arrow = arrow(length = unit(0.03, "npc")),
               colour = grey60K,
               data = stunting_untidy) +
  geom_point(aes_string(x = 'avg', y = group_var,
                        color = paste0('as.factor(', year_var, ')'), 
                        shape = paste0('as.factor(', year_var, ')'), 
                        fill = 'avg'),
             # paste0('as.factor(', year_var, ')')),
             size = dot_size, colour = grey90K,
             data = stunting_comb) +
  
  # geom_text(aes(x = stunting, y = group_var,
  #                color = year, shape = year, fill = year,
  #                label = percent(stunting, 0)),
  #           colour = grey75K,
  #           size = 3) +
  theme_xgrid() +
  scale_shape_manual(values = c(21, 23, 22, 24)) +
  scale_x_continuous(labels = percent) +
  scale_fill_gradientn(colours = brewer.pal(9, 'RdPu')) +
  # scale_fill_manual(values = c('2010' = 'white', '2014' = brewer.pal(9, 'Spectral')[1])) +
  theme(axis.text.y = element_text(size = 10),
        axis.title.x = element_blank())

save_plot('~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/stunting_change.pdf', width = 7, height = 10)



# Map ---------------------------------------------------------------------

stunting2015 = ch %>% 
  select(livelihood_zone, stuntingZ) %>% 
  mutate(year = 2015)

stunting2012 = ch_hh2012 %>% 
  select(livelihood_zone, stuntingZ) %>% 
  mutate(year = 2012)

stunting = bind_rows(stunting2012, stunting2015) %>%
  filter(!is.na(livelihood_zone))

stunting_levels = stunting %>% 
  filter(year == 2015) %>% 
  group_by(livelihood_zone) %>% 
  summarise(avg = mean(stuntingZ, na.rm = TRUE)) %>% 
  arrange((avg))

stunting$livelihood_zone = factor(stunting$livelihood_zone, 
                                  levels = stunting_levels$livelihood_zone)

stunting$year = factor(stunting$year, levels = c(2015, 2012))

dhs = dhs %>% 
  filter(lznum != 13)

ggplot(removeAttributes(dhs), aes(x = stunting, fill = factor(year))) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = -2) +
  # geom_rect(aes(xmin = -5, xmax = -2, ymin = 0, ymax = 0.4),
  # fill = 'red', alpha = 0.002) +
  theme_xgrid() +
  scale_x_continuous(limits = c(-5, 5)) +
  facet_wrap(~lznamee, nrow = 2) +
  theme(strip.text = element_text(size = 10), legend.position = c(0.9, 0.2))


dhs = read_dta('~/Documents/USAID/Rwanda/processeddata/DHS_2010_2015_analysis.dta')


dhs_sum = dhs %>% 
  filter(!is.na(stunted)) %>% 
  group_by(lznamee, year) %>% 
  summarise(avg = mean(stunted))

dhs_diff = dhs_sum %>% 
  spread(year, avg) %>% 
  mutate(diff = `2014` - `2010`) %>% 
  filter(!lznamee %like% 'Forest')

dhs_map = full_join(RWA_LZ$df, dhs_diff, by = c('LZNAMEE' = 'lznamee'))
plot_map(dhs_map, fill_var = 'diff') +
  scale_fill_gradientn(colours = rev(brewer.pal(10, 'RdYlBu')), limits = c(-0.15, 0.15))

