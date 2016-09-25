# Merging w/ CHAIN data and plotting

ggplot(stunting_admin2, aes(x = stunting_cfsva, y = fct_reorder(admin2, stunting_cfsva), 
                            fill = stunting_cfsva)) +
  geom_segment(aes(x = lb_cfsva, xend = ub_cfsva, yend = fct_reorder(admin2, stunting_cfsva)), 
               colour = grey50K, alpha = 0.2,
               size = 1.5) +
  geom_point(size = 4, shape = 22, colour = grey90K) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, 'Spectral')[1:6])) +
  theme_xygrid()


# import CHAIN data -------------------------------------------------------

chain = readRDS('~/GitHub/RwandaCHAIN/Rwanda-CHAIN/data/RW_projects_adm2_2016-06-14.rds')

nutrition = chain %>% 
  filter(result %like% 'nutrition') %>% 
  group_by(shortName, District) %>% 
  summarise(n = n()) %>% 
  mutate(worksIn = n > 0)

nutrition = nutrition %>% 
  select(-n) %>% 
  spread(key = shortName, value = worksIn)

View(data.frame(colSums(nutrition %>% select(-District))))
# -- Convert NAs to 0s --
nutrition[is.na(nutrition)] = 0

# -- Merge CHAIN locations + stunting at the district level --
chain = full_join(stunting_admin2,nutrition, by = c('admin2' = 'District'))


# plot CHAIN relative to stunting -----------------------------------------

ggplot(chain) +
  # -- error bars --
  geom_segment(aes(x = lb_cfsva, xend = ub_cfsva, 
                   y = fct_reorder(admin2, stunting_cfsva), yend = fct_reorder(admin2, stunting_cfsva)), 
               colour = grey50K, alpha = 0.2,
               size = 1.25) +
  # -- averages (squares) --
  geom_point(aes(x = stunting_cfsva, 
                 y = fct_reorder(admin2, stunting_cfsva), 
                 fill = stunting_cfsva),
             size = 4, shape = 22, colour = grey90K) +
  
  # -- binary values for CHAIN projects --
  geom_point(aes(x = 0.9, 
                 y = fct_reorder(admin2, stunting_cfsva),
                 colour = factor(RSMP)),
             size = 5) +
  geom_point(aes(x = 1, y = fct_reorder(admin2, stunting_cfsva),
                 colour = factor(AEE)),
             size = 5) +
  geom_point(aes(x = 1.1, y = fct_reorder(admin2, stunting_cfsva),
                 colour = factor(OFSP)),
             size = 5) +
  geom_point(aes(x = 1.2, y = fct_reorder(admin2, stunting_cfsva),
                 colour = factor(PSDAG)),
             size = 5) +
  geom_point(aes(x = 1.3, y = fct_reorder(admin2, stunting_cfsva),
                 colour = factor(`ROADS III`)),
             size = 5) +
  
  # -- scales --
  scale_colour_manual(values = c('0' = grey15K, '1' = brewer.pal(11, 'Spectral')[9])) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, 'Spectral')[1:6])) +
  scale_x_continuous(name = 'percent of stunted children (under 5 years old)',
                     labels = scales::percent,
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
  # -- themes --
  theme_xygridlight() +
  theme(axis.title.y = element_blank(), 
          panel.grid.major.y = element_line(size = 0.05, color = grey50K) # lighter and half as thick
  )
