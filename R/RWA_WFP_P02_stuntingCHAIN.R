# Merging w/ CHAIN data and plotting

ggplot(stunting_admin2, aes(x = isStunted, y = fct_reorder(admin2, isStunted), fill = isStunted)) +
  geom_segment(aes(x = lb, xend = ub, yend = fct_reorder(admin2, isStunted)), 
               colour = grey50K, alpha = 0.2,
               size = 1.5) +
  geom_point(size = 4, shape = 22, colour = grey90K) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, 'Spectral')[1:6])) +
  theme_xgrid()


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

chain = full_join(stunting_admin2,nutrition, by = c('admin2' = 'District'))

ggplot(chain, aes(x = isStunted, y = fct_reorder(admin2, isStunted), fill = isStunted)) +
  geom_segment(aes(x = lb, xend = ub, yend = fct_reorder(admin2, isStunted)), 
               colour = grey50K, alpha = 0.2,
               size = 1.25) +
  geom_point(size = 4, shape = 22, colour = grey90K) +
  geom_point(aes(x = 0.9, y = fct_reorder(admin2, isStunted),
                 colour = factor(RSMP)),
    size = 5) +
  geom_point(aes(x = 1, y = fct_reorder(admin2, isStunted),
                 colour = factor(AEE)),
             size = 5) +
  geom_point(aes(x = 1.1, y = fct_reorder(admin2, isStunted),
                 colour = factor(OFSP)),
             size = 5) +
  geom_point(aes(x = 1.2, y = fct_reorder(admin2, isStunted),
                 colour = factor(PSDAG)),
             size = 5) +
  geom_point(aes(x = 1.3, y = fct_reorder(admin2, isStunted),
                 colour = factor(`ROADS III`)),
             size = 5) +
  scale_colour_manual(values = c('0' = grey15K, '1' = brewer.pal(11, 'Spectral')[9])) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, 'Spectral')[1:6])) +
  theme_xgrid()

ggplot(nutrition, aes(x = shortName, y = fct_reorder(District, shortName))) +
  geom_point(aes(
                 colour = factor(worksIn)),
             size = 5) +
  scale_colour_manual(values = c('0' = grey15K, '1' = brewer.pal(11, 'Spectral')[9])) +
  theme_xgrid()
