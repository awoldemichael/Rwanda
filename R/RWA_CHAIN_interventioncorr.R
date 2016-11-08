x = sectors %>% 
  select(partner, program, intervention = intervention.y, sector) %>% 
  distinct()

x = x %>% 
  mutate(exists = 1) 

y  = x %>% 
  ungroup() %>% 
  group_by(partner, program, sector) %>% 
  summarise(total = sum(exists)) %>% 
  arrange(desc(total))

x = x %>% 
  spread(intervention, exists)




x = x %>% 
  group_by(partner, program, sector) %>% 
  mutate_each(funs(coalesce(., 0)))

cor_interv = cor(x  %>% ungroup() %>% select(-partner, -program, -sector))
                 
cor_interv = data.frame(cor_interv)

cor_interv$x = row.names(cor_interv)
  
cor_interv = cor_interv %>% gather(y, corr, -x)

cor_interv = cor_interv %>% filter(corr < 1)

ggplot(cor_interv, aes(x = forcats::fct_reorder(x, corr), y = forcats::fct_reorder(y, corr), fill = corr)) + 
  geom_tile() +
  scale_fill_gradientn(colours = brewer.pal(11, 'RdYlBu'), values = c(-1, 1)) +
  theme_xylab() +
  theme(axis.text.x = element_text(hjust = 1, angle = 45))
