# san2012/2015 ------------------------------------------------------------
hh2012 = factorize(hh2012, hh2012_raw, 'fews_code', 'livelihood_zone')

san2012 = ch_hh2012 %>% group_by(livelihood_zone) %>% 
  summarise(san= mean(impr_unshared_toilet)) %>% 
  arrange(desc(san)) %>% 
  mutate(year = 2012)

san2015 = hh %>% 
  group_by(livelihood_zone) %>% 
  summarise(san = mean(impr_unshared_toilet)) %>% 
  arrange(desc(san)) %>% 
  mutate(year = 2015)

san = bind_rows(san2012, san2015)

san_untidy = full_join(san2012 %>% select(-year), san2015 %>% 
                         select(-year), by = c("livelihood_zone")) %>% 
  rename(san2012 = san.x,
         san2015 = san.y) %>% 
  mutate(san_diff = san2015 - san2012)

san = san %>% 
  mutate(year = as.character(year))

stunting_san = full_join(san, stunting_comb, by = c('livelihood_zone', 'year'))


stunting_san_untidy = full_join(san_untidy, stunting_untidy)

ggplot(stunting_san_untidy, aes(x = san_diff, y = diff)) +
  geom_smooth(method='lm', colour = 'red', fill = NA) +
  geom_point(size = 4) +
  geom_text(aes(label = livelihood_zone),
            size = 2,
            hjust = 1, 
            nudge_y = 0.005) +
  theme_xygrid() +
  xlim(c(0, .6))


# merge w/ 2015 -----------------------------------------------------------

ggplot(stunting_comb) +
  geom_segment(aes(x = `2012`, xend  = y2, 
                   y = livelihood_zone, yend = livelihood_zone),
               size = 0.5, 
               arrow = arrow(length = unit(0.03, "npc")),
               colour = grey60K, 
               data = stunting_untidy) +
  geom_point(aes(x = stunting, y = livelihood_zone,
                 color = year, shape = year, fill = year),
             size = 8, colour = grey90K) +
  geom_point(aes(x = b, y = livelihood_zone),
             # width = 0,
             size = 8, colour = grey90K,
             fill = brewer.pal(9, 'Spectral')[9],
             shape = 23,
             data = stunting_lz_dhs) +
  # geom_text(aes(x = stunting, y = livelihood_zone,
  #                color = year, shape = year, fill = year,
  #                label = percent(stunting, 0)),
  #           colour = grey75K,
  #           size = 3) +
  theme_xgrid() +
  scale_shape_manual(values = c(21, 23)) +
  scale_x_continuous(labels = percent, limits = c(0.2, 0.62),
                     breaks = seq(0.2, 0.6, by = 0.2)) +
  scale_fill_manual(values = c('2012' = 'white', '2015' = brewer.pal(9, 'Spectral')[1])) +
  theme(axis.text.y = element_text(size = 10),
        axis.title.x = element_blank())

