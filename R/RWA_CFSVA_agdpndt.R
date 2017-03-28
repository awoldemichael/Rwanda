# Calculate where hh are dependent on ag

ag_dpdt =  hh  %>% 
  group_by(admin2, hh_occup_cat %in% c('Unskilled', 'Skilled')) %>%
  summarise(n=n()) %>% 
  ungroup() %>% 
  group_by(admin2) %>% 
  mutate(pct = n/sum(n))

ag_dpdt = ag_dpdt %>% rename(ag = `hh_occup_cat %in% c("Unskilled", "Ski...`) %>% filter(ag == FALSE)

ag_map = full_join(RWA_admin2$df, ag_dpdt, by = c("District" = "admin2"))

geocenter::plot_map(ag_map, fill_var = 'pct') + scale_fill_gradientn(colours = (brewer.pal(9, "Greens"))) + theme_blank(legend.position = c(0.1, 0.9))


pct_ownland = hh %>% group_by(admin2) %>% summarise(pct = mean(own_land))

land_map = full_join(RWA_admin2$df, pct_ownland, by = c("District" = "admin2"))

  geocenter::plot_map(land_map, fill_var = 'pct') + scale_fill_gradientn(colours = (brewer.pal(9, "YlGn"))) + theme_blank(legend.position = c(0.1, 0.9))

pcexp = hh %>% group_by(admin2) %>% summarise(pct=mean(log_pcexp, na.rm=T)) %>% arrange(desc(pct))
pc_map = full_join(RWA_admin2$df, pct_ownland, by = c("District" = "admin2"))
geocenter::plot_map(ag_map, fill_var = 'pct') + scale_fill_gradientn(colours = (brewer.pal(9, "Greens"))) + theme_blank(legend.position = c(0.1, 0.9))
