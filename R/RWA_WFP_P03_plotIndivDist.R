highlight_colour = brewer.pal(11, 'Spectral')[2]
  
districts = unique(RWA_admin2$df$District)

for (i in seq_along(districts)){
  
  df = RWA_admin2$df %>% 
    filter(District == districts[i])
  
  ggplot(df, aes(x = long, y = lat, group = group)) + 
    
    # -- base fill the country --
    geom_polygon(fill = grey15K, data = RWA_admin0$df) +
    # -- themes --
    theme_void() + 
    coord_equal() +
    
    
    # -- choropleth over regions --
    geom_polygon(fill = highlight_colour) 
  
  save_plot(paste0('~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/', districts[i], '.pdf'),
            width = 2, 
            height = 2)
  
}