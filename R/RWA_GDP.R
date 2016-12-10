
# Rwanda GDP, according to the World Bank. --------------------------------

library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(llamar)

line_colour = '#353839'
text_colour = grey60K
line_size = 2
font_size = 15
dot_size = 4.5
text_size = 6
dot_stroke = line_size/4
colour1 = grey30K
colour2 = grey70K

gdp = read_excel('~/Documents/USAID/Rwanda/rawdata/RW_WB_GDP.xlsx')

gdp = gdp %>% 
  gather(year, gdp, -`Country Name`, -`Country Code`, -`Indicator Name`, -`Indicator Code`) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year >= 1990,
         !is.na(gdp)) %>% 
  mutate(gdp = gdp/1e9)


ggplot(gdp, aes(x = year, y = gdp, label = paste0('$', round_exact(gdp, 1), 'B'),
                group = `Country Name`)) +
  geom_line(size = line_size, colour = line_colour) +
  
  geom_point(size = dot_size, shape = 21,
             stroke = dot_stroke, colour = line_colour,
             fill = colour1, data = gdp %>% filter(year == min(gdp$year))) +
  geom_text(size = text_size, family = 'Lato',
            nudge_y = 1.5,
             colour = colour1, data = gdp %>% filter(year == min(gdp$year))) +
  
  geom_point(size = dot_size, shape = 21,
             stroke = dot_stroke, colour = line_colour,
             fill = colour2, data = gdp %>% filter(year == max(gdp$year))) +
  geom_text(size = text_size, family = 'Lato',
            nudge_y = -1.75,
            colour = colour2, 
            data = gdp %>% filter(year == 2015)) +
  
  
  ylim(c(0, max(gdp$gdp))) +
  scale_x_continuous(expand = c(0.1, 0)) +
  ggtitle('Rwanda GDP growth (1990-2015)') +
  labs(caption = 'Source: World Bank') +
  theme_blank() +
  theme_stroke() +
  theme(text = element_text(size = font_size, family = 'Lato',
                            colour = text_colour),
        title = element_text(size = font_size, family = 'Lato',
                             colour = text_colour),
        plot.caption = element_text(size = font_size*0.7))

save_plot('~/GitHub/Rwanda-results/img/gdp.png', width = 4, height = 2)
