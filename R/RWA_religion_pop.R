library(readxl)
library(dplyr)
library(ggplot2)
library(forcats)
library(llamar)

df = read_excel('~/Documents/USAID/Rwanda/rawdata/RWA_2002_census_relig.xlsx', sheet = 2)

df$religion = forcats::fct_reorder(df$religion, df$`extrapolated_number`, .desc = FALSE)
df$religion = forcats::fct_relevel(df$religion, 'protestant')

df = df %>% mutate(y2 = ifelse(year == 2002, year-2, year+2))
# stacked bar
ggplot(df %>% filter(religion != "national"), aes(x = year, y = `extrapolated_number`, fill = religion)) +
         geom_bar(stat = 'identity') +
  theme_ygrid(legend.position = 'right', legend.direction = 'vertical')
       

# stacked area
ggplot(df %>% filter(religion != "national"), aes(x = year, y = `extrapolated_number`, fill = religion)) +
  geom_area(alpha = 0.4) +
  geom_bar(aes(x=y2, width = 4), stat = 'identity') +
  theme_ygrid(legend.position = 'right', legend.direction = 'vertical')


df$religion = forcats::fct_reorder(df$religion, df$`extrapolated_number`, .desc = TRUE)

# slope chart (pop)
llamar::plot_bump(df, value_var = 'extrapolated_number', region_var = 'religion', facet_var = 'religion')

# slope chart (pct)
llamar::plot_bump(df%>% filter(religion != "national"), value_var = 'pct', region_var = 'religion')

ggplot(df%>% filter(religion != "national"), aes(x = year, y = pct, size = extrapolated_number, colour = religion)) + geom_point() + theme_ygrid() + scale_size(range = c(1,12))


# stacked area: just protestants / catholics
ggplot(df %>% filter(religion %in% c("catholic", "protestant")), aes(x = religion, y = `extrapolated_number`, fill = year)) +
  geom_bar(alpha = 1, stat = 'identity', position = 'dodge') +
  theme_ygrid(legend.position = 'right', legend.direction = 'vertical')
