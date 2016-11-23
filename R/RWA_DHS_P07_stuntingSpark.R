# Quick sparkline of national stunting over time in Rwanda, from DHS stats.
# Laura Hughes, 20 November 2016, lhughes@usaid.gov


# libraries ---------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(llamar)

# pull in DHS data --------------------------------------------------------
rw_stunting = llamar::loadDHS(breakdown = 'national', indicators = 'CN_NUTS_C_HA2', countries = 'RW')


# Pulling in all Africa data from WHO -------------------------------------
# source: http://apps.who.int/gho/data/node.main.NUTWHOREGIONS?lang=en
africa = data.frame(SurveyYear = c(2015,
                                   2010,
                                   2005,
                                   2000,
                                   1995,
                                   1990),
                    stunting = c(37.8,
                                 40.5,
                                 43.2,
                                 46.0,
                                 48.8,
                                 51.6)) %>% 
  mutate(pct_stunting = stunting/100)

# # UN estimates: all Africa
# c(31.6, 
# 33.9, 
# 36.1, 
# 38.3, 
# 40.2, 
# 42.3) 
# 
# # UN estimates: E. Africa
# c(37.5,
# 40.1,
# 42.9,
# 45.6,
# 48.4,
# 51.2)

# clean data --------------------------------------------------------------
rw_stunting = rw_stunting %>% 
  mutate(pct_stunting = Value/100) # percentize


# aesthetics --------------------------------------------------------------
stroke_size = 1
dot_size = 4
stroke_colour = '#353839'
label_colour = grey60K
label_size = 4

# plot --------------------------------------------------------------------
minYear = min(rw_stunting$SurveyYear)
maxYear = max(rw_stunting$SurveyYear)

ggplot(rw_stunting, aes(x = SurveyYear, y = pct_stunting)) +
  geom_line(size = stroke_size, colour = stroke_colour) +
  geom_line(data = africa, colour = label_colour) +
  geom_point(size = dot_size, fill = 'white', 
             colour = stroke_colour, stroke = stroke_size/3,
             shape = 21,
             data = rw_stunting %>% filter(SurveyYear == maxYear)) +
  geom_text(aes(label = paste0(SurveyYearLabel, ': \n', percent(pct_stunting, 0))),
             size = label_size, 
             colour = label_colour, 
            nudge_y = 0.05,
             data = rw_stunting %>% filter(SurveyYear %in% c(minYear, maxYear))) +
  geom_point(size = dot_size, fill = label_colour, 
             colour = stroke_colour, stroke = stroke_size/3,
             shape = 21,
             data = rw_stunting %>% filter(SurveyYear == minYear)) +
  scale_y_continuous(limits = c(0, max(rw_stunting$pct_stunting) + 0.1)) +
  scale_x_continuous(expand = c(0.1, 0.1)) +
  theme_blank()

# Export
save_plot('~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/DHS_stunting_spark', saveBoth = TRUE,
          width = 4.5, height = 2)
