# Merging w/ CHAIN data and plotting


# set constants -----------------------------------------------------------
stunting_pal = rev(brewer.pal(11, 'Spectral')[1:6])
stunting_range = c(0, 0.80)

IM_pal = c(grey15K, colorRampPalette(c('#66C2A5', '#1E4436'))(9)) # color ramp from Spectral #9 to Spectral #10 w/ grey15K appended to the start
IM_range = c(0, 9)

size_stunting = 5
size_IM = 3
size_IM_label = 2.5

squish_factor = 0.02
x_init = 0.825

# import CHAIN data -------------------------------------------------------

chain = readRDS('~/GitHub/RwandaCHAIN/Rwanda-CHAIN/data/RW_projects_adm2_2016-06-14.rds')


nutrition_tidy = chain %>% 
  filter(result %like% 'nutrition') %>% 
  group_by(shortName, District) %>% 
  summarise(n = n()) %>% 
  mutate(worksIn = n > 0)

# figure out max / district (== 9)
nutrition_tidy %>% 
  group_by(District) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# For rank-ordering:
nutrit_order = nutrition_tidy %>% 
  group_by(shortName) %>% 
  summarise(n = n()) 

nutrit_order = nutrit_order %>% 
  mutate(x_posit = (nrow(nutrit_order) - row_number(n)) * squish_factor + x_init) %>% 
  select(-n) %>% 
  spread(shortName, x_posit)

nutrition = nutrition_tidy %>% 
  select(-n) %>% 
  spread(key = shortName, value = worksIn) 

# -- Convert NAs to 0s --
nutrition[is.na(nutrition)] = 0
 
# -- Calculate total column --
nutrition = nutrition %>% 
  mutate(total = AEE + Caritas + FXB + HarvestPlus + IILP + INWA + ISVP +
           OFSP + PSDAG + `RDCP II` + `ROADS III` + RRSA + RSMP)

# -- Merge CHAIN locations + stunting at the district level --
chain = full_join(stunting_admin2,nutrition, by = c('admin2' = 'District'))


# plot CHAIN relative to stunting -----------------------------------------
# This is sorta stupid to 
# CFSVA data

ggplot(chain) +
  # -- error bars --
  geom_segment(aes(x = lb_dhs, xend = ub_dhs, 
                   y = fct_reorder(admin2, stunting_dhs), 
                   yend = fct_reorder(admin2, stunting_dhs)), 
               colour = grey50K, alpha = 0.2,
               size = 1.25) +
  
  # -- country average --
  geom_vline(xintercept = 0.38,
             size = 0.25, colour = grey70K) + # note: stroke gets doubled when saved
  
  # -- averages (squares) --
  geom_point(aes(x = stunting_dhs, 
                 y = fct_reorder(admin2, stunting_dhs), 
                 fill = stunting_dhs),
             size = size_stunting, shape = 22, colour = grey90K) +
  
  # -- overlay %s on top --
  geom_text(aes(x = stunting_dhs, 
                y = fct_reorder(admin2, stunting_dhs), 
                label = round(stunting_dhs * 100, 0)),
            family = font_normal,
            size = 2, colour = grey90K) +
  
  # -- binary values for CHAIN projects --
  geom_point(aes(x = nutrit_order$AEE, 
                 y = fct_reorder(admin2, stunting_dhs),
                 colour = AEE),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$Caritas, 
                 y = fct_reorder(admin2, stunting_dhs),
                 colour = Caritas),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$FXB, 
                 y = fct_reorder(admin2, stunting_dhs),
                 colour = FXB),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$HarvestPlus, 
                 y = fct_reorder(admin2, stunting_dhs),
                 colour = HarvestPlus),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$IILP, 
                 y = fct_reorder(admin2, stunting_dhs),
                 colour = IILP),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$INWA, 
                 y = fct_reorder(admin2, stunting_dhs),
                 colour = INWA),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$ISVP, 
                 y = fct_reorder(admin2, stunting_dhs),
                 colour = ISVP),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$OFSP, 
                 y = fct_reorder(admin2, stunting_dhs),
                 colour = OFSP),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$PSDAG, 
                 y = fct_reorder(admin2, stunting_dhs),
                 colour = PSDAG),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$RRSA, 
                 y = fct_reorder(admin2, stunting_dhs),
                 colour = RRSA),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$RSMP, 
                 y = fct_reorder(admin2, stunting_dhs),
                 colour = RSMP),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$`RDCP II`, 
                 y = fct_reorder(admin2, stunting_dhs),
                 colour = `RDCP II`),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$`ROADS III`, 
                 y = fct_reorder(admin2, stunting_dhs),
                 colour = `ROADS III`),
             size = size_IM) +
  
  # -- Text labels --
  geom_text(aes(x = nutrit_order$AEE, 
                 y = 0,
                 label = 'AEE'),
             size = size_IM_label,
            family = font_light,
            colour = grey70K) +
  geom_text(aes(x = nutrit_order$Caritas, 
                y = 0,
                label = 'Caritas'),
            size = size_IM_label,
            family = font_light,
            colour = grey70K) +
  geom_text(aes(x = nutrit_order$FXB, 
                y = 0,
                label = 'FXB'),
            size = size_IM_label,
            family = font_light,
            colour = grey70K) +
  geom_text(aes(x = nutrit_order$HarvestPlus, 
                y = 0,
                label = 'HarvestPlus'),
            size = size_IM_label,
            family = font_light,
            colour = grey70K) +
  geom_text(aes(x = nutrit_order$IILP, 
                y = 0,
                label = 'IILP'),
            size = size_IM_label,
            family = font_light,
            colour = grey70K) +
  geom_text(aes(x = nutrit_order$INWA, 
                y = 0,
                label = 'INWA'),
            size = size_IM_label,
            family = font_light,
            colour = grey70K) +
  geom_text(aes(x = nutrit_order$ISVP, 
                y = 0,
                label = 'ISVP'),
            size = size_IM_label,
            family = font_light,
            colour = grey70K) +
  geom_text(aes(x = nutrit_order$OFSP, 
                y = 0,
                label = 'OFSP'),
            size = size_IM_label,
            family = font_light,
            colour = grey70K) +
  geom_text(aes(x = nutrit_order$PSDAG, 
                y = 0,
                label = 'PSDAG'),
            size = size_IM_label,
            family = font_light,
            colour = grey70K) +
  geom_text(aes(x = nutrit_order$`RDCP II`, 
                y = 0,
                label = 'RDCP'),
            size = size_IM_label,
            family = font_light,
            colour = grey70K) +
  geom_text(aes(x = nutrit_order$`ROADS III`, 
                y = 0,
                label = 'ROADS'),
            size = size_IM_label,
            family = font_light,
            colour = grey70K) +
  geom_text(aes(x = nutrit_order$RRSA, 
                y = 0,
                label = 'RRSA'),
            size = size_IM_label,
            family = font_light,
            colour = grey70K) +
  geom_text(aes(x = nutrit_order$RSMP, 
                y = 0,
                label = 'RSMP'),
            size = size_IM_label,
            family = font_light,
            colour = grey70K) +
  

  # -- totals --
    geom_point(aes(x = max(nutrit_order) + 2 * squish_factor, 
                 y = fct_reorder(admin2, stunting_dhs),
                 colour = total),
             size = size_stunting) +
  geom_text(aes(x = max(nutrit_order) + 2 * squish_factor, 
                y = fct_reorder(admin2, stunting_dhs), 
                label = total),
            family = font_normal,
            size = 3, colour = grey10K) +
  
  
  # -- scales --
  # scale_colour_manual(values = c('0' = grey15K, '1' = brewer.pal(11, 'Spectral')[9])) +
  scale_colour_gradientn(colours = IM_pal, limits = IM_range) +
  scale_fill_gradientn(colours = stunting_pal, limits = stunting_range) +
  scale_x_continuous(name = 'percent of stunted children (under 5 years old)',
                     labels = scales::percent,
                     breaks = c(0, 0.25, 0.50, 0.75)) +
                     # breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
  # -- themes --
  theme_xygridlight() +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_text(hjust = 0, family = font_normal),
        axis.text = element_text(family = font_light),
        panel.grid.major.y = element_line(size = 0.05, color = grey50K) # lighter and half as thick
  )


save_plot('~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/RWA_02_CHAIN_stunting_dhs.pdf',
          width = 10, height = 7)
# 