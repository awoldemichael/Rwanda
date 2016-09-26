# Merging w/ CHAIN data and plotting


# set constants -----------------------------------------------------------
stunting_pal = rev(brewer.pal(11, 'Spectral')[1:6])
stunting_range = c(0, 0.8)

IM_pal = c(grey15K, colorRampPalette(c('#66C2A5', '#1E4436'))(9)) # color ramp from Spectral #9 to Spectral #10 w/ grey15K appended to the start
IM_range = c(0, 9)

size_IM = 5

squish_factor = 0.2
x_init = 0.9

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
  summarise(n = n()) %>% 
  arrange(n) %>% 
  mutate(x_posit = (row_number(n) - 1) * squish_factor + x_init) %>% 
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
  geom_segment(aes(x = lb_cfsva, xend = ub_cfsva, 
                   y = fct_reorder(admin2, stunting_cfsva), 
                   yend = fct_reorder(admin2, stunting_cfsva)), 
               colour = grey50K, alpha = 0.2,
               size = 1.25) +
  # -- averages (squares) --
  geom_point(aes(x = stunting_cfsva, 
                 y = fct_reorder(admin2, stunting_cfsva), 
                 fill = stunting_cfsva),
             size = 4, shape = 22, colour = grey90K) +
  
  # -- overlay %s on top --
  geom_text(aes(x = stunting_cfsva, 
                y = fct_reorder(admin2, stunting_cfsva), 
                label = round(stunting_cfsva * 100, 0)),
            size = 2, colour = grey90K) +
  
  # -- binary values for CHAIN projects --
  geom_point(aes(x = nutrit_order$AEE, 
                 y = fct_reorder(admin2, stunting_cfsva),
                 colour = AEE),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$Caritas, 
                 y = fct_reorder(admin2, stunting_cfsva),
                 colour = Caritas),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$FXB, 
                 y = fct_reorder(admin2, stunting_cfsva),
                 colour = FXB),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$HarvestPlus, 
                 y = fct_reorder(admin2, stunting_cfsva),
                 colour = HarvestPlus),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$IILP, 
                 y = fct_reorder(admin2, stunting_cfsva),
                 colour = IILP),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$INWA, 
                 y = fct_reorder(admin2, stunting_cfsva),
                 colour = INWA),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$ISVP, 
                 y = fct_reorder(admin2, stunting_cfsva),
                 colour = ISVP),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$OFSP, 
                 y = fct_reorder(admin2, stunting_cfsva),
                 colour = OFSP),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$PSDAG, 
                 y = fct_reorder(admin2, stunting_cfsva),
                 colour = PSDAG),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$RRSA, 
                 y = fct_reorder(admin2, stunting_cfsva),
                 colour = RRSA),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$RSMP, 
                 y = fct_reorder(admin2, stunting_cfsva),
                 colour = RSMP),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$`RDCP II`, 
                 y = fct_reorder(admin2, stunting_cfsva),
                 colour = `RDCP II`),
             size = size_IM) +
  geom_point(aes(x = nutrit_order$`ROADS III`, 
                 y = fct_reorder(admin2, stunting_cfsva),
                 colour = `ROADS III`),
             size = size_IM) +

  # -- totals --
    geom_point(aes(x = max(nutrit_order) + squish_factor, 
                 y = fct_reorder(admin2, stunting_cfsva),
                 colour = total),
             size = size_IM) +
  geom_text(aes(x = max(nutrit_order) + squish_factor, 
                y = fct_reorder(admin2, stunting_cfsva), 
                label = total),
            size = 3, colour = grey10K) +
  
  
  # -- scales --
  # scale_colour_manual(values = c('0' = grey15K, '1' = brewer.pal(11, 'Spectral')[9])) +
  scale_colour_gradientn(colours = IM_pal, limits = IM_range) +
  scale_fill_gradientn(colours = stunting_pal, limits = stunting_range) +
  scale_x_continuous(name = 'percent of stunted children (under 5 years old)',
                     labels = scales::percent,
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
  # -- themes --
  theme_xygridlight() +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_text(hjust = 0),
        panel.grid.major.y = element_line(size = 0.05, color = grey50K) # lighter and half as thick
  )


# save_plot('~/Creative Cloud Files/MAV/Projects/RWA_LAM-stunting_2016-09/exported_fromR/RWA_02_CHAIN_stunting.pdf')
# 