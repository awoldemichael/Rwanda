# PCA! Thanks to Luke
num_PCs = 3

pca_hh = ch_hh %>% 
  filter(!is.na(isStunted)) %>% 
  mutate(lz0 = ifelse(livezone == 0, 1, 0),
         lz1 = ifelse(livezone == 1, 1, 0),
         lz2 = ifelse(livezone == 2, 1, 0),
         lz3 = ifelse(livezone == 3, 1, 0),
         lz4 = ifelse(livezone == 4, 1, 0),
         lz5 = ifelse(livezone == 5, 1, 0),
         lz6 = ifelse(livezone == 6, 1, 0),
         lz7 = ifelse(livezone == 7, 1, 0),
         lz8 = ifelse(livezone == 8, 1, 0),
         lz9 = ifelse(livezone == 9, 1, 0),
         lz10 = ifelse(livezone == 10, 1, 0),
         lz11 = ifelse(livezone == 11, 1, 0),
         lz12 = ifelse(livezone == 12, 1, 0)) %>% 
  select(stuntingZ,
         age_months, 
         sex, 
         # interview_date, 
         rural_cat, 
         kids_under5, 
         crowding, 
         fem_head, 
         head_age, 
         numWomen_18plus, 
         impr_unshared_toilet, 
         impr_water_30min, 
         diarrhea, 
         low_birthwt, 
         health_less_60min, 
         TLU, 
         land_size_cat, 
         hh_garden, 
         head_education_cat, 
         FCS, 
         CSI_cat, 
         months_food_access, 
         mother_age, 
         mother_education, 
         mother_mosquito_net, 
         stunted_mother, 
         shock_drought, 
         shock_illness, 
         food_assistance, 
         financial_assistance, 
         ag_assistance, 
         log_pcexp, 
         contains('lz'),
         -lz_name
         # hh_occup_cat
  ) %>% 
  mutate(sex = as.numeric(sex), 
         rural_cat = as.numeric(rural_cat), 
         kids_under5 = as.numeric(kids_under5), 
         health_less_60min = as.numeric(health_less_60min), 
         land_size_cat = as.numeric(land_size_cat), 
         head_education_cat = as.numeric(head_education_cat), 
         CSI_cat = as.numeric(CSI_cat), 
         mother_education = as.numeric(mother_education), 
         food_assistance = as.numeric(food_assistance)
  )

pca_hh = na.omit(pca_hh)

# independent vars
indpt = pca_hh %>% select(-stuntingZ)
dpndt = pca_hh %>% select(stuntingZ)
dpndt = as.vector(scale(dpndt))

# calc PCA ----------------------------------------------------------------

pca = prcomp(indpt, center = TRUE, scale = TRUE)

pct_var = data.frame(var = (pca$sdev)^2 / sum(pca$sdev^2) , x = 1:43)
ggplot(pct_var, aes(x=  x, y = var)) + 
  geom_bar(stat='identity') + 
  ylab('% variance') + 
  theme_ygrid() + 
  scale_y_continuous(labels = percent)


summary(pca)

# project stunting onto PCs -----------------------------------------------
# Select 3 PCs (~ 20% of variance)
eigenvectors = pca$rotation[,1:num_PCs]

# Matrix multiplication to convert to a [2800 x 3] matrix
projected_indpt = as.matrix(indpt) %*% eigenvectors

# Multiply by the stunting score
pos =  colMeans(projected_indpt * (dpndt > 0) * dpndt)

neg =  -1 * colMeans(projected_indpt * (dpndt < 0) * dpndt)

# relative contribution of each PC to malnutrition signal.
dipole = pos - neg

# PC1         PC2         PC3 
# -2.22041662  0.18120252  0.08619781 

# contribution of each variable to stunting 
var_contrib = eigenvectors %*% dipole

var_contrib = data.frame(var = row.names(var_contrib), factor = var_contrib) 


# 2012 --------------------------------------------------------------------
pca_hh = ch_hh2012 %>% 
  filter(!is.na(isStunted)) %>% 
  mutate(lz0 = ifelse(fews_code == 0, 1, 0),
         lz1 = ifelse(fews_code == 1, 1, 0),
         lz2 = ifelse(fews_code == 2, 1, 0),
         lz3 = ifelse(fews_code == 3, 1, 0),
         lz4 = ifelse(fews_code == 4, 1, 0),
         lz5 = ifelse(fews_code == 5, 1, 0),
         lz6 = ifelse(fews_code == 6, 1, 0),
         lz7 = ifelse(fews_code == 7, 1, 0),
         lz8 = ifelse(fews_code == 8, 1, 0),
         lz9 = ifelse(fews_code == 9, 1, 0),
         lz10 = ifelse(fews_code == 10, 1, 0),
         lz11 = ifelse(fews_code == 11, 1, 0),
         lz12 = ifelse(fews_code == 12, 1, 0)) %>% 
  select(stuntingZ,
         age_months, 
         sex, 
         # interview_date, 
         rural_cat, 
         kids_under5, 
         crowding, 
         fem_head, 
         head_age, 
         numWomen_18plus, 
         impr_unshared_toilet, 
         impr_water_30min, 
         diarrhea, 
         low_birthwt, 
         health_less_60min, 
         TLU, 
         land_size_cat, 
         hh_garden, 
         head_education_cat, 
         FCS, 
         CSI_cat, 
         months_food_access, 
         mother_age, 
         mother_education, 
         mother_mosquito_net, 
         stunted_mother, 
         shock_drought, 
         shock_illness, 
         food_assistance, 
         financial_assistance, 
         ag_assistance, 
         log_pcexp, 
         contains('lz')
         # hh_occup_cat
  ) %>% 
  mutate(sex = as.numeric(sex), 
         fem_head = as.numeric(fem_head),
         rural_cat = as.numeric(rural_cat), 
         kids_under5 = as.numeric(kids_under5), 
         health_less_60min = as.numeric(health_less_60min), 
         land_size_cat = as.numeric(land_size_cat), 
         head_education_cat = as.numeric(head_education_cat), 
         CSI_cat = as.numeric(CSI_cat), 
         mother_education = as.numeric(mother_education), 
         food_assistance = as.numeric(food_assistance)
  )

pca_hh = na.omit(pca_hh)

# independent vars
indpt = pca_hh %>% select(-stuntingZ)
dpndt = pca_hh %>% select(stuntingZ)
dpndt = as.vector(scale(dpndt))

# calc PCA ----------------------------------------------------------------

pca = prcomp(indpt, center = TRUE, scale = TRUE)

pct_var = data.frame(var = (pca$sdev)^2 / sum(pca$sdev^2) , x = 1:43)

ggplot(pct_var, aes(x=  x, y = var)) + 
  geom_bar(stat='identity') + 
  ylab('% variance') + 
  theme_ygrid() + 
  scale_y_continuous(labels = percent)

summary(pca)


# project stunting onto PCs -----------------------------------------------
# Select 3 PCs (~ 20% of variance)
eigenvectors = pca$rotation[,1:num_PCs]

# Matrix multiplication to convert to a [2800 x 3] matrix
projected_indpt = as.matrix(indpt) %*% eigenvectors

# Multiply by the stunting score
pos =  colMeans(projected_indpt * (dpndt > 0) * dpndt)

neg =  -1 * colMeans(projected_indpt * (dpndt < 0) * dpndt)

# relative contribution of each PC to malnutrition signal.
dipole = pos - neg

# PC1         PC2         PC3 
# -2.22041662  0.18120252  0.08619781 

# contribution of each variable to stunting 
var_contrib2 = eigenvectors %*% dipole

var_contrib2 = data.frame(var = row.names(var_contrib2), factor = var_contrib2) 



# compare years -----------------------------------------------------------
# (re-ran for 2012; will document properly later)

var_contrib = full_join(var_contrib, var_contrib2, by = 'var')

var_contrib = var_contrib %>% 
  rename(`2015` = factor.x,
         `2012` = factor.y)

var_contrib$var = fct_reorder(var_contrib$var, var_contrib$`2015`)

var_contrib = var_contrib %>% 
  gather(year, factor, - var)


var_contrib_untidy = var_contrib %>% 
  spread(year, factor)

ggplot(var_contrib, aes(x = factor, y = var)) +
  geom_segment(aes(x = `2012`, xend = `2015`, y = var, yend = var),
               colour = grey90K, size = 0.1, data = var_contrib_untidy) +
  geom_vline(xintercept = 0, colour = grey90K, size = 0.5) +
  geom_point(aes(shape = year),
             fill = 'white',
             alpha = 1,
             size = 4) +
  geom_point(aes(fill = factor,
                 alpha = year,
                 shape = year),
             size = 4) +
  scale_fill_gradientn(colours = brewer.pal(10, 'RdYlBu'), 
                       limits = c(-max(abs(var_contrib$factor)), max(abs(var_contrib$factor)))) +
  scale_shape_manual(values = c('2012' = 21, '2015' = 23)) +
  scale_alpha_manual(values = c('2012' = 0.4, '2015' = 1)) +
  theme_xgrid()
