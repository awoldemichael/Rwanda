a#-------------------------------------------------------------------------------
# Name:		  02_livelihoodzone
# Purpose:	Import and recode livelihood zones
# Author:	  Tim Essam, Ph.D.
# Created:	2016/08/11
# Owner:	  USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
#-------------------------------------------------------------------------------

library(readr)
library(car)
lvhood <- read_csv("~/GitHub/RwandaLAM/Dataout/RWA_DHS_Livelihoods.csv")


# These DHS cluster offsets fall outside of livelihood zones or within
# National Park boundaries. We use the nearest livelihood zone (from FEWSNET)

lvhood = lvhood %>% 
  mutate(lvhood_zone = factor(lvhood$LZNAMEE))




lvhood$lvhood_zone = ifelse(lvhood$DHSCLUST %in% c(418, 161, 296, 81, 171), 9,
                              ifelse(lvhood$DHSCLUST %in% c(101, 317), 3, 
                                     ifelse(lvhood$DHSCLUST %in% c(199, 347, 281), 16, lvhood$lvhood_zone)))

                           

