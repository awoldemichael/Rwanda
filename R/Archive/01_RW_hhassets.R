a#-------------------------------------------------------------------------------
# Name:		01_hhvars
# Purpose:	recode and rename household assets for use in stunting analysis  # Author:	Tim Essam, Ph.D.
# Created:	2016/08/01
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------

# Capture output
#sink("~/GitHub/RwandaLAM/R/01_hhvars")

# RWHR70FL is equivalent to the hh data frame created in 00 file
hhvars = hh_all %>% transmute(v001 = hv001, v002 = hv002, cluster = hv001, hhnum = hv002, #merging info
                       
                       #interview info
                       monthint = hv006, yearint = hv007, intdate = hv008,  
                       
                       #Sampling information
                       psu = hv021, strata = hv024, 
                       hhweight = (hv005 / 1000000), maleweight = (hv028 / 1000000),
                       
                       # Geography
                       altitude = hv040, district = shdistrict, 
                       district = factor(district, 
                                       levels = c(11, 12, 13, 21, 22, 23, 24, 
                                                  25, 26, 27, 28, 31, 32, 33, 
                                                  34, 35, 36, 37, 41, 42, 43, 
                                                  44, 45, 51, 52, 53, 54, 55, 
                                                  56, 57),
                                       labels = c("Nyarugenge", "Gasabo", "Kicukiro", "Nyanza", 
                                                  "Gisagara", "Nyaruguru", "Huye", "Nyamagabe", 
                                                  "Ruhango", "Muhanga", "Kamonyi", "Karongi", 
                                                  "Rutsiro", "Rubavu", "Nyabihu", "Ngororero",
                                                  "Rusizi", "Nyamasheke", "Rulindo", "Gakenke", 
                                                  "Musanze", "Burera", "Gicumbi", "Rwamagana", 
                                                  "Nyagatare", "Gatsibo", "Kayonza", "Kirehe", 
                                                  "Ngoma", "Bugesera")),
                       # hh assets
                       hhsize = hv009, hhchildUnd5 = hv014, toilet = hv025, 
                       toiletShare = hv225, electricity = hv206, radio = hv207, 
                       tv = hv208, refig = hv209, bike = hv210, moto = hv211, 
                       car = hv212, mobile = hv243a, 
                       
                       # Demographics
                       rural = ifelse(hv025 == 1, 0, 
                                      ifelse(hv025 == 2, 1, NA)),
                       rural = factor(rural, levels = c(0, 1), labels = c("urban", "rural")),
                       hhrooms = hv216, roomsPC = (hhrooms / hhsize),
                       femhead = ifelse(hv219 == 1, 0, 
                                        ifelse(hv219 == 2, 1, NA)),
                       femhead = factor(femhead, levels = c(0, 1), labels = c("no", "yes")),
                       agehead = ifelse(hv220 != 98, hv220, NA),
                       
                       # WASH related
                       dirtfloor = ifelse(hv213 %in% c(11, 12), 1, 
                                          ifelse(hv213 %in% c(33, 34, 35, 96), 0, NA)),
                       dirtfloor = factor(dirtfloor, levels = c(0, 1), 
                                          labels = c("earth, sand, dung", "ceramic or better")),
                       handwash = ifelse(hv230a %in% c(2, 3, 4), 0, 
                                         ifelse(hv230a == 1, 1, NA)),
                       handwash = factor(handwash, levels = c(0, 1), 
                                         labels = c("unobserved", "observed")), 
                       treatwater = ifelse(hv237 != 8, hv237, NA),
                       kitchen = hv242,
                       bednet = hv227
                       ) 

# Now tackle hh ag assets
# Set the TLU values
camelVal 	= 0.70
cattleVal = 0.50
pigVal 	  = 0.20
sheepVal 	= 0.10
horsesVal = 0.50
mulesVal 	= 0.60
assesVal 	= 0.30
chxVal   	= 0.01

# Replace all casese of livestock where values are 98 instead of NA
mRecode <- function(x) {
  ifelse(x != 98, x, NA)
}

hhag = hh_all %>% transmute(v001 = hv001, v002 = hv002, cluster = hv001, 
                        
                        # land assets
                        landless = ifelse(hv244 == 0, 1, ifelse(hv244 == 1, 0, NA) ),
                        landowned = ifelse(hv245 != 998, (hv245 / 10), NA),
                        
                        livestock = hv246,
                        # Generate livestock and TLU calculations
                        cowtrad = mRecode(hv246a), horse = mRecode(hv246c), 
                        goat = mRecode(hv246d), sheep = mRecode(hv246e),
                        chicken = mRecode(hv246f), pig = mRecode(hv246g),
                        rabbit = mRecode(hv246h), cowmilk = mRecode(hv246i),
                        cowbull = mRecode(hv246j),
                        
                        # Create TLU weighted variables that go into aggregate
                        tlucattle = ((cowtrad + cowmilk + cowbull) * cattleVal),
                        tlusheep = ((sheep + goat) * sheepVal),
                        tluhorses = (horse * horsesVal),
                        tlupig = (pig * pigVal),
                        tluchx = ((rabbit + chicken) * chxVal),
                        
                        # clone and deflate the wealth variables
                        wealthGroup = hv270,
                        wealth = (hv271 / 100000),
                        
                        # Grab info about bank accounts just in case
                        bankAccount = hv247)

# Create TLUs
hhag = hhag %>% 
  rowwise() %>%
  mutate(tlutotal = sum(tlucattle, tlusheep, tluhorses, tlupig, tluchx, na.rm=TRUE))

# Merge the two new data frames together and alpha sort variables
hhchar = left_join(hhvars, hhag, by =c("v001", "v002")) %>% 
  select(noquote(order(colnames(.))))

# Reproduce summary statistics using sampling weights
# Appears to yield the same point estimates but slightly different standard errors
# Probably hase something to do with the manner in which variance is calculated.

library(survey)
DHSdesign <- svydesign(id = ~psu, strata = ~strata, weights = ~hhweight, data = hhchar)
summary(DHSdesign)

svymean(~livestock, DHSdesign, na.rm = TRUE)
cv(svymean(~livestock, DHSdesign, na.rm = TRUE))
confint(svymean(~livestock, DHSdesign))

svyby(~radio, ~rural, DHSdesign, svymean, deff, level = 0.95, na.rm = TRUE)

# Save the dataset as a Stata file
library(foreign)
write.dta(data = hhchar, file = "~/GitHub/RwandaLAM/Dataout/DHS_hhvarR.dta")




                                