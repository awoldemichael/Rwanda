
# Testing clustering errors by village ------------------------------------

# Function Tim has double-checked with Stata
library(sandwich)
library(lmtest)
library(plm)
library(haven)

# Define clustering function as described in paper
cl   <- function(dat,fm, cluster){
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL) 
}

# Testing the functions; 
data(Crime)
Crime$geography <-factor(Crime$region)

# Run a plain linear regression
regModel = lm(crmrte ~ density + taxpc + avgsen, data = Crime)
summary(regModel)

# apply the 'cl' function by choosing a variable to cluster on.
# here, we are clustering first on geography then on year; Can also do two-way clustering.
cl(Crime, regModel, Crime$geography)
cl(Crime, regModel, Crime$year)



# now for the real deal. --------------------------------------------------

model = lm(stuntingZ ~ splines::bs(age_months, degree = 3,
                                   knots = 24) + village_cat + livelihood_zone + splines::bs(monthly_pc_expend,
                                                                                             degree = 2), data = females_hh)

# cl requires proper filtering of the data
filtered = females_hh %>% filter(!is.na(village_cat), !is.na(monthly_pc_expend))
cl(filtered, model, filtered$village)

# checking if multwayvcov treats NAs properly 
lmtest::coeftest(model, vcov = multiwayvcov::cluster.vcov(model, females_hh$village))
# gives same answer as cl function (with NAs removed)