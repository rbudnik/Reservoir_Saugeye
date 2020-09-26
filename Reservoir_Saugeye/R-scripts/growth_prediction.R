# Set working directory
setwd("C:/Users/richa/OneDrive/Desktop/Resevoir_Saugeye")

# Load data
dat=read.csv("csv/Data_complete3.csv")

# Load necessary packages
library(lme4)
library(MuMIn) 
library(performance)
library(pracma)
library(car)
library(arm)

# Response variable
growth = dat$growth

# Fixed effects 
lbass_cpue = dat$lbass_cpue
lcrap_cpue = dat$lcrap_cpue
lsaug_cpue = dat$lsaug_cpue
sto_dens_ha = dat$sto_dens_ha
sto_length_mm = dat$sto_length_mm
sto_doy = dat$sto_doy
secchi = dat$secchi
size_ha = dat$size_ha

# Possible random effects 
location = as.factor(dat$location)
year = as.factor(dat$year)

# Cube root transform non-normal variables
lbass_cpue = nthroot(lbass_cpue, 3) 
lcrap_cpue = nthroot(lcrap_cpue, 3)
lsaug_cpue = nthroot(lsaug_cpue, 3)
sto_dens_ha = nthroot(sto_dens_ha, 3)
secchi = nthroot(secchi, 3)
size_ha = nthroot(size_ha, 3)

######--------------------------------------------------------------
# Multiple regression model for recruitment
######--------------------------------------------------------------

#### Multiple regression model ####
# COefficients used for prediction models
(LM <- lm(growth ~ sto_doy + sto_dens_ha + sto_length_mm + lbass_cpue +
                lcrap_cpue + lsaug_cpue + secchi + size_ha))

#### Determine median values  for predictors ####
med_sto_doy = median(sto_doy)
med_sto_dens_ha = median(sto_dens_ha)
med_sto_length_mm = median(sto_length_mm)
med_lbass_cpue = median(lbass_cpue)
med_lcrap_cpue = median(lcrap_cpue)
med_lsaug_cpue = median(lsaug_cpue)
med_secchi = median(secchi)
med_size_ha = median(size_ha)


######--------------------------------------------------------------
# Predictions for each significant variable
######--------------------------------------------------------------

# Prediction for median values
pred_growth <- 1.1356358-0.0054247*med_sto_doy-0.0084160*med_sto_dens_ha+
  0.0006889*med_sto_length_mm+0.0173824*med_lbass_cpue-0.0297924*med_lcrap_cpue-
  0.0496443*med_lsaug_cpue+0.0078388*med_secchi+0.0006500*med_size_ha

#### sto_doy ####
# Prediction for median values but sto_doy adjusted -7 days
adj_sto_doy = med_sto_doy - 7

adj_growth <- 1.1356358-0.0054247*adj_sto_doy-0.0084160*med_sto_dens_ha+
  0.0006889*med_sto_length_mm+0.0173824*med_lbass_cpue-0.0297924*med_lcrap_cpue-
  0.0496443*med_lsaug_cpue+0.0078388*med_secchi+0.0006500*med_size_ha

# Difference between Median prediction and adjusted sto_doy prediction
adj_growth - pred_growth