# Set working directory
setwd("C:/Users/richa/OneDrive/Desktop/Reservoir_Saugeye")

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
age0_cpue = dat$age0_cpue

# Fixed effects 
lbass_cpue = dat$lbass_cpue
lcrap_cpue = dat$lcrap_cpue
lsaug_cpue = dat$lsaug_cpue
sto_dens_ha = dat$sto_dens_ha
sto_length_mm = dat$sto_length_mm
sto_doy = dat$sto_doy
secchi = dat$secchi
size_ha = dat$size_ha

# untransformed median values for use in predictions
utmed_sto_dens_ha = median(sto_dens_ha)
utmed_lbass_cpue = median(lbass_cpue)
utmed_secchi = median(secchi)
utmed_size_ha = median(size_ha)

# Possible random effects 
location = as.factor(dat$location)
year = as.factor(dat$year)

# Cube root transform non-normal variables
age0_cpue = nthroot(age0_cpue, 3)
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
(LM <- lm(age0_cpue ~ sto_doy + sto_dens_ha + sto_length_mm + lbass_cpue +
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
pred_age0_cpue <- 9.89924-0.05715*med_sto_doy+0.18117*med_sto_dens_ha+
  0.03603*med_sto_length_mm-0.76436*med_lbass_cpue-0.07785*med_lcrap_cpue+
  0.14476*med_lsaug_cpue+0.47568*med_secchi-0.21785*med_size_ha

pred_age0_cpue <- pred_age0_cpue^3


#### sto_doy ####
# Prediction for median values but sto_doy adjusted -7 days
adj_sto_doy = med_sto_doy - 7

adj_age0_cpue <- 9.89924-0.05715*adj_sto_doy+0.18117*med_sto_dens_ha+
  0.03603*med_sto_length_mm-0.76436*med_lbass_cpue-0.07785*med_lcrap_cpue+
  0.14476*med_lsaug_cpue+0.47568*med_secchi-0.21785*med_size_ha

adj_age0_cpue <- adj_age0_cpue^3

# Difference between Median prediction and adjusted sto_doy prediction
adj_age0_cpue - pred_age0_cpue


#### sto_dens_ha ####

# Prediction for median values but sto_dens_ha adjusted +10% fish/ha
adj_sto_dens_ha = med_sto_dens_ha + nthroot((utmed_sto_dens_ha*.1), 3)

adj_age0_cpue <- 9.89924-0.05715*med_sto_doy+0.18117*adj_sto_dens_ha+
  0.03603*med_sto_length_mm-0.76436*med_lbass_cpue-0.07785*med_lcrap_cpue+
  0.14476*med_lsaug_cpue+0.47568*med_secchi-0.21785*med_size_ha

adj_age0_cpue <- adj_age0_cpue^3

# Difference between Median prediction and adjusted sto_dens_ha prediction
adj_age0_cpue - pred_age0_cpue


#### lbass_cpue ####

# Prediction for median values but median lbass_cpue adjusted -10% fish/hr
adj_lbass_cpue = med_lbass_cpue - nthroot((utmed_lbass_cpue*.1), 3)

adj_age0_cpue <- 9.89924-0.05715*med_sto_doy+0.18117*med_sto_dens_ha+
  0.03603*med_sto_length_mm-0.76436*adj_lbass_cpue-0.07785*med_lcrap_cpue+
  0.14476*med_lsaug_cpue+0.47568*med_secchi-0.21785*med_size_ha

adj_age0_cpue <- adj_age0_cpue^3

# Difference between Median prediction and adjusted lbass_cpue prediction
adj_age0_cpue - pred_age0_cpue


#### secchi ####

# Prediction for median values but median lbass_cpue adjusted -10% fish/hr
adj_secchi = med_secchi + nthroot((utmed_secchi*.1), 3)

adj_age0_cpue <- 9.89924-0.05715*med_sto_doy+0.18117*med_sto_dens_ha+
  0.03603*med_sto_length_mm-0.76436*med_lbass_cpue-0.07785*med_lcrap_cpue+
  0.14476*med_lsaug_cpue+0.47568*adj_secchi-0.21785*med_size_ha

adj_age0_cpue <- adj_age0_cpue^3

# Difference between Median prediction and adjusted secchi prediction
adj_age0_cpue - pred_age0_cpue


#### size_ha ####

# Prediction for median values but median size_ha adjusted -10% ha
adj_size_ha = med_size_ha - nthroot((utmed_size_ha*.1), 3)

adj_age0_cpue <- 9.89924-0.05715*med_sto_doy+0.18117*med_sto_dens_ha+
  0.03603*med_sto_length_mm-0.76436*med_lbass_cpue-0.07785*med_lcrap_cpue+
  0.14476*med_lsaug_cpue+0.47568*med_secchi-0.21785*adj_size_ha

adj_age0_cpue <- adj_age0_cpue^3

# Difference between Median prediction and adjusted size_ha prediction
adj_age0_cpue - pred_age0_cpue