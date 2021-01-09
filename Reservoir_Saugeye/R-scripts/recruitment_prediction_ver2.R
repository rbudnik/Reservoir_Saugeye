# Set working directory
setwd("C:/Users/richa/OneDrive/Desktop/SAE_Retrospective/SAE_Retrospective")

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
(LM <- lm(age0_cpue ~ sto_doy + sto_dens_ha + lbass_cpue + size_ha + secchi))

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


#### sto_doy ####
# Prediction for median values but sto_doy adjusted -7 days
new.sto_doy.df <- data.frame(sto_doy = c(med_sto_doy, med_sto_doy - 7),
                       sto_dens_ha = med_sto_dens_ha,
                       sto_length_mm = med_sto_length_mm,
                       lbass_cpue = med_lbass_cpue,
                       lcrap_cpue = med_lcrap_cpue,
                       lsaug_cpue = med_lsaug_cpue,
                       secchi = med_secchi,
                       size_ha = med_size_ha)

# Predicts growth for median and stocking day of year - 7
new.recr.sto_doy <- predict(LM, newdata = new.sto_doy.df)

# Cubes values to get real values for predictions
((new.recr.sto_doy[2])^3) - ((new.recr.sto_doy[1])^3)



#### sto_dens_ha ####
new.sto_dens_ha.df <- data.frame(sto_doy = med_sto_doy,
                       sto_dens_ha = c(med_sto_dens_ha, med_sto_dens_ha + nthroot((utmed_sto_dens_ha*.1), 3)),
                       sto_length_mm = med_sto_length_mm,
                       lbass_cpue = med_lbass_cpue,
                       lcrap_cpue = med_lcrap_cpue,
                       lsaug_cpue = med_lsaug_cpue,
                       secchi = med_secchi,
                       size_ha = med_size_ha)

# Predicts growth for median and stocking density + 10%
new.recr.sto_dens_ha <- predict(LM, newdata = new.sto_dens_ha.df)

# Cubes values to get real values for predictions
((new.recr.sto_dens_ha[2])^3) - ((new.recr.sto_dens_ha[1])^3)



#### lbass_cpue ####
new.lbass_cpue.df <- data.frame(sto_doy = med_sto_doy,
                       lbass_cpue = c(med_lbass_cpue, med_lbass_cpue - nthroot((utmed_lbass_cpue*.1), 3)),
                       sto_length_mm = med_sto_length_mm,
                       sto_dens_ha = med_sto_dens_ha,
                       lcrap_cpue = med_lcrap_cpue,
                       lsaug_cpue = med_lsaug_cpue,
                       secchi = med_secchi,
                       size_ha = med_size_ha)

# Predicts growth for median and bass cpue - 10%
new.recr.lbass_cpue <- predict(LM, newdata = new.lbass_cpue.df)

# Cubes values to get real values for predictions
((new.recr.lbass_cpue[2])^3) - ((new.recr.lbass_cpue[1])^3)



#### secchi ####
new.secchi.df <- data.frame(sto_doy = med_sto_doy,
                       secchi = c(med_secchi, med_secchi + nthroot((utmed_secchi*.1), 3)),
                       sto_length_mm = med_sto_length_mm,
                       lbass_cpue = med_lbass_cpue,
                       lcrap_cpue = med_lcrap_cpue,
                       lsaug_cpue = med_lsaug_cpue,
                       sto_dens_ha = med_sto_dens_ha,
                       size_ha = med_size_ha)

# Predicts growth for median and secchi + 10%
new.recr.secchi <- predict(LM, newdata = new.secchi.df)

# Cubes values to get real values for predictions
((new.recr.secchi[2])^3) - ((new.recr.secchi[1])^3)



#### size_ha ####
new.size_ha.df <- data.frame(sto_doy = med_sto_doy,
                       size_ha = c(med_size_ha, med_size_ha - nthroot((utmed_size_ha*.1), 3)),
                       sto_length_mm = med_sto_length_mm,
                       lbass_cpue = med_lbass_cpue,
                       lcrap_cpue = med_lcrap_cpue,
                       lsaug_cpue = med_lsaug_cpue,
                       secchi = med_secchi,
                       sto_dens_ha = med_sto_dens_ha)

# Predicts growth for median and szize - 10%
new.recr.size_ha <- predict(LM, newdata = new.size_ha.df)

# Cubes values to get real values for predictions
((new.recr.size_ha[2])^3) - ((new.recr.size_ha[1])^3)

