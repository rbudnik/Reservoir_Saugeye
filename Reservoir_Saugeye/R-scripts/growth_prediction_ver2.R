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

(LMM1 <- lmer(growth ~ sto_doy + (1|location), REML = FALSE))

#### Determine median values  for predictors ####
med_sto_doy = median(sto_doy)

######--------------------------------------------------------------
# Predictions for each significant variable
######--------------------------------------------------------------

#### Creates new data frame with median values
#### Median stocking day of year is also decreased by 7 days to predict change in saugeye growth
new.sto_doy_df <- data.frame(sto_doy = c(med_sto_doy, med_sto_doy - 7, med_sto_doy + 7))

# Predicts growth for median and stocking day of year - 7
new.grow.sto_doy <- predict(LMM1, newdata = new.sto_doy_df, re.form = NA)

# Cubes values to get real values for predictions -7
(new.grow.sto_doy[2]) - (new.grow.sto_doy[1])

