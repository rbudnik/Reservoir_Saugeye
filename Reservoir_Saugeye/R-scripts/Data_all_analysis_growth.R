# Set working directory
setwd("C:/Users/richa/OneDrive/Desktop/Reservoir_Saugeye")

# Load data
dat=read.csv("csv/Data_complete.csv")

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
AvgSpWTemp = dat$AvgSpWTemp
AvgSuWTemp = dat$AvgSuWTemp

# Possible random effects 
location = as.factor(dat$location)
year = as.factor(dat$year)

# Cube root transform non-normal variables
lbass_cpue = nthroot(lbass_cpue, 3) 
lcrap_cpue = nthroot(lcrap_cpue, 3)
sto_dens_ha = nthroot(sto_dens_ha, 3)
secchi = nthroot(secchi, 3)
size_ha = nthroot(size_ha, 3)

######--------------------------------------------------------------
# Possible models
######--------------------------------------------------------------

#### Mixed-model with random effect of location ####

(LMM1 <- lmer(growth ~ sto_doy + sto_dens_ha + AvgSuWTemp + AvgSpWTemp + lbass_cpue + 
                lsaug_cpue + lcrap_cpue + secchi + size_ha + (1|location), REML = FALSE))

# standardize input variables using Gelman's approach
LMM1 <- standardize(LMM1, standardize.y = FALSE) 

# Variance inflation factor
vif(LMM1)

# Check residual histograms and qqplot
hist(residuals(LMM1))
qqnorm(residuals(LMM1))
qqline(residuals(LMM1))


########--------------------------------------------------------------
# Dredge and AIC
########--------------------------------------------------------------

# Model selection: ranking by AICc using multiple linear model
options(na.action = "na.fail")
LMM1_dredge <- dredge(LMM1, trace = TRUE, rank = "AICc", REML = FALSE)

# Get the models
fmList <- get.models(LMM1_dredge,1:20)
options(max.print=999999) # increase max.print to ensure null model is printed

## Summarize each model and rank by AIC
summary(model.avg(fmList))
LMM1_dredge

# Models coefficients with delta AIC < 2
summary(model.avg(get.models(LMM1_dredge, subset = delta<2)))

# Confidence interval calculations
confset.95p<-get.models(LMM1_dredge, subset = delta<2)
avgmod.95p<-model.avg(confset.95p)
confint(avgmod.95p, full = T)


########--------------------------------------------------------------
# Variable importance
########--------------------------------------------------------------
importance(model.avg(LMM1_dredge, subset = delta <= 2))


########--------------------------------------------------------------
# Summarize complete and top performing models (delta AIC < 2)
########--------------------------------------------------------------

# Calculate Nakagawa's R2 to determine marginal vs conditional
LMM1_Best <- lmer(growth ~ sto_doy + AvgSuWTemp + (1|location), REML = FALSE)
r2_nakagawa(LMM1_Best, by_group = FALSE)

LMM2_Best <- lmer(growth ~ sto_doy + sto_dens_ha  + AvgSuWTemp + (1|location), REML = FALSE)
r2_nakagawa(LMM2_Best, by_group = FALSE)

LMM3_Best <- lmer(growth ~ sto_doy + secchi + AvgSuWTemp + (1|location), REML = FALSE)
r2_nakagawa(LMM3_Best, by_group = FALSE)

LMM4_Best <- lmer(growth ~ sto_doy + lsaug_cpue + AvgSuWTemp + (1|location), REML = FALSE)
r2_nakagawa(LMM4_Best, by_group = FALSE)
