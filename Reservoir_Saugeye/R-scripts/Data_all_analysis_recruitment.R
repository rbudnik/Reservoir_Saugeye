# Set working directory
setwd("C:/Users/richa/OneDrive/Desktop/SAE_Retrospective/NAJFM manuscript 2")

# Load data
dat=read.csv("csv/Data_complete7.csv")

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
sto_doy = dat$sto_doy
secchi = dat$secchi
size_ha = dat$size_ha
AvgSpWTemp = dat$AvgSpWTemp
AvgSuWTemp = dat$AvgSuWTemp

# Possible random effects 
location = as.factor(dat$location)
year = as.factor(dat$year)

# Cube root transform non-normal variables
age0_cpue = nthroot(age0_cpue, 3)
lbass_cpue = nthroot(lbass_cpue, 3) 
lcrap_cpue = nthroot(lcrap_cpue, 3)
sto_dens_ha = nthroot(sto_dens_ha, 3)
secchi = nthroot(secchi, 3)
size_ha = nthroot(size_ha, 3)


######--------------------------------------------------------------
# Possible models
######--------------------------------------------------------------

#### Mixed-model with random effect of location ####

(LMM1 <- lmer(age0_cpue ~ sto_doy + sto_dens_ha + AvgSpWTemp + AvgSuWTemp + lbass_cpue + 
                lsaug_cpue + lcrap_cpue + secchi + size_ha + (1|location), REML = FALSE))

LMM1 <- standardize(LMM1, standardize.y = FALSE) # standardize input variables using Gelman's approach

# Check for multicollinearity w/ Variance inflation factor
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
(LMM1_Best <- lmer(age0_cpue ~ sto_doy + sto_dens_ha + lsaug_cpue + (1|location), REML = FALSE))
r2_nakagawa(LMM1_Best, by_group = FALSE)

(LMM2_Best <- lmer(age0_cpue ~ sto_doy + sto_dens_ha + (1|location), REML = FALSE))
r2_nakagawa(LMM2_Best, by_group = FALSE)

(LMM3_Best <- lmer(age0_cpue ~ sto_dens_ha + (1|location), REML = FALSE))
r2_nakagawa(LMM3_Best, by_group = FALSE)

(LMM4_Best <- lmer(age0_cpue ~ sto_doy + sto_dens_ha + lbass_cpue + lsaug_cpue + (1|location), REML = FALSE))
r2_nakagawa(LMM4_Best, by_group = FALSE)

(LMM5_Best <- lmer(age0_cpue ~ sto_doy + sto_dens_ha + lsaug_cpue + AvgSpWTemp + (1|location), REML = FALSE))
r2_nakagawa(LMM5_Best, by_group = FALSE)

(LMM6_Best <- lmer(age0_cpue ~ sto_doy + sto_dens_ha + secchi + lsaug_cpue + (1|location), REML = FALSE))
r2_nakagawa(LMM6_Best, by_group = FALSE)

(LMM7_Best <- lmer(age0_cpue ~ sto_doy + sto_dens_ha + lcrap_cpue + (1|location), REML = FALSE))
r2_nakagawa(LMM7_Best, by_group = FALSE)

(LMM8_Best <- lmer(age0_cpue ~ sto_doy + sto_dens_ha + lbass_cpue + (1|location), REML = FALSE))
r2_nakagawa(LMM8_Best, by_group = FALSE)

(LMM9_Best <- lmer(age0_cpue ~ sto_doy + sto_dens_ha + lsaug_cpue + lcrap_cpue + (1|location), REML = FALSE))
r2_nakagawa(LMM9_Best, by_group = FALSE)

