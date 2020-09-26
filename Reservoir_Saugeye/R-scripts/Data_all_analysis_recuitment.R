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
# Possible models
######--------------------------------------------------------------

#### Multiple regression model ####
(LM <- lm(age0_cpue ~ sto_doy + sto_dens_ha + sto_length_mm + lbass_cpue +
                lcrap_cpue + lsaug_cpue + secchi + size_ha))

LM <- standardize(LM, standardize.y = FALSE) # standardize input variables using Gelman's approach

# Check for multicollinearity w/ Variance inflation factor
vif(LM)

# Check residual histograms and qqplot
hist(residuals(LM))
qqnorm(residuals(LM))
qqline(residuals(LM))


#### Mixed-model with random effect of location ####

(LMM1 <- lmer(age0_cpue ~ sto_doy + sto_dens_ha + sto_length_mm + lbass_cpue + 
                lsaug_cpue + lcrap_cpue + secchi + size_ha + (1|location), REML = FALSE))

LMM1 <- standardize(LMM1, standardize.y = FALSE) # standardize input variables using Gelman's approach

# Variance inflation factor
vif(LMM1)

# Check residual histograms and qqplot
hist(residuals(LMM1))
qqnorm(residuals(LMM1))
qqline(residuals(LMM1))


#### Mixed-model with random effect of year ####

(LMM2 <- lmer(age0_cpue ~ sto_doy + sto_dens_ha + sto_length_mm + lbass_cpue + 
                    lsaug_cpue + lcrap_cpue + secchi + size_ha + (1|year), REML = FALSE))

LMM2 <- standardize(LMM2, standardize.y = FALSE) # standardize input variables using Gelman's approach

# Variance inflation factor
vif(LMM2)

# Check residual histograms and qqplot
hist(residuals(LMM2))
qqnorm(residuals(LMM2))
qqline(residuals(LMM2))


######--------------------------------------------------------------
# Should random effects be included in model?
######--------------------------------------------------------------

# Is random effect of location sig?
anova(LMM1, LM)

# Is random effect of year sig?
anova(LMM2, LM)


########--------------------------------------------------------------
# Dredge and AIC
########--------------------------------------------------------------

# Model selection: ranking by AICc using multiple linear model
options(na.action = "na.fail")
LM_dredge <- dredge(LM, trace = TRUE, rank = "AICc", REML = FALSE)

# Get the models
fmList <- get.models(LM_dredge,1:20)
options(max.print=999999) # increase max.print to ensure null model is printed

## Summarize each model and rank by AIC
summary(model.avg(fmList))
LM_dredge

# Models coefficients with delta AIC < 4
summary(model.avg(get.models(LM_dredge, subset = delta<2)))

# Confidence interval calculations
confset.95p<-get.models(LM_dredge, subset = delta<2)
avgmod.95p<-model.avg(confset.95p)
confint(avgmod.95p, full = T)


########--------------------------------------------------------------
# Variable importance
########--------------------------------------------------------------
importance(model.avg(LM_dredge, subset = delta <= 2))


########--------------------------------------------------------------
# Summarize complete and top performing models (delta AIC < 2)
########--------------------------------------------------------------
summary(LM)

LM_Best <- lm(age0_cpue ~ sto_doy + sto_dens_ha + lbass_cpue + size_ha + secchi)
summary(LM_Best)

LM_2Best <- lm(age0_cpue ~ sto_doy + sto_dens_ha + sto_length_mm + lbass_cpue +
                secchi + size_ha)
summary(LM_2Best)

LM_3Best <- lm(age0_cpue ~ sto_doy + sto_dens_ha + lbass_cpue +
                lcrap_cpue + secchi + size_ha)
summary(LM_3Best)

LM_4Best <- lm(age0_cpue ~ sto_doy + sto_dens_ha + lbass_cpue +
                lsaug_cpue + secchi + size_ha)
summary(LM_4Best)

LM_5Best <- lm(age0_cpue ~ sto_doy + sto_dens_ha + lbass_cpue +
                size_ha)
summary(LM_5Best)

LM_6Best <- lm(age0_cpue ~ sto_doy + sto_dens_ha + lbass_cpue +
                lcrap_cpue + size_ha)
summary(LM_6Best)