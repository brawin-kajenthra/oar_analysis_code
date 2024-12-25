m1 <- glm(as.factor(not_OAR) ~ `Health Profession Group`+ `Age Group` + `Site of XR Examination`, data = cleaned_dat2, family = 'binomial')

step(m1, direction = 'both', trace = 1)

summary(m1)

# Testing a random effects model

library(lme4)

m2 <- glmer(not_OAR ~ `Health Profession Group` + (1 | Procedure), 
            data = cleaned_dat,
            family = 'binomial')

m3 <- glmer(not_OAR ~ `Health Profession Group` + (1 | age_group), 
            data = cleaned_dat,
            family = 'binomial')

anova(m2, m3) # Compare the AIC, BIG, logLik and deviance - M2 does better

# Let's use M2 to calculate odds ratios for the professions
or_m2 <- exp(fixef(m2))

# The ORs for M1 and M2 are similar so we will use M1 as it is the simpler 
# model. 

# Let's rename the columns so it'll come out properly on the plot 


library(forestmodel)
forest_model(m1)


