
# Initial Setup -----------------------------------------------------------

# #Clear the Environment, Console and Plots respectively
# rm(list = ls())
# cat("\014")
# dev.off() # remove '#' when there are plots to clear, gives an error message when there are no plots

# # Set working directory to where the R file is saved
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load necessary Packages
# # System Library
# library(base) # R Base
# library(datasets) # R Dataset
# library(graphics) # R graphics
# library(grDevices) # R graphics devices for color and fonts
# library(methods) # formal methods and classes
# library(stats) # R stats
# library(utils) # R utils
# # User Library
# library('car') #Applied Regression and Data sets
# library(olsrr) #OLS Regression Models
# library(rgl) # for 3D Visualization
# library(leaps) #Regression/ Variable Selection Procedures
# library(faraway) #
# library(GLMsData) #Generalized Linear Model Data Sets
# library(statmod)
# library(MASS)


# Data and Variables -----------------------------------------------------------

data(motorins) #load the dataset from the GLMsData package
motorins$Kilometres <- as.factor(motorins$Kilometres) #converting categorical variables to factors/levels
motorins$Bonus <- as.factor(motorins$Bonus) #converting categorical variables to factors/levels

# Response Variable is Claims(numerical variable)
# Explanatory Variables are Kilometers(categorical variable), Bonus(categorical variable), Insured(numerical variable) and Payment(numerical variable)

##EDA ----
par(mfrow = c(1, 2))
with(motorins, {
  logClaims <- log(Claims+1)
  
  hist(Claims, las = 1, nclass = 20, main = "A: Histogram for Claims", xlab = "Claims")
  hist(log(Claims+1), las = 1, nclass = 20, main = "A: Histogram for  log(Claims+1)", xlab = "log(Claims+1)")
  
  plot(jitter(Claims) ~ Kilometres, ylab = "Claims", las = 1, main = "B: Claims/Kilometers")
  plot(jitter(logClaims) ~ Kilometres, ylab = "log(Claims+1)", las = 1, main = "B: log(Claims+1)/Kilometers")
  
  plot(jitter(Claims) ~ Bonus, ylab = "Claims", las = 1, main = "C: Claims/Bonus")
  plot(jitter(logClaims) ~ Bonus, ylab = "log(Claims+1)", las = 1, main = "C: log(Claims+1)/Bonus")
  
  plot(jitter(Claims) ~ Payment, ylab = "Claims", xlab = "Payments(in $)", las = 1, col=as.numeric(Kilometres)+2, main = "D: Claims/Payments", pch = 1:5)
  legend("topleft", col = 3:7, pch = 1:5, lwd = 2, lty = 1:5,
         merge = FALSE, legend = c("1 km", "2 km", "3 km", "4 km","5 km"))
  plot(jitter(logClaims) ~ log(Payment+1), ylab = "log(Claims+1)", xlab = "log(Payments+1)", las = 1, col=as.numeric(Kilometres)+2, main = "D: log(Claims+1)/log(Payments+1)", pch = 1:5)
  legend("topleft", col = 3:7, pch = 1:5, lwd = 2, lty = 1:5,
         merge = FALSE, legend = c("1 km", "2 km", "3 km", "4 km","5 km"))
  
  plot(jitter(Claims) ~ Insured, ylab = "Claims", las = 1, col=as.numeric(Kilometres)+2, main = "E: Claims/Insured", pch = 1:5)
  legend("topleft", col = 3:7, pch = 1:5, lwd = 2, lty = 1:5,
         merge = FALSE, legend = c("1 km", "2 km", "3 km", "4 km","5 km"))
  plot(jitter(logClaims) ~ log(Insured+1), ylab = "log(Claims+1)", las = 1, col=as.numeric(Kilometres)+2, main = "E: log(Claims+1)/log(Insured+1)", pch = 1:5)
  legend("topleft", col = 3:7, pch = 1:5, lwd = 2, lty = 1:5,
         merge = FALSE, legend = c("1 km", "2 km", "3 km", "4 km","5 km"))
})

par(mfrow = c(1, 1))
with(motorins, {
  plot(jitter(log(Insured+1)) ~ Kilometres, ylab = "log(Insured+1)", las = 1, main = "F: log(Insured+1) vs Kilometers")
  plot(jitter(log(Insured+1)) ~ Bonus, ylab = "log(Insured+1)", las = 1, main = "F: log(Insured+1) vs Bonus")
  plot(jitter(log(Insured+1)) ~ log(Payment+1), ylab = "log(Insured+1)", las = 1, col=as.numeric(Kilometres)+2, main = "F: log(Insured+1) vs log(Payment+1)", pch = 1:5)
  legend("topleft", col = 3:7, pch = 1:5, lwd = 2, lty = 1:5,
         merge = FALSE, legend = c("1 km", "2 km", "3 km", "4 km","5 km"))
})

options(contrasts = c("contr.treatment", "contr.treatment"))

vr <- with(motorins, tapply(X = Claims, INDEX = list(Kilometres,Bonus), FUN = "var" ))
mn <- with(motorins, tapply(X = Claims, INDEX = list(Kilometres,Bonus), FUN = "mean" ))
vr <- as.vector(vr)
mn <- as.vector(mn)
# plot log(var) against log(mean)
par(mfrow = c(1, 2))
plot(vr ~ mn, las = 1,  pch = 19, main = "D: Group Variance vs Group Mean",
     xlab = "Group Means", ylab = "Group Variances")
plot(log(vr+1) ~ log(mn+1), las = 1,  pch = 19, main = "E: Log(Group Variance) vs Log(Group Mean)",
     xlab = "Log(Group Means)", ylab = "Log(Group Variances)")
mf.lm <- lm(c(log(vr)) ~ c(log(mn)) )
coef(mf.lm)
abline(coef(mf.lm), lwd = 2)
#Slope ~ 2 Hence Gamma GLM 

# Gamma Model 1 -----------------------------------------------------------

gam.m1 <- glm(Claims+1 ~ Kilometres + Bonus + log(Payment+1) + log(Insured+1),
              family = Gamma(link = log), data = motorins, maxit = 100)
summary(gam.m1)
#all variables are significant

#dispersion parameter
P1 <- sum(resid(gam.m1, type = "pearson")^2)
res.df <- df.residual(gam.m1)
phi.hat <- P1/res.df #summary(gam.m1)$dispersion

#Model Diagnostics
par(mfrow = c(2, 4))
plot(rstandard(gam.m1) ~ log(fitted(gam.m1)), las = 1, 
     main = "M1: Deviance Residuals", ylab = "Standardized Residuals", 
     xlab ="Log of Fitted Values")

eta.log <- predict(gam.m1, type = "link")
z.log <- resid(gam.m1, type = "working") + eta.log
plot(z.log ~ eta.log, las = 1, ylab = "Working Responses", 
     xlab = "Linear Predictors", main = "M1: Working Responses")

plot(cooks.distance(gam.m1), type = "h", las = 1, 
     ylab = "Cook's Distance, D", main = "M1: Cook's Distance")

qqnorm(rstandard(gam.m1), las = 1,
       main = "M1: Normal Q-Q plot")
qqline(rstandard(gam.m1))

#check if model is enough
pchisq(gam.m1$deviance,gam.m1$df.residual,lower.tail = FALSE)

# #The analysis of deviance for a Gamma GLM
# m1.noK <- glm(Claims+1 ~ Bonus + log(Payment+1) + log(Insured+1), 
#               family = Gamma(link = log), data = motorins)
# m1.noB <- glm(Claims+1 ~ Kilometres + log(Payment+1) + log(Insured+1), 
#               family = Gamma(link = log), data = motorins)
# m1.noP <- glm(Claims+1 ~ Kilometres + Bonus + log(Insured+1), 
#               family = Gamma(link = log), data = motorins)
# m1.noI <- glm(Claims+1 ~ Kilometres + Bonus + log(Payment+1), 
#               family = Gamma(link = log), data = motorins, maxit = 100)
# anova(m1.noK, gam.m1, test = "F")
# anova(m1.noB, gam.m1, test = "F")
# anova(m1.noP, gam.m1, test = "F")
# anova(m1.noI, gam.m1, test = "F")

# Gamma Model 2 -----------------------------------------------------------
gam.m2 <- glm(Claims+1 ~ Kilometres + Bonus + log(Payment+1) * log(Insured+1),
              family = Gamma(link = log), data = motorins, maxit = 100)
summary(gam.m2)
#all variables are significant

#dispersion parameter
P2 <- sum(resid(gam.m2, type = "pearson")^2)
res.df2 <- df.residual(gam.m2)
phi.hat2 <- P2/res.df2 #summary(gam.m2)$dispersion

#Model Diagnostics
# par(mfrow = c(2, 2))
plot(rstandard(gam.m2) ~ log(fitted(gam.m2)), las = 1,
     main = "M2: Deviance Residuals", ylab = "Standardized Residuals",
     xlab ="Log of Fitted Values")

eta.log <- predict(gam.m2, type = "link")
z.log <- resid(gam.m2, type = "working") + eta.log
plot(z.log ~ eta.log, las = 1, ylab = "Working Responses",
     xlab = "Linear Predictors", main = "M2: Working Responses")

plot(cooks.distance(gam.m2), type = "h", las = 1,
     ylab = "Cook's Distance, D", main = "M2: Cook's Distance")

qqnorm(rstandard(gam.m2), las = 1,
       main = "M2: Normal Q-Q plot")
qqline(rstandard(gam.m2))

#Simpler Comparison with trend lines
# plot(gam.m1, main = "M1")
# plot(gam.m2, main = "M2")

#check if model is enough
pchisq(gam.m2$deviance,gam.m2$df.residual,lower.tail = FALSE)

#The analysis of deviance for a Gamma GLM
# m2.noK2 <- glm(Claims+1 ~ Bonus + log(Payment+1) * log(Insured+1),
#               family = Gamma(link = log), data = motorins)
# m2.noB2 <- glm(Claims+1 ~ Kilometres + log(Payment+1) * log(Insured+1),
#               family = Gamma(link = log), data = motorins)
# m2.noP2 <- glm(Claims+1 ~ Kilometres + Bonus + log(Insured+1),
#               family = Gamma(link = log), data = motorins)
# m2.noI2 <- glm(Claims+1 ~ Kilometres + Bonus + log(Payment+1),
#               family = Gamma(link = log), data = motorins, maxit = 100)
# anova(m2.noK2, gam.m2, test = "F")
# anova(m2.noB2, gam.m2, test = "F")
# anova(m2.noP2, gam.m2, test = "F")
# anova(m2.noI2, gam.m2, test = "F")
anova(gam.m2, test = "F")

#anova(gam.m1, gam.m2, test = "F") #check if interaction term is necessary

# Model 2 is better - no assumption violation and better than Model 1 in terms of AIC and residual deviance
# But the added interaction term doesn't have a real life significance, hence not recommended but selected for the project

