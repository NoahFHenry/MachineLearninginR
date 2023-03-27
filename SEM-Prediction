# Out of sample predictions for an SEM based on the method developed by De Rooij et al. (2022)

# load packages
library(lavaan)
library(semPlot)

# specify initial model

model <- 
  "# latent variables
Expertise =~ Exp_1 + Exp_2 + Exp_3
PRQ =~ PRQ_1 + PRQ_2 + PRQ_3 + PRQ_4 + PRQ_5 + PRQ_6
PSE =~ PSE_1 + PSE_2 + PSE_3 + PSE_4 + PSE_5

# regressions
PRQ ~ a*0.5*Expertise
PSE ~ b*0.7*PRQ + c*0.5*Expertise

# indirect and total effect (mediation)
ind_Expertise_PRQ_PSE := a*b
tot_Expertise_PSE := ind_Expertise_PRQ_PSE + c"

# simulate data based on model
set.seed(123)
sim.model <- simulateData(model = model, sample.nobs = 500L)
fitted(sem(model))
round(cov(sim.model), 3)
round(colMeans(sim.model), 3)

# Specify model
model.to.fit <- 
  "# latent variables
Expertise =~ Exp_1 + Exp_2 + Exp_3
PRQ =~ PRQ_1 + PRQ_2 + PRQ_3 + PRQ_4 + PRQ_5 + PRQ_6
PSE =~ PSE_1 + PSE_2 + PSE_3 + PSE_4 + PSE_5

# regressions
PRQ ~ a*0.5*Expertise
PSE ~ b*0.7*PRQ + c*0.5*Expertise

ind_Expertise_PRQ_PSE := a*b
tot_Expertise_PSE := ind_Expertise_PRQ_PSE + c"

# fit and inspect model
fit <- sem(model.to.fit, data = sim.model, std.lv = TRUE, estimator = "MLM")
summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# sem plot
semPaths(fit, what = "paths", whatLabels = "path", weighted = FALSE, layout = "tree2", rotation = 1, nodeLabels = c("Exp 1", "Exp 2", "Exp 3", "PRQ 1", "PRQ 2","PRQ 3","PRQ 4","PRQ 5","PRQ 6", "PSE 1", "PSE 2", "PSE 3", "PSE 4", "PSE 5", "Expertise", "PRQ", "PSE"), style = "ram", curvature = 3, springLevels = TRUE, curvePivot = FALSE, residuals = TRUE, intercepts = FALSE)

# out-of-sample predictions for Y indicators based on values of X indicators
lavPredictY(fit, newdata = NULL, 
            ynames = c("PSE_1", "PSE_2", "PSE_3", "PSE_4", "PSE_5"),
            xnames = c("Exp_1", "Exp_2", "Exp_3"), 
            method = "conditional.mean",
            label = TRUE, assemble = TRUE, force.zero.mean = FALSE)
