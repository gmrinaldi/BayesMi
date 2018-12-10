library(ggmcmc)
library(ggthemes)
library(gridExtra)

load("C:/Users/Giulia/Desktop/Bayesian_prog/R_code/BayesMi/fit_modello5.RData")

S <- ggs(fit.1)
is(S)
names(S) 
unique(S$Parameter)

ggmcmc(S, file = 'output_ggmcmc.pdf')  # it produces a pdf file 
# with all the plots
ggmcmc(S, file = 'density_only.pdf', plot = c('density'), param_page = 2)
x11()
ggs_histogram(S)
ggs_density(S)
ggs_traceplot(S)
ggs_running(S)
ggs_compare_partial(S, partial = 0.3) # it produces overlapped density plots that compare 
# the last part of the chain, partial
ggs_Rhat(S) + xlab('R_hat') + ylab('Parameters')


# Function caterpillar:
# The function combines plots of the highest 
# posterior densities for many parameters
radon.intercept <- data.frame(
  Parameter = paste("alpha[", radon$counties$id.county,"]", sep=""),
  Label = radon$counties$County)
# par_labels{ggs}: data frame with two colums. One named "Parameter" with 
# the same names of the parameters of the model. Another named "Label" with 
# the label of the parameter. 
# When missing, the names passed to the model are used for representation. 
# Label contains the name of the county
head(radon.intercept)
S.full <- ggs(radon$s.radon, par_labels = radon.intercept, family = "alpha")
x11()
ggs_caterpillar(S.full, thick_ci = c(0.05, 0.95), thin_ci = c(0.025, 0.975))

# Against a continous feature:
Z <- data.frame(
  Parameter = paste("alpha[", radon$counties$id.county,"]", sep=''),
  value = radon$counties$uranium)
x11()
ggs_caterpillar(ggs(radon$s.radon, family='^alpha'), X = Z, horizontal = FALSE) 

# Caterpillar plots of intercepts using a color scale to
# emphasize the uranium level by county
L.radon <- data.frame(
  Parameter = c(paste("alpha[", radon$counties$id.county, "]", sep=""),
                paste("beta[", radon$counties$id.county, "]", sep="")),
  Label = rep(radon$counties$County, 2),
  Uranium = rep(radon$counties$uranium, 2),
  Location = rep(radon$counties$ns.location, 2), # North or South
  Coefficient = gl(n = 2, k = length(radon$counties$id.county), 
                   labels=c("Intercept", "Slope")))
# n: an integer giving the number of levels.
# k: an integer giving the number of replications.
# labels: an optional vector of labels for the resulting factor levels.
x11()
ggs_caterpillar(ggs(radon$s.radon, par_labels = L.radon, family="^alpha")) +
  facet_wrap(~Location, scales="free") +
  aes(color = Uranium) 
# wrap: split up your data by one or more variables and plot the subsets of data together.

# Aesthetic variations:
f1 <- ggs_traceplot(ggs(radon$s.radon, family = "^alpha\\[[123]\\]")) + 
  theme_fivethirtyeight(base_size=14, base_family="sans")
f2 <- ggs_density(ggs(radon$s.radon, family = "^alpha\\[[123]\\]")) + 
  theme_solarized() 
x11()
grid.arrange(f1, f2, ncol=2, nrow=1)

graphics.off()

##########################
# Function ci computes credibility interval
ci.median <- ci(ggs(radon$s.radon, family="^alpha|^beta")) %>%
  select(Parameter, median)

###############################################à
### Posterior predictive checks:
# continous outcomes:
data(linear) # samples from a linear model with an intercept and a covariate
# where y.rep are posterior predictive samples
S.y.rep <- ggs(s.y.rep)
head(S.y.rep)
y.observed <- y 

S.y.rep$Parameter

x11()
# Histogram with the distribution of the predicted posterior means, 
# compared with the mean of the observed outcome.
ggs_ppmean(S.y.rep, outcome = y.observed, bins=50) # mean
# ggs_ppsd(S.y.rep, outcome = y.observed) # standard deviation

x11()
# Binary outcomes:
data(binary)
S.binary <- ggs(s.binary, family = 'mu')
# ROC curve:
ggs_rocplot(S.binary, outcome = y.binary)
