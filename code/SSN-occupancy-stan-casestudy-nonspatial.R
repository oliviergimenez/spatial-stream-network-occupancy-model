#library(SSNbayes)
library(tidyverse)
library(sf)
library(unmarked)
library(ubms)

#library(viridis)
#library(RColorBrewer)


#--- get occupancy data

sites_occ <- st_read("data/shp/sites.shp")
mask <- c(25:71,73:81) # take single watershed
dat <- sites_occ[mask,]

# take only 3 first occasions, last is full of Nas
y2003 <- cbind(dat$CONTACT_1,
               dat$CONTACT_2,
               dat$CONTACT_3)

# add some fake covariates for illustration
site.covs <- data.frame(P100ZTCUL = dat$P100ZTC,
                        ZT200_popK = dat$ZT200_K)

# observation covariates are in site-major, observation-minor order
tmp <- cbind(dat$o_2003_1,
             dat$o_2003_2, 
             dat$o_2003_3)
tmp[tmp == 'PNR'] <- 'AUTRE'
obs.covs <- list(obs = tmp)

umf <- unmarkedFrameOccu(y = y2003, siteCovs = site.covs, obsCovs = obs.covs)   # organize data
umf                     # look at data
summary(umf)            # summarize      

plot(umf)

# our model is
# p function of observer ONCFS vs OTHER
# occupancy is function of prop zone cultivated + human pop
(fm <- occu(~ obs ~ scale(P100ZTCUL) + scale(log(ZT200_popK + 1)), data = umf))

# fit same model in STAN through ubms
(fm <- occu(~ obs ~ scale(P100ZTCUL) + scale(log(ZT200_popK + 1)), data = umf))

# 
# Call:
#   stan_occu(formula = ~obs ~ scale(P100ZTCUL) + scale(log(ZT200_popK + 
#                                                             1)), data = umf, chains = 3, iter = 500, cores = 3, seed = 123)
# 
# Occupancy (logit-scale):
#   Estimate    SD    2.5%  97.5% n_eff  Rhat
# (Intercept)                   1.070 0.597  0.0467  2.380   417 0.999
# scale(P100ZTCUL)             -0.189 0.502 -1.2400  0.697   424 0.999
# scale(log(ZT200_popK + 1))   -1.196 0.483 -2.2118 -0.359   759 0.999
# 
# Detection (logit-scale):
#   Estimate    SD   2.5% 97.5% n_eff  Rhat
# (Intercept)     1.03 0.276  0.506  1.59   544 0.997
# obsONCFS       -1.64 0.554 -2.638 -0.52   613 0.998

waic(fit_stan)

# Computed from 750 by 56 log-likelihood matrix
# 
# Estimate   SE
# elpd_waic    -81.6  7.4
# p_waic         5.6  1.1
# waic         163.2 14.8
# 

plot_effects(fit_stan, "state")
plot_effects(fit_stan, "det")


(fit_ct <- stan_occu(~1 ~1, data = umf, chains = 3, iter = 500, cores = 3, seed = 123))

# Estimate    SD   2.5% 97.5% n_eff Rhat
# 0.364 0.309 -0.239 0.985   623 1.01
# 
# Detection (logit-scale):
#   Estimate    SD  2.5% 97.5% n_eff Rhat
# 0.852 0.267 0.359  1.38   586 1.01

#-------------------- non-centered param wo nugget

# Stan code
constant <- "
data{
  int nsite; // Number of sites
  int nrep; // Number of visits
  array[nsite, nrep] int y; // det/non-det data
  array[nsite, nrep] int sampled; // indicator for whether site is sampled or not (NA)
  // vector[nsite] x; // covariate x
}

parameters{
  real beta; // slope (intercept is k_bar)
  // real k_bar; // intercept (slope is beta)
  real <lower=0, upper = 1> p;
}

transformed parameters{
  vector[nsite] psi; // Probability of occurrence at each site i
  for(isite in 1:nsite){
    // psi[isite] = inv_logit(k_bar + beta * x[isite]);
    psi[isite] = inv_logit(beta);
  }

}

model{

  vector[nsite] log_psi; // Log of psi
  vector[nsite] log1m_psi; // Log of 1-psi

  // Priors
  beta ~ normal(0, 1.5);
  p ~ uniform(0, 1);
  //k_bar ~ normal(0, 1.5);

  // Likelihood
  for(isite in 1:nsite){
  real lp_if_present = log(psi[isite]);
    for (j in 1:nrep){
      if (sampled[isite,j])
        lp_if_present += bernoulli_lpmf(y[isite, j] | p);
    }
      if(sum(y[isite, 1:nrep]) > 0){
        target += lp_if_present;
      } else {
        target += log_sum_exp(lp_if_present, log1m(psi[isite]));
      }
    }// end likelihood contribution
}
"


# data
y2003woNA <- y2003
y2003woNA[is.na(y2003woNA)] <- 0
data_list <- list(y = y2003woNA,
                  nrep = ncol(y2003),
                  nsite = nrow(y2003),
                  sampled = !is.na(y2003))
                  #x = simDFobs$X1,)

# params to monitor
pars <- c("beta","p")

# initial values
ini <- function(){list(beta = 0.5)}

# mcmc details
iter = 5000
warmup = 1000
chains = 2
seed <- 2024

# run stan
fit <- rstan::stan(model_code = constant,
                   data = data_list,
                   init = ini,
                   pars = pars,
                   iter = iter,
                   warmup = warmup,
                   chains = chains,
                   seed = seed)

# numerical summaries
stats <- data.frame(summary(fit)$summary)
round(stats,5)

# 
# beta   0.38    0.00 0.30  -0.20   0.18   0.37   0.58   0.99 5198.25    1
# p      0.69    0.00 0.06   0.58   0.66   0.70   0.73   0.80 5155.55    1

log(0.69497/(1-0.69497)) # is almost 0.85 like we found w/ ubms

log_lik <- extract_log_lik(fit, parameter_name = "lp__")
waic(log_lik)

