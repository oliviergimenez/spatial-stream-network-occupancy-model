#----------------------#
#--- load libraries ---#
#----------------------#

library(SSN2) # spatial modeling on stream networks
library(tidyverse) # importing, tidying, manipulating, and visualizing data
library(sf) # working with spatial data
library(rstan) # running STAN from R for Bayesian analyses
library(bayesplot) # plotting results after Bayesian analyses
library(geodata) # downloading geographic data
library(ggspatial) # interacting with spatial data using ggplot2
library(rmapshaper) # wrapping the mapshaper javascript library to simplify spatial object

#----------------------------------#
#--- wrangle and visualize data ---#
#----------------------------------#

# get occupancy data

sites_occ <- st_read("data/shp/sites.shp")
mask <- c(25:71,73:81) # take single watershed
dat <- sites_occ[mask,]
# take 3 first occasions
y2003 <- cbind(dat$CONTACT_1,
               dat$CONTACT_2,
               dat$CONTACT_3)

# get network data

network <- ssn_import(
  path = 'data/nc.ssn',
  overwrite = TRUE
)

load("data/mat_all.RData")
flow_con_mat <- mat_all$flow.con.mat # flow connected matrix
D <- mat_all$D # downstream hydro distance matrix
h <- mat_all$H # total stream distance
alpha_max <- 4 * max(mat_all$H) # upper bound for scale parameter

# visualize data

# 0. france

france <- gadm(country = "FR" , level = 1,  path = tempdir())
france <- st_as_sf(france) %>% st_transform(st_crs(network$obs))
france <- ms_simplify(input = as(france, 'Spatial')) %>%
  st_as_sf() # simplify spatial object for faster plotting
p1 <- ggplot(data = france) +
  geom_sf(fill = "white", lwd = 1) +
  layer_spatial(st_bbox(network$obs[mask,])) +
  annotation_north_arrow(location = "tr", # add orientation, bottom left
                         which_north = "true", 
                         height = unit(2.5, "cm"),
                         width = unit(2.5, "cm"),
                         pad_x = unit(1.5, "cm"),
                         pad_y = unit(1.5, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 30)) +
  annotation_scale(location = "bl", 
                   text_cex = 2,
                   pad_x = unit(1, "cm"),
                   pad_y = unit(1, "cm")) + # add scale, bottom right
  theme_bw(base_size = 30) +
  theme(legend.text = element_text(size = 30))

ggsave("outputs/france.png", dpi = 600, width = 15, height = 15)

# 1. locations

ggplot() +
  geom_sf(data = network$edges, lwd = 0.1, col = "darkblue") +
  geom_sf(data = network$obs[mask,], color = "brown", size = 1.5) +
  coord_sf(xlim = c(min(st_coordinates(network$obs[mask,])[,1]), 
                    max(st_coordinates(network$obs[mask,])[,1])), # min & max of x values
           ylim = c(min(st_coordinates(network$obs[mask,])[,2]), 
                    max(st_coordinates(network$obs[mask,])[,2]))) + # min & max of y values
  theme_bw()

# mapview::mapview(st_bbox(network$obs[mask,]))

# 2. number of detections

nb_det <- cbind(network$obs[mask,]$CONTACT_1, network$obs[mask,]$CONTACT_2, network$obs[mask,]$CONTACT_3)
tot_nb_det <- as.factor(apply(nb_det, 1, sum, na.rm = TRUE))
bla <- network$obs[mask,]
bla$tot_nb_det <- tot_nb_det
p2 <- ggplot() +
  geom_sf(data = network$edges, lwd = 0.1, col = "darkblue") +
  geom_sf(data = bla, aes(col = tot_nb_det), size = 3) +
  scale_color_viridis_d(option = "C", name = "# detections") + 
coord_sf(xlim = c(min(st_coordinates(network$obs[mask,])[,1]), 
                    max(st_coordinates(network$obs[mask,])[,1])), # min & max of x values
           ylim = c(min(st_coordinates(network$obs[mask,])[,2]), 
                    max(st_coordinates(network$obs[mask,])[,2]))) + # min & max of y values
  theme_bw(base_size = 30) +
  theme(legend.text = element_text(size=30)) +
  guides(color = guide_legend(override.aes = list(size = 7)))

ggsave("outputs/detections.png", dpi = 600, width = 15, height = 10)

# 3. pop density

bla <- network$obs[mask,]
p3 <- ggplot() +
  geom_sf(data = network$edges, lwd = 0.1, col = "darkblue") +
  geom_sf(data = bla %>% mutate(class = cut(ZT200_K, seq(0, 270, 30))), aes(col = class), size = 3) +
  scale_color_viridis_d(option = "D", direction = -1, name = "pop density") + 
  coord_sf(xlim = c(min(st_coordinates(network$obs[mask,])[,1]), 
                    max(st_coordinates(network$obs[mask,])[,1])), # min & max of x values
           ylim = c(min(st_coordinates(network$obs[mask,])[,2]), 
                    max(st_coordinates(network$obs[mask,])[,2]))) + # min & max of y values
  theme_bw(base_size = 30) +
  theme(legend.text = element_text(size=30)) +
  guides(color = guide_legend(override.aes = list(size = 7)))

ggsave("outputs/density.png", dpi = 600, width = 15, height = 10)

#-----------------------------------------------------------------#
#--- write model with Stan code / WITH spatial autocorrelation ---#
#-----------------------------------------------------------------#

# freely inspired from 
# https://peter-stewart.github.io/blog/gaussian-process-occupancy-tutorial/ for the occupancy component and
# https://github.com/EdgarSantos-Fernandez/SSNbayes/blob/main/R/all_func.R for the spatial component
ssnlogistic <- "
functions{
  matrix cov_ssn(matrix h, matrix D, matrix flow_con_mat, real sigma_td, real alpha_td) {
    int nsite = dims(flow_con_mat)[1];
    matrix[nsite, nsite] logC_td;
    for (i in 1:nsite) { // Tail-down exponential model
      for (j in 1:nsite) {
        if(flow_con_mat[i,j] == 1){ // if points are flow connected
          logC_td[i,j] = 2 * log(sigma_td) - 3 * h[i,j] / alpha_td;
        }
        else{// if points are flow unconnected
          logC_td[i,j] = 2 * log(sigma_td) - 3 * (D[i,j] + D[j,i]) / alpha_td;
        }
      }
    }
    return logC_td;
  }
}

data{
  int nsite; // number of sites
  int nrep; // number of visits
  array[nsite, nrep] int y; // det/non-det data
  array[nsite, nrep] int sampled; // indicator for whether site is sampled or not (NA)
  vector[nsite] agr; // covariate agr
  vector[nsite] pop; // covariate pop
  matrix[nsite, nsite] flow_con_mat; // flow conected matrix
  matrix[nsite, nsite] D; // downstream hydrological dist matrix
  matrix[nsite, nsite] h; // total hydrological dist
  real<lower=1> alpha_max;
}

parameters{
  real betaagr; // slope 
  real betapop; // slope 
  real k_bar; // intercept
  real <lower=0, upper = 1> p; // detection probability
  real<lower=0> sigma_td; // sd of tail-down
  real<lower=0> alpha_td; // range of the tail-down model
  vector[nsite] z; // z-scores for intercept term (for non-centred parameterisation)
}

transformed parameters{
  vector[nsite] psi; // probability of occurrence at each site i
  matrix[nsite, nsite] logSIGMA; // covariance matrix
  matrix[nsite, nsite] SIGMA; // covariance matrix
  matrix[nsite, nsite] L_SIGMA; // Cholesky decomposition
  vector[nsite] k; // perturbation from k_bar
  logSIGMA = cov_ssn(h, D, flow_con_mat, sigma_td, alpha_td);
  for(isite in 1:nsite){
  for(jsite in 1:nsite){
    SIGMA[isite,jsite] = exp(logSIGMA[isite,jsite]);
  }
  }
  L_SIGMA = cholesky_decompose(SIGMA);
  k = L_SIGMA * z;
  for(isite in 1:nsite){
    psi[isite] = inv_logit(k_bar + k[isite] + betaagr * agr[isite] + betapop * pop[isite]);
  }
}

model{

  vector[nsite] log_psi; // log of psi
  vector[nsite] log1m_psi; // log of 1-psi

  // Priors
  betaagr ~ normal(0, 1.5);
  betapop ~ normal(0, 1.5);
  k_bar ~ normal(0, 1.5);
  z ~ normal(0, 1);
  sigma_td ~ uniform(0, 5); // sd tail-down, partial sill
  alpha_td ~ uniform(0, alpha_max); // prior range

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
  }
}
"

# prepare data
agr <- as.vector(scale(dat$P100ZTC))
pop <- as.vector(scale(log(dat$ZT200_K + 1)))
y2003woNA <- y2003
y2003woNA[is.na(y2003woNA)] <- 0 # data need to be numeric
data_list <- list(y = y2003woNA, # detections/non-detections
                  sampled = !is.na(y2003), # TRUE if cell sampled, FALSE otherwise
                  nrep = ncol(y2003), # number of visits
                  nsite = nrow(y2003), # number of sites
                  agr = agr, # agric covariate
                  pop = pop, # pop density covariate
                  flow_con_mat = mat_all$flow.con.mat, #flow connected matrix
                  D = mat_all$D, #downstream hydro distance matrix
                  h = mat_all$H, # total stream distance
                  alpha_max = 4 * max(mat_all$H))

# specify parameters to monitor
pars <- c("sigma_td", "alpha_td", "k_bar","betaagr","betapop","p","psi")

# pick initial values
ini <- function(){list(sigma_td = .5, k_bar = 0, betaagr = 0.5, betapop = 0.5)}

# specify mcmc details
iter <- 15000
warmup <- 5000
chains <- 2
seed <- 202406

# run stan
fit <- stan(model_code = ssnlogistic,
            data = data_list,
            init = ini,
            pars = pars,
            iter = iter,
            warmup = warmup,
            chains = chains,
            seed = seed)

# run convergence diagnostics
check_hmc_diagnostics(fit)

# visualize pairs plots
pairs(fit, pars = c("sigma_td", "alpha_td", "k_bar","betaagr","betapop","p"))

# get trace plots
traceplot(fit, pars = c("sigma_td", "alpha_td", "k_bar","betaagr","betapop","p"))

# calculate numerical summaries
stats <- data.frame(summary(fit)$summary)
round(stats,2)

#               mean  se_mean     sd       X2.5.     X25.      X50.      X75.      X97.5. n_eff   Rhat
# sigma_td      3.26    0.03      1.10     1.01      2.45      3.36      4.17       4.92 1210.86    1
# alpha_td 538397.22 9286.98 348996.27 79556.89 237331.02 458334.35 800396.75 1260235.22 1412.19    1
# k_bar        -0.05    0.03      1.15    -2.37     -0.79     -0.03      0.69       2.20 1399.25    1
# betaagr       0.60    0.02      0.66    -0.67      0.18      0.59      1.02       1.96 1612.73    1
# betapop      -0.96    0.01      0.61    -2.24     -1.34     -0.91     -0.53       0.17 2079.17    1
# p             0.71    0.00      0.05     0.59      0.67      0.71      0.74       0.80 2592.97    1

# relabel param 
my_labeller <- as_labeller(
  x = c(
    'sigma_td' = 'sigma',
    'alpha_td' = 'theta',
    'k_bar' = 'beta[0]', 
    'betapop' = 'beta[1]',
    'betaagr' = 'beta[2]',
    'p' = 'p'
  ), 
  default = label_parsed
)

# get posterior densities
mcmc_dens_overlay(
  as.array(fit),
  pars = c("k_bar","betapop","betaagr","sigma_td", "alpha_td","p"),
  facet_args = list(ncol = 1, labeller = my_labeller)) + 
  theme_bw() +
  theme(legend.position = "none") + 
  facet_text(size = 15)

ggsave("outputs/posterior.png", dpi = 600, width = 5, height = 10)

# get trace
mcmc_trace(as.array(fit),  
           pars = c("k_bar","betapop","betaagr","sigma_td", "alpha_td","p"),
           facet_args = list(ncol = 1, labeller = my_labeller)) + 
  theme_bw() +
  theme(legend.position = "none") + 
  facet_text(size = 15)

ggsave("outputs/trace.png", dpi = 600, width = 5, height = 10)

# # both posterior/trace side-by-side
# mcmc_combo(
#   x = as.array(fit), 
#   #widths = c(1, 2),
#   pars = c("k_bar","betapop","betaagr","sigma_td", "alpha_td","p"),
#   combo = c("dens", "trace"),
#   facet_args = list(
#     ncol = 1,
#     labeller = my_labeller),
#   gg_theme = ggplot2::theme_bw() + facet_text(size = 15) + legend_none())

#--------------------------------------------------------------------#
#--- write model with Stan code / WITHOUT spatial autocorrelation ---#
#--------------------------------------------------------------------#

constant <- "
data{
  int nsite; // number of sites
  int nrep; // number of visits
  vector[nsite] agr; // covariate agr
  vector[nsite] pop; // covariate pop
  array[nsite, nrep] int y; // det/non-det data
  array[nsite, nrep] int sampled; // indicator for whether site is sampled or not (NA)
  // vector[nsite] x; // covariate x
}

parameters{
  real betaagr; // slope 
  real betapop; // slope 
  real k_bar; // intercept
  real <lower=0, upper = 1> p;
}

transformed parameters{
  vector[nsite] psi; // prob of occurrence at each site i
  for(isite in 1:nsite){
    psi[isite] = inv_logit(k_bar + betaagr * agr[isite] + betapop * pop[isite]);
  }

}

model{

  vector[nsite] log_psi; // log of psi
  vector[nsite] log1m_psi; // log of 1-psi

  // Priors
  betaagr ~ normal(0, 1.5);
  betapop ~ normal(0, 1.5);
  k_bar ~ normal(0, 1.5);
  p ~ uniform(0, 1);

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
    }
}
"

#--- prepare data
data_list <- list(y = y2003woNA,
                  nrep = ncol(y2003),
                  nsite = nrow(y2003),
                  agr = agr, # agric covariate
                  pop = pop, # pop density covariate
                  sampled = !is.na(y2003))

# specify parameters to monitor
pars <- c("betaagr","betapop","p")

# pick initial values
ini <- function(){list(betaagr = 0.5, betapop = 0.5)}

# run stan
fit <- stan(model_code = constant,
                   data = data_list,
                   init = ini,
                   pars = pars,
                   iter = iter,
                   warmup = warmup,
                   chains = chains,
                   seed = seed)

# calculate numerical summaries
stats <- data.frame(summary(fit)$summary)
round(stats,2)

#          mean se_mean  sd    X2.5.  X25.   X50.   X75.  X97.5. n_eff    Rhat
#betaagr   0.05    0.00 0.37  -0.68  -0.20   0.05   0.29   0.77 16187.89    1
#betapop  -1.10    0.00 0.42  -1.99  -1.35  -1.07  -0.81  -0.34 15835.27    1
#p         0.69    0.00 0.06   0.58   0.66   0.69   0.73   0.80 17076.84    1


# bayesplot:
#   Gabry J, Mahr T (2022). “bayesplot: Plotting for Bayesian Models.” R package version 1.10.0, <https://mc-stan.org/bayesplot/>.
#   Gabry J, Simpson D, Vehtari A, Betancourt M, Gelman A (2019). Visualization in Bayesian workflow. J. R. Stat. Soc. A, 182, 389-402. doi:10.1111/rssa.12378 <https://doi.org/10.1111/rssa.12378>.
#
