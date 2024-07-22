# load libraries
library(SSN)
library(SSNbayes)
library(tidyverse)
library(viridis)
library(RColorBrewer)

#---- CREATE NETWORK
seed <- 202401
set.seed(seed)

path <-  "./code/raw_logistic1.ssn"
raw.ssn <- createSSN(n = c(100), # nb of distinct random tree structures
                     obsDesign = systematicDesign(spacing=1),#binomialDesign(150),
                     #                   obsDesign = systematicDesign(spacing = 0.5),
                     importToR = TRUE,
                     path = path,
                     treeFunction = iterativeTreeLayout)
nrow(raw.ssn)
plot(raw.ssn)

createDistMat(raw.ssn, predpts = NULL, o.write=TRUE)

rawDFobs <- getSSNdata.frame(raw.ssn, "Obs")
nrow(rawDFobs)

# continuous cov
rawDFobs[,"X1"] <- rnorm(length(rawDFobs[,1]))

#---- SIMULATE DATA ON NETWORK

# partial sill = the spatially dependent (correlated) random error variance; sigma2_u
# range	= the correlation parameter; alpha_u
# nugget = the spatially independent (not correlated) random error variance

# I modified the SSN::SimulateOnSSN() function to save the psi's
source("code/SimulateOnSSN2.R")

sim.out <- SimulateOnSSN2(raw.ssn,
                          ObsSimDF = rawDFobs,
                          family = "Binomial",
                          formula = ~ X1,
                          coefficients = c(0.5, 1), 
                          CorModels = c("Exponential.taildown"),
                          use.nugget = FALSE,
                          use.anisotropy = FALSE,
                          #CorParms = c(2, 50, 0.1), # w/ nugget
                          CorParms = c(2, 10), 
                          addfunccol = "addfunccol")

# explore the prob of occupancy we simulate
mean(sim.out$psi) # mean occupancy
mean(sim.out$psi<0.1) # not too many 0 prob of occupancy
hist(sim.out$psi) # distribution over all sites

# get a Torgegram of the simulated prob of occupancy
sim.out$ssn.object@obspoints@SSNPoints[[1]]@point.data$psi <- sim.out$psi
ESVF <- Torgegram(sim.out$ssn.object, ResponseName = "psi")
plot(ESVF)

# plot the simulated network/data
sim.ssn <- sim.out$ssn.object
plot(sim.ssn,
     "Sim_Values",
     nclasses = 2,
     color.palette = c("blue","red"),
     breaktype = "user",
     brks = cbind(c(-.5,.5),c(.5, 1.5)))

# plot levels of psi
rawDFobs[,"psi"] <- sim.out$psi
rawDFobs$UTM_Xcoord <- sim.out$ssn.object@obspoints@SSNPoints[[1]]@point.coords[,1]
rawDFobs$UTM_Ycoord <- sim.out$ssn.object@obspoints@SSNPoints[[1]]@point.coords[,2]

# get lines to plot networks w/ ggplot
slot <- NULL
df_all <- NULL
line_id <- NULL
df0 <- SSN::as.SpatialLines(sim.out[[1]]) %>% st_as_sf() %>% st_union
df0 <- df0[[1]]
for (i in 1:length(df0)) {
  df <- data.frame(df0[i])
  df$slot <- i
  df$psi <- sim.out$psi[i]
  df$line_id <- as.numeric(as.character(df$slot))
  df_all <- rbind(df, df_all)
}
df_all <- dplyr::arrange(df_all, line_id)

df_all$addfunccol_cat <- cut(df_all$psi, 
                             breaks = seq(min(df_all$psi),
                                          max(df_all$psi),
                                          length.out=5),
                             labels = 1:4,
                             include.lowest = T)

# plot network
col <- 'gray'
ggplot(df_all) + 
  # geom_path(aes(X1, X2, group = slot, size = addfunccol_cat), 
  #           lineend = 'round', 
  #           linejoin = 'round', 
  #           col = col) +
  geom_path(aes(X1, X2, group = slot), 
            lineend = 'round', 
            linejoin = 'round', 
            col = col) +
  geom_point(data = rawDFobs,
             aes(x = UTM_Xcoord, y = UTM_Ycoord, col = psi), size = 1.75) +
  geom_text(data = rawDFobs,
            aes(x = UTM_Xcoord, y = UTM_Ycoord + .3, label = locID),size = 2) +
  scale_size_manual(values = seq(0.2,2,length.out = 5))+
  scale_color_viridis(option = 'C')+
  scale_shape_manual(values = c(16))+
  ylab("Latitude") +
  xlab("Longitude")+
#  coord_fixed()+
  theme_bw()+
  guides(size = 'none')+
  labs(size="",colour = "Pr(occupancy)")+
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=13),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13),
        axis.text.x = element_text(angle = 45, hjust=1),
        strip.background =element_rect(fill='white'))

# get data.frame of data points
#simDFobs <- getSSNdata.frame(sim.ssn, "Obs")

# sim values (the latent states for site occupied/non-occupied)
#z <- simDFobs$Sim_Values
z <- sim.out$ssn.object@obspoints@SSNPoints[[1]]@point.data$Sim_Values
z

# compare prob and realized
cbind(sim.out$psi, z)

# nb of sites
R <- length(z)

# Detection probability
p <- 0.6      

# nb of surveys/visits
T <- 5

# matrix of detections and non-detections
y <- matrix(NA, nrow = R, ncol = T)

# observation process
# Sample detection/nondetection observations from a Bernoulli (with p)
# if site is occupied, otherwise non-detection for sure
for (j in 1:T){
  y[,j] <- rbinom(n = R, size = 1, prob = z * p)
}

y

#-------------------- non-centered param wo nugget

# Stan code
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
  int nsite; // Number of sites
  int nrep; // Number of visits
  array[nsite, nrep] int y; // det/non-det data
  vector[nsite] x; // covariate x
  matrix[nsite, nsite] flow_con_mat; // flow conected matrix
  matrix[nsite, nsite] D; // downstream hydrological dist matrix
  matrix[nsite, nsite] h; // total hydrological dist
  real<lower=1> alpha_max;
}

parameters{
  real beta; // slope (intercept is k_bar)
  real k_bar; // intercept
  real <lower=0, upper = 1> p;
  real<lower=0> sigma_td; // sd of tail-down
  real<lower=0> alpha_td; // range of the tail-down model
  vector[nsite] z; // z-scores for intercept term (for non-centred parameterisation)
}

transformed parameters{
  vector[nsite] psi; // Probability of occurrence at each site i
  matrix[nsite, nsite] logSIGMA; // Covariance matrix
  matrix[nsite, nsite] SIGMA; // Covariance matrix
  matrix[nsite, nsite] L_SIGMA; // Cholesky
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
    psi[isite] = inv_logit(k_bar + k[isite] + beta * x[isite]);
  }

}

model{

  vector[nsite] log_psi; // Log of psi
  vector[nsite] log1m_psi; // Log of 1-psi

  // Priors
  beta ~ normal(0, 1.5);
  k_bar ~ normal(0, 1.5);
  z ~ normal(0, 1);
  sigma_td ~ uniform(0, 5); // sd tail-down, partial sill
  alpha_td ~ uniform(0, alpha_max); // prior range // uniform(0, alpha_max);

  // Likelihood
  // Log psi and log(1-psi)
  for(isite in 1:nsite){
    log_psi[isite] = log(psi[isite]);
    log1m_psi[isite] = log1m(psi[isite]);
  }

  // Likelihood
  for(isite in 1:nsite){
    if(sum(y[isite, 1:nrep]) > 0){
      target += log_psi[isite] + bernoulli_lpmf(y[isite, 1:nrep] | p);
    } else {
      target += log_sum_exp(log_psi[isite] + bernoulli_lpmf(y[isite, 1:nrep] | p), log1m_psi[isite]);
    }
  }// end likelihood contribution
}
"

# extract the network id (netID)
simDFobs <- getSSNdata.frame(sim.ssn, "Obs")
net_num <- as.numeric(unique(simDFobs$netID))

# create matrix of distances/weights
net <- net_num # network to use in the object
addfunccol <- 'addfunccol' # additive function column used for tail-up spatial weights
#path <- '/Users/oliviergimenez/Dropbox/OG/GITHUB/otter-crayfish-model/raw.ssn/'
mat_all <- SSNbayes::dist_weight_mat(path = path, net = net, addfunccol = addfunccol)

# data
data_list <- list(y = y,
                  nrep = ncol(y),
                  nsite = nrow(simDFobs),
                  x = simDFobs$X1,
                  flow_con_mat = mat_all$flow.con.mat, #flow connected matrix
                  D = mat_all$D, #downstream hydro distance matrix
                  h = mat_all$H, # total stream distance
                  alpha_max = 4 * max(mat_all$H))

# params to monitor
pars <- c("sigma_td", "alpha_td", "k_bar","beta","p","psi")

# initial values
ini <- function(){list(sigma_td = .5, k_bar = 0, beta = 0.5)}

# mcmc details
iter = 5000
warmup = 1000
chains = 2

# run stan
fit <- rstan::stan(model_code = ssnlogistic,
                   data = data_list,
                   init = ini,
                   pars = pars,
                   iter = iter,
                   warmup = warmup,
                   chains = chains,
                   seed = seed)

# check div
#rstan::check_divergences(fit)
rstan::check_hmc_diagnostics(fit)

# pairs plots
pairs(fit, pars = c("sigma_td", "alpha_td", "k_bar","beta","p"))

# trace plots
rstan::traceplot(fit, pars = c("sigma_td", "alpha_td", "k_bar","beta","p"))

# numerical summaries
stats <- data.frame(summary(fit)$summary)
round(stats,2)

# posterior densities
library(bayesplot)

mcmc_dens_overlay(
  as.array(fit),
  pars = c("sigma_td", "alpha_td","beta","p"),
  facet_args = list(nrow = 2))

# extract MCMC samples from fitted model
post <- rstan::extract(fit)

# compute posterior mean and HPDI for prob of occupancy
psi_mu <- apply(post$psi, 2, mean)
psi_ci <- apply(post$psi, 2, rethinking::HPDI, prob = 0.89)

# compute RMSPE root mean square prediction error
sqrt(sum((psi_mu - sim.out$psi)^2) / length(psi_mu))

# compare estimated vs true values of prob of occupancy
par(mfrow = c(1,1))
plot(NULL, xlim = c(0,1), ylim = c(0,1), xlab = "True value", ylab = "Model estimate", main = expression(psi))
abline(a = 0, b = 1, lty = 2) # true = predicted
points(x = sim.out$psi, y = psi_mu, pch = 16)
for(i in 1:length(sim.out$psi)){
  lines(x = rep(sim.out$psi[i],2), y = c(psi_ci[1,i], psi_ci[2,i]))
}

# METTRE LES PREDICTIONS OCCUPANCY SUR UNE CARTE
# OU LA DIFFERENCE ENTRE OCC SIMUL ET OCC PREDITES.

library(AHMbook)
str(tmp <- simExpCorrRF(theta = 0.0001, size = 200)) 
str(tmp <- simExpCorrRF(theta = 1, size = 200)) 
str(tmp <- simExpCorrRF(theta = 5, size = 200)) 
str(tmp <- simExpCorrRF(theta = 10, size = 200)) 
str(tmp <- simExpCorrRF(theta = 100, size = 200)) 
str(tmp <- simExpCorrRF(theta = 10000, size = 200))
