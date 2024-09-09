#----------------------#
#--- load libraries ---#
#----------------------#

library(SSNbayes)
library(SSN)
library(SSN2) # spatial modeling on stream networks
library(tidyverse) # importing, tidying, manipulating, and visualizing data
library(sf) # working with spatial data
library(viridis)
library(RColorBrewer)
library(rstan) # running STAN from R for Bayesian analyses

# I modified the SSN::SimulateOnSSN() function to save the occupancy probabilities, the psi's
source("code/SimulateOnSSN2.R")

#--------------------------------------------------------------------#
#----- write model with Stan code / WITH spatial autocorrelation ----#
#--------------------------------------------------------------------#

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
  vector[nsite] x; // covariate 
  matrix[nsite, nsite] flow_con_mat; // flow conected matrix
  matrix[nsite, nsite] D; // downstream hydrological dist matrix
  matrix[nsite, nsite] h; // total hydrological dist
  real<lower=1> alpha_max;
}

parameters{
  real beta; // slope 
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
    psi[isite] = inv_logit(k_bar + k[isite] + beta * x[isite]);
  }
}

model{

  vector[nsite] log_psi; // log of psi
  vector[nsite] log1m_psi; // log of 1-psi

  // Priors
  beta ~ normal(0, 1.5);
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

#--------------------------------------------------------------------#
#--- write model with Stan code / WITHOUT spatial autocorrelation ---#
#--------------------------------------------------------------------#

constant <- "
data{
  int nsite; // number of sites
  int nrep; // number of visits
  vector[nsite] x; // covariate 
  array[nsite, nrep] int y; // det/non-det data
  array[nsite, nrep] int sampled; // indicator for whether site is sampled or not (NA)
}

parameters{
  real beta; // slope 
  real k_bar; // intercept
  real <lower=0, upper = 1> p;
}

transformed parameters{
  vector[nsite] psi; // prob of occurrence at each site i
  for(isite in 1:nsite){
    psi[isite] = inv_logit(k_bar + beta * x[isite]);
  }

}

model{

  vector[nsite] log_psi; // log of psi
  vector[nsite] log1m_psi; // log of 1-psi

  // Priors
  beta ~ normal(0, 1.5);
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

#----------------------#
#----- simul data -----#
#----------------------#

seed <- 202409
set.seed(seed)

nb_simul <- 100
estim_with <- list()
estim_without <- list()
rmspe <- matrix(0, nrow = nb_simul, ncol = 2)

for (i in 1:nb_simul){
  # create network
  path <-  paste0("./code/raw_logistic",i,".ssn")
  
  skip_to_next <- FALSE
  tryCatch(raw.ssn <- createSSN(n = c(100), # nb of distinct random tree structures
                                obsDesign = systematicDesign(spacing=1),# binomialDesign(150)
                                importToR = TRUE,
                                path = path,
                                treeFunction = iterativeTreeLayout), 
           error = function(e) { skip_to_next <<- TRUE})
  if(skip_to_next) { next }    
  
  # nrow(raw.ssn)
  # plot(raw.ssn)
  
  createDistMat(raw.ssn, predpts = NULL, o.write=TRUE)
  
  rawDFobs <- getSSNdata.frame(raw.ssn, "Obs")
  # nrow(rawDFobs)
  
  # add a continuous explanatory variable
  rawDFobs[,"X1"] <- rnorm(length(rawDFobs[,1]))
  
  # partial sill = the spatially dependent (correlated) random error variance; sigma2_u
  # range	= the correlation parameter; alpha_u
  # nugget = the spatially independent (not correlated) random error variance
  
  # simulate data on network
  
  skip_to_next <- FALSE
  tryCatch(sim.out <- SimulateOnSSN2(raw.ssn,
                                     ObsSimDF = rawDFobs,
                                     family = "Binomial",
                                     formula = ~ X1,
                                     coefficients = c(0.5, 1), 
                                     CorModels = c("Exponential.taildown"),
                                     use.nugget = FALSE,
                                     use.anisotropy = FALSE,
                                     #CorParms = c(2, 50, 0.1), # w/ nugget
                                     CorParms = c(2, 10), 
                                     addfunccol = "addfunccol"), 
           error = function(e) { skip_to_next <<- TRUE})
  if(skip_to_next) { next }    
  
  # explore the prob of occupancy we simulate
  # mean(sim.out$psi) # mean occupancy
  # mean(sim.out$psi<0.1) # not too many 0 prob of occupancy
  # hist(sim.out$psi) # distribution over all sites
  
  # plot the simulated network/data
  sim.ssn <- sim.out$ssn.object
  # plot(sim.ssn,
  #      "Sim_Values",
  #      nclasses = 2,
  #      color.palette = c("blue","red"),
  #      breaktype = "user",
  #      brks = cbind(c(-.5,.5),c(.5, 1.5)))
  
  # plot levels of psi
  rawDFobs[,"psi"] <- sim.out$psi
  rawDFobs$UTM_Xcoord <- sim.out$ssn.object@obspoints@SSNPoints[[1]]@point.coords[,1]
  rawDFobs$UTM_Ycoord <- sim.out$ssn.object@obspoints@SSNPoints[[1]]@point.coords[,2]
  
  # # get lines to plot networks w/ ggplot
  # slot <- NULL
  # df_all <- NULL
  # line_id <- NULL
  # df0 <- SSN::as.SpatialLines(sim.out[[1]]) %>% st_as_sf() %>% st_union
  # df0 <- df0[[1]]
  # for (i in 1:length(df0)) {
  #   df <- data.frame(df0[i])
  #   df$slot <- i
  #   df$psi <- sim.out$psi[i]
  #   df$line_id <- as.numeric(as.character(df$slot))
  #   df_all <- rbind(df, df_all)
  # }
  # df_all <- dplyr::arrange(df_all, line_id)
  # df_all$addfunccol_cat <- cut(df_all$psi, 
  #                              breaks = seq(min(df_all$psi),
  #                                           max(df_all$psi),
  #                                           length.out=5),
  #                              labels = 1:4,
  #                              include.lowest = T)
  # 
  # # plot network
  # col <- 'gray'
  # ggplot(df_all) + 
  #   geom_path(aes(X1, X2, group = slot), 
  #             lineend = 'round', 
  #             linejoin = 'round', 
  #             col = col) +
  #   geom_point(data = rawDFobs,
  #              aes(x = UTM_Xcoord, y = UTM_Ycoord, col = psi), size = 1.75) +
  #   geom_text(data = rawDFobs,
  #             aes(x = UTM_Xcoord, y = UTM_Ycoord + .3, label = locID),size = 2) +
  #   scale_size_manual(values = seq(0.2,2,length.out = 5)) +
  #   scale_color_viridis(option = 'C') +
  #   scale_shape_manual(values = c(16)) +
  #   ylab("Latitude") +
  #   xlab("Longitude") +
  #   theme_bw() +
  #   guides(size = 'none') +
  #   labs(size="",colour = "Pr(occupancy)") +
  #   theme(axis.text=element_text(size=12), 
  #         axis.title=element_text(size=13),
  #         legend.text=element_text(size=13),
  #         legend.title=element_text(size=13),
  #         axis.text.x = element_text(angle = 45, hjust=1),
  #         strip.background =element_rect(fill='white'))
  
  # sim values (the latent states for site occupied/non-occupied)
  z <- sim.out$ssn.object@obspoints@SSNPoints[[1]]@point.data$Sim_Values
  
  # # compare prob and realized
  # cbind(sim.out$psi, z)
  
  # number of sites
  R <- length(z)
  
  # detection probability
  p <- 0.6      
  
  # number of surveys/visits
  T <- 5
  
  # matrix of detections and non-detections
  y <- matrix(NA, nrow = R, ncol = T)
  
  # simulate the observation process
  # by drawing detection/non-detection observations from a Bernoulli (with probability p)
  # if site is occupied, otherwise non-detection for sure
  for (j in 1:T){
    y[,j] <- rbinom(n = R, size = 1, prob = z * p)
  }
  # y
  
  
  #----------------------#
  #----- fit models -----#
  #----------------------#
  
  
  # extract the network id (netID)
  simDFobs <- getSSNdata.frame(sim.ssn, "Obs")
  net_num <- as.numeric(unique(simDFobs$netID))
  
  # create matrix of distances/weights
  net <- net_num # network to use in the object
  addfunccol <- 'addfunccol' # additive function column used for tail-up spatial weights
  #path <- '/Users/oliviergimenez/Dropbox/OG/GITHUB/otter-crayfish-model/raw.ssn/'
  mat_all <- SSNbayes::dist_weight_mat(path = path, net = net, addfunccol = addfunccol)
  
  
  #-------------------------------------------------------#
  #------------ WITH spatial autocorrelation -------------#
  #-------------------------------------------------------#
  
  # data
  data_list <- list(y = y,
                    nrep = ncol(y),
                    nsite = nrow(simDFobs),
                    sampled = !is.na(y), # TRUE if cell sampled, FALSE otherwise
                    x = simDFobs$X1,
                    flow_con_mat = mat_all$flow.con.mat, #flow connected matrix
                    D = mat_all$D, #downstream hydro distance matrix
                    h = mat_all$H, # total stream distance
                    alpha_max = 4 * max(mat_all$H))
  
  # params to monitor
  pars <- c("sigma_td", "alpha_td", "k_bar","beta","p","psi")
  
  # initial values
  ini <- function(){list(sigma_td = 2, alpha_td = 10, k_bar = 0, beta = 0.5)}
  
  # mcmc details
  iter <- 5000
  warmup <- 1500
  chains <- 1
  
  # run stan
  fit <- rstan::stan(model_code = ssnlogistic,
                     data = data_list,
                     init = ini,
                     pars = pars,
                     iter = iter,
                     warmup = warmup,
                     chains = chains,
                     seed = seed)
  
  # numerical summaries
  stats <- data.frame(summary(fit)$summary)
  #round(stats,2)
  
  estim_with[[i]] <- stats[1:5,]
  
  # extract MCMC samples from fitted model
  post <- rstan::extract(fit)
  
  # compute posterior mean and HPDI for prob of occupancy
  psi_mu <- apply(post$psi, 2, mean)
  psi_ci <- apply(post$psi, 2, rethinking::HPDI, prob = 0.89)
  
  # compute RMSPE root mean square prediction error
  rmspe[i,1] <- sqrt(sum((psi_mu - sim.out$psi)^2) / length(psi_mu))
  
  #-------------------------------------------------------#
  #---------- WITHOUT spatial autocorrelation ------------#
  #-------------------------------------------------------#
  
  # data
  data_list <- list(y = y,
                    nrep = ncol(y),
                    nsite = nrow(simDFobs),
                    sampled = !is.na(y), # TRUE if cell sampled, FALSE otherwise
                    x = simDFobs$X1)
  
  # params to monitor
  pars <- c("k_bar","beta","p","psi")
  
  # initial values
  ini <- function(){list(k_bar = 0, beta = 0.5)}
  
  # mcmc details
  iter <- 3000
  warmup <- 1000
  chains <- 1
  
  # run stan
  fit_ct <- rstan::stan(model_code = constant,
                        data = data_list,
                        init = ini,
                        pars = pars,
                        iter = iter,
                        warmup = warmup,
                        chains = chains,
                        seed = seed)
  
  # numerical summaries
  stats_ct <- data.frame(summary(fit_ct)$summary)
  #round(stats_ct,2)
  
  estim_without[[i]] <- stats_ct[1:3,]
  
  # extract MCMC samples from fitted model
  post <- rstan::extract(fit_ct)
  
  # compute posterior mean and HPDI for prob of occupancy
  psi_mu <- apply(post$psi, 2, mean)
  psi_ci <- apply(post$psi, 2, rethinking::HPDI, prob = 0.89)
  
  # compute RMSPE root mean square prediction error
  rmspe[i,2] <- sqrt(sum((psi_mu - sim.out$psi)^2) / length(psi_mu))
  
}

save(rmspe, estim_with, estim_without, file = "code/simuls.RData")

mask <- apply(rmspe, 1, sum) != 0

apply(rmspe[mask,], 2, mean)
# 0.1805907 0.2313576
apply(rmspe[mask,], 2, median)
# 0.1826795 0.2297532

index <- 1:nb_simul
res_without <- NULL
res_with <- NULL
for (i in index[mask]){
  res_without <- cbind(res_without, estim_without[[i]][,1]) # k_bar, beta, p
  res_with <- cbind(res_with, estim_with[[i]][,1]) # sigma_td, alpha_td, k_bar, beta, p
}

k_bar <- 0.5
beta <- 1
sigma_td <- 2
alpha_td <- 10
p <- 0.6

true_without <- c(k_bar, beta, p)
true_with <- c(sigma_td, alpha_td, k_bar, beta, p)

(apply(res_without, 1, mean) - true_without)/true_without * 100
# -28.300489624 -26.242607149   0.003016397
(apply(res_with, 1, mean) - true_with)/true_with * 100
# 12.19027037 330.21113925 -43.64560414   0.66810783  -0.04667957

ratio <- res_with[4,] / res_with[5,]
true_ratio <- true_with[4] / true_with[5]
(mean(ratio) - true_ratio)/true_ratio*100
# 0.9220916
