#### information ####
# script to extract node centralities to compare social personality

#### set up ####
### load packages
# library(tidyverse) ; library(dplyr) ; library(cmdstanr) ; library(igraph) ; library(janitor) ; library(lubridate) ; library(hms) ; library(LaplacesDemon)
library(cmdstanr, lib.loc = '../packages/')    # library(cmdstanr)
library(tidyverse, lib.loc = '../packages/')   # library(tidyverse)
library(dplyr, lib.loc = '../packages/')       # library(dplyr)
library(igraph, lib.loc = '../packages/')      # library(igraph)
library(janitor, lib.loc = '../packages/')     # library(janitor)
library(lubridate, lib.loc = '../packages/')   # library(lubridate)

### set stan path
set_cmdstan_path('../../packages/.cmdstan/cmdstan-2.31.0')

### set seed
set.seed(12345)

## set ggplot theme
theme_set(theme_bw())

### create file of output graphs
pdf('../outputs/simulation_centralities_modelprep.pdf', width = 20, height = 15)

#### simulate node centralities ####
## load data
load('step4_centralitytrends/model_ready.RData')
sim <- df3 %>% 
  dplyr::select(-cent_mean, -cent_stdv) %>%  # remove measured values to replace with simulated ones
  dplyr::select(-first_sighting, -last_sighting, -age_diff_first_last, -start_yr, -end_yr) %>% # remove unnecessary clutter
  mutate(node_id = as.integer(as.factor(id))) %>% 
  relocate(node, .after = casename) %>% 
  relocate(node_id, .after = node) %>% 
  relocate(population_size, .before = popn_std) %>% 
  mutate(node_year = paste0(node_id, '_', year))
rm(list = ls()[! ls() %in% c('sim')])

## set 'true' parameters
intercept <- -2
node_effect <- rnorm(10, 0, 0.5)
year_effect <- -0.2
popn_effect <- 0.3
stdv_cent <- 0.1

## set model run parameters
n_chains <- 4
n_samples <- 1000

## simulate 'true' mean centrality
sim$cent_mean_logit <- intercept + node_effect[sim$node_id] + sim$year_std * year_effect + sim$popn_std * popn_effect

## simulate 'true' centrality measures
cent_full <- matrix(NA, nrow = n_samples*n_chains, ncol = nrow(sim),
                    dimnames = list(1:(n_chains*n_samples),
                                    sim$node_year))
for(j in 1:ncol(cent_full)){
  cent_full[,j] <- rnorm(n = nrow(cent_full),
                         mean = sim$cent_mean_logit[j],
                         sd = stdv_cent)
}

## clean up and save workspace
print('centralities simulated')

#### plot selected centralities against age, year and population size ####
## cut down to only half so not reusing sightings of same elephants within multiple data points
sim %>% 
  ggplot(aes(x = age, y = cent_mean_logit, colour = id))+
  geom_point()+
  geom_line()+
  scale_colour_viridis_d()+
  theme_bw()
sim %>% 
  ggplot(aes(x = year, y = cent_mean_logit, colour = id))+
  geom_point()+
  geom_line()+
  scale_colour_viridis_d()+
  theme_bw()

## plot number of elephants in population vs centrality
sim %>% 
  ggplot(aes(x = population_size, y = cent_mean_logit,
             colour = id))+
  geom_point()+
  geom_smooth(se = F, method = 'lm')+
  scale_colour_viridis_d()+
  theme_bw()
sim %>% 
  ggplot(aes(x = year, y = population_size))+
  geom_point()+
  geom_smooth()+
  theme_bw()

## plot against ID
sim %>% 
  ggplot(aes(x = id, y = cent_mean_logit))+
  geom_boxplot(notch = T)+
  theme_bw()

#### prior predictive check -- total effect of year / personality ####
n_years <- (max(sim$year) - min(sim$year)) + 1

n <- 100
beta_node <- rnorm(n, 0, 0.5)
beta_year <- rnorm(n, 0, 0.5)
min_raw <- min(sim$cent_mean_logit)
max_raw <- max(sim$cent_mean_logit)
prior_intcp <- rnorm(n, LaplacesDemon::logit(0.05), 0.8) # taking the intercept from the logit of results from Chiyo 2011 (doesn't state mean/median centrality so estimated from graph based on where correlation line would cross x = 0)
par(mfrow = c(1,1), mai = c(1,1,0.5,0.5))
plot(NULL, las = 1, xlab = 'year', ylab = 'eigenvector',
     ylim = c(min_raw-3, max_raw+3),
     xlim = c(min(sim$year_std), max(sim$year_std)))
abline(h = min_raw, lty = 2) ; abline(h = max_raw, lty = 2)
for(i in 1:n){
  lines(x = seq(min(sim$year_std), max(sim$year_std), length.out = 2),
        y = prior_intcp[i] + 
          beta_year[i]*c(min(sim$year_std), max(sim$year_std)) + beta_node[i],
        col = rgb(0,0,1,0.4))
}

#### prior predictive check -- year after controlling for population size ####
sim$popn_std <- (sim$population_size - mean(sim$population_size)) / sd(sim$population_size)

beta_node <- rnorm(n, 0, 0.5)
beta_year <- rnorm(n, 0, 0.5)
beta_popn <- rnorm(n, 0, 0.5)

plot(NULL, las = 1, xlab = 'year', ylab = 'eigenvector',
     ylim = c(min_raw-3, max_raw+3),
     xlim = c(min(sim$year_std), max(sim$year_std)))
abline(h = min_raw, lty = 2) ; abline(h = max_raw, lty = 2)
for(i in 1:n){
  lines(x = seq(min(sim$year_std), max(sim$year_std), length.out = 2),
        y = prior_intcp[i] + 
          beta_year[i]*c(min(sim$year_std), max(sim$year_std)) + 
          beta_popn[i]*c(min(sim$popn_std), max(sim$popn_std)) + 
          beta_node[i],
        col = rgb(0,0,1,0.4))
}

#### normal approximation -- IS THIS CORRECT?? IN OTHER CHAPTER, I ONLY PLOTTED THE DISTRIBUTIONS FOR ONE ELEPHANT, NOT THE ENTIRE TIME WINDOW, BUT THE TIME WINDOWS CONTAINED A LOT MORE ELEPHANTS THEN SO COV() WAS A LOT LESS DEPENDENT ON A SPECIFIC INDIVIDUAL. THESE ARE VERY GOOD APPROXIMATIONS OF THE WHOLE TIME WINDOW, BUT THEY'RE TERRIBLE FOR A SINGLE ELEPHANT ####
mean_eigen <- apply(cent_full, 2, mean)
covs_eigen <- list()

## plot full time window
par(mfrow = c(6,8),
    mai = c(0.1,0.1,0.1,0.1))
for(i in 1:n_years){
  centralities_per_year <- cent_full[,sim$year_id == i]
  if(length(centralities_per_year) > (n_chains*n_samples)){
    covs_eigen[[i]] <- cov(centralities_per_year)
  } else {
    covs_eigen[[i]] <- var(centralities_per_year)
  }
  plot(density(centralities_per_year),
       main = paste0('centralities ID 1, ',(1973:2021)[i]))
  lines(density(MASS::mvrnorm(1000,
                              mu = mean_eigen[sim$year_id == i],
                              Sigma = covs_eigen[[i]])),
        col = 'blue')
}

## plot only a single elephant
par(mfrow = c(6,8),
    mai = c(0.15,0.15,0.15,0.15))
for(i in 1:n_years){
  centralities_per_year <- cent_full[,sim$year_id == i]
  if(length(centralities_per_year) > (n_chains*n_samples)){
    covs_eigen[[i]] <- cov(centralities_per_year)
    plot(density(centralities_per_year[,1]),
         main = (1973:2021)[i],
         xlim = c(min(centralities_per_year[,1] - 1),
                  max(centralities_per_year[,1] + 1)))
  } else {
    covs_eigen[[i]] <- var(centralities_per_year)
    plot(density(centralities_per_year),
         main = paste0('only male in ',sort(unique(sim$year))[i]),
         xlim = c(min(centralities_per_year - 1),
                  max(centralities_per_year + 1)))
  }
  lines(density(MASS::mvrnorm(1000,
                              mu = mean_eigen[sim$year_id == i],
                              Sigma = covs_eigen[[i]])),
        col = 'blue')
}

## clean up
par(mfrow = c(1,1),
    mai = c(1,1,0.5,0.5))
rm(centralities_per_year, beta_node, beta_popn, beta_year, i, j, max_raw, mean_eigen, min_raw, n) ; gc()

#### create data list ####
elephants_per_year <- rep(NA, n_years)
for(i in 1:n_years){
  elephants_per_year[i] <- length(which(sim$year_id == i))
}

data_list <- list(
  ## global parameters ####
  n_data = nrow(sim),
  n_years = n_years,
  n_nodes = length(unique(sim$id)),
  ## explanatory variables ####
  node = sim$node_id,
  # age = sim$age,
  year = sim$year_std,
  population_size = sim$popn_std,
  ## number of elephants per year ####
  n_nodes1 = elephants_per_year[1],
  n_nodes2 = elephants_per_year[2],
  n_nodes3 = elephants_per_year[3],
  n_nodes4 = elephants_per_year[4],
  n_nodes5 = elephants_per_year[5],
  n_nodes6 = elephants_per_year[6],
  n_nodes7 = elephants_per_year[7],
  n_nodes8 = elephants_per_year[8],
  n_nodes9 = elephants_per_year[9],
  n_nodes10 = elephants_per_year[10],
  n_nodes11 = elephants_per_year[11],
  n_nodes12 = elephants_per_year[12],
  n_nodes13 = elephants_per_year[13],
  n_nodes14 = elephants_per_year[14],
  n_nodes15 = elephants_per_year[15],
  n_nodes16 = elephants_per_year[16],
  n_nodes17 = elephants_per_year[17],
  n_nodes18 = elephants_per_year[18],
  n_nodes19 = elephants_per_year[19],
  n_nodes20 = elephants_per_year[20],
  n_nodes21 = elephants_per_year[21],
  n_nodes22 = elephants_per_year[22],
  n_nodes23 = elephants_per_year[23],
  n_nodes24 = elephants_per_year[24],
  n_nodes25 = elephants_per_year[25],
  n_nodes26 = elephants_per_year[26],
  n_nodes27 = elephants_per_year[27],
  n_nodes28 = elephants_per_year[28],
  n_nodes29 = elephants_per_year[29],
  n_nodes30 = elephants_per_year[30],
  n_nodes31 = elephants_per_year[31],
  n_nodes32 = elephants_per_year[32],
  n_nodes33 = elephants_per_year[33],
  n_nodes34 = elephants_per_year[34],
  n_nodes35 = elephants_per_year[35],
  n_nodes36 = elephants_per_year[36],
  n_nodes37 = elephants_per_year[37],
  n_nodes38 = elephants_per_year[38],
  n_nodes39 = elephants_per_year[39],
  n_nodes40 = elephants_per_year[40],
  n_nodes41 = elephants_per_year[41],
  n_nodes42 = elephants_per_year[42],
  n_nodes43 = elephants_per_year[43],
  n_nodes44 = elephants_per_year[44],
  n_nodes45 = elephants_per_year[45],
  ## number of elephants in previous years ####
  prev_eles1 = 0,
  prev_eles2 = elephants_per_year[1],
  prev_eles3 = elephants_per_year[2],
  prev_eles4 = elephants_per_year[3],
  prev_eles5 = elephants_per_year[4],
  prev_eles6 = elephants_per_year[5],
  prev_eles7 = elephants_per_year[6],
  prev_eles8 = elephants_per_year[7],
  prev_eles9 = elephants_per_year[8],
  prev_eles10 = elephants_per_year[9],
  prev_eles11 = elephants_per_year[10],
  prev_eles12 = elephants_per_year[11],
  prev_eles13 = elephants_per_year[12],
  prev_eles14 = elephants_per_year[13],
  prev_eles15 = elephants_per_year[14],
  prev_eles16 = elephants_per_year[15],
  prev_eles17 = elephants_per_year[16],
  prev_eles18 = elephants_per_year[17],
  prev_eles19 = elephants_per_year[18],
  prev_eles20 = elephants_per_year[19],
  prev_eles21 = elephants_per_year[20],
  prev_eles22 = elephants_per_year[21],
  prev_eles23 = elephants_per_year[22],
  prev_eles24 = elephants_per_year[23],
  prev_eles25 = elephants_per_year[24],
  prev_eles26 = elephants_per_year[25],
  prev_eles27 = elephants_per_year[26],
  prev_eles28 = elephants_per_year[27],
  prev_eles29 = elephants_per_year[28],
  prev_eles30 = elephants_per_year[29],
  prev_eles31 = elephants_per_year[30],
  prev_eles32 = elephants_per_year[31],
  prev_eles33 = elephants_per_year[32],
  prev_eles34 = elephants_per_year[33],
  prev_eles35 = elephants_per_year[34],
  prev_eles36 = elephants_per_year[35],
  prev_eles37 = elephants_per_year[36],
  prev_eles38 = elephants_per_year[37],
  prev_eles39 = elephants_per_year[38],
  prev_eles40 = elephants_per_year[39],
  prev_eles41 = elephants_per_year[40],
  prev_eles42 = elephants_per_year[41],
  prev_eles43 = elephants_per_year[42],
  prev_eles44 = elephants_per_year[43],
  prev_eles45 = elephants_per_year[44],
  ## normal approximation of centrality ####
  # centrality_mean = sim$cent_mean_logit,
  # centrality_stdv = sim$cent_stdv  # changed to using a normal distribution instead of a multinormal, because then I can run all windows in the same model formula rather than having a separate for loop for 49 windows and very few data points per window. This does mean that I've removed the interdependencies between network measures, so it's not ideal... ask Dan if he has a better plan!
  centrality_mean1 = sim$cent_mean_logit[sim$year_id == 1],
  centrality_mean2 = sim$cent_mean_logit[sim$year_id == 2],
  centrality_mean3 = sim$cent_mean_logit[sim$year_id == 3],
  centrality_mean4 = sim$cent_mean_logit[sim$year_id == 4],
  centrality_mean5 = sim$cent_mean_logit[sim$year_id == 5],
  centrality_mean6 = sim$cent_mean_logit[sim$year_id == 6],
  centrality_mean7 = sim$cent_mean_logit[sim$year_id == 7],
  centrality_mean8 = sim$cent_mean_logit[sim$year_id == 8],
  centrality_mean9 = sim$cent_mean_logit[sim$year_id == 9],
  centrality_mean10 = sim$cent_mean_logit[sim$year_id == 10],
  centrality_mean11 = sim$cent_mean_logit[sim$year_id == 11],
  centrality_mean12 = sim$cent_mean_logit[sim$year_id == 12],
  centrality_mean13 = sim$cent_mean_logit[sim$year_id == 13],
  centrality_mean14 = sim$cent_mean_logit[sim$year_id == 14],
  centrality_mean15 = sim$cent_mean_logit[sim$year_id == 15],
  centrality_mean16 = sim$cent_mean_logit[sim$year_id == 16],
  centrality_mean17 = sim$cent_mean_logit[sim$year_id == 17],
  centrality_mean18 = sim$cent_mean_logit[sim$year_id == 18],
  centrality_mean19 = sim$cent_mean_logit[sim$year_id == 19],
  centrality_mean20 = sim$cent_mean_logit[sim$year_id == 20],
  centrality_mean21 = sim$cent_mean_logit[sim$year_id == 21],
  centrality_mean22 = sim$cent_mean_logit[sim$year_id == 22],
  centrality_mean23 = sim$cent_mean_logit[sim$year_id == 23],
  centrality_mean24 = sim$cent_mean_logit[sim$year_id == 24],
  centrality_mean25 = sim$cent_mean_logit[sim$year_id == 25],
  centrality_mean26 = sim$cent_mean_logit[sim$year_id == 26],
  centrality_mean27 = sim$cent_mean_logit[sim$year_id == 27],
  centrality_mean28 = sim$cent_mean_logit[sim$year_id == 28],
  centrality_mean29 = sim$cent_mean_logit[sim$year_id == 29],
  centrality_mean30 = sim$cent_mean_logit[sim$year_id == 30],
  centrality_mean31 = sim$cent_mean_logit[sim$year_id == 31],
  centrality_mean32 = sim$cent_mean_logit[sim$year_id == 32],
  centrality_mean33 = sim$cent_mean_logit[sim$year_id == 33],
  centrality_mean34 = sim$cent_mean_logit[sim$year_id == 34],
  centrality_mean35 = sim$cent_mean_logit[sim$year_id == 35],
  centrality_mean36 = sim$cent_mean_logit[sim$year_id == 36],
  centrality_mean37 = sim$cent_mean_logit[sim$year_id == 37],
  centrality_mean38 = sim$cent_mean_logit[sim$year_id == 38],
  centrality_mean39 = sim$cent_mean_logit[sim$year_id == 39],
  centrality_mean40 = sim$cent_mean_logit[sim$year_id == 40],
  centrality_mean41 = sim$cent_mean_logit[sim$year_id == 41],
  centrality_mean42 = sim$cent_mean_logit[sim$year_id == 42],
  centrality_mean43 = sim$cent_mean_logit[sim$year_id == 43],
  centrality_mean44 = sim$cent_mean_logit[sim$year_id == 44],
  centrality_mean45 = sim$cent_mean_logit[sim$year_id == 45],
  centrality_cov1 = covs_eigen[[1]],
  centrality_cov2 = as.matrix(covs_eigen[[2]]),
  centrality_cov3 = covs_eigen[[3]],
  centrality_cov4 = covs_eigen[[4]],
  centrality_cov5 = covs_eigen[[5]],
  centrality_cov6 = covs_eigen[[6]],
  centrality_cov7 = covs_eigen[[7]],
  centrality_cov8 = covs_eigen[[8]],
  centrality_cov9 = covs_eigen[[9]],
  centrality_cov10 = covs_eigen[[10]],
  centrality_cov11 = covs_eigen[[11]],
  centrality_cov12 = covs_eigen[[12]],
  centrality_cov13 = covs_eigen[[13]],
  centrality_cov14 = covs_eigen[[14]],
  centrality_cov15 = covs_eigen[[15]],
  centrality_cov16 = covs_eigen[[16]],
  centrality_cov17 = covs_eigen[[17]],
  centrality_cov18 = covs_eigen[[18]],
  centrality_cov19 = covs_eigen[[19]],
  centrality_cov20 = covs_eigen[[20]],
  centrality_cov21 = covs_eigen[[21]],
  centrality_cov22 = covs_eigen[[22]],
  centrality_cov23 = covs_eigen[[23]],
  centrality_cov24 = covs_eigen[[24]],
  centrality_cov25 = covs_eigen[[25]],
  centrality_cov26 = covs_eigen[[26]],
  centrality_cov27 = covs_eigen[[27]],
  centrality_cov28 = covs_eigen[[28]],
  centrality_cov29 = covs_eigen[[29]],
  centrality_cov30 = covs_eigen[[30]],
  centrality_cov31 = covs_eigen[[31]],
  centrality_cov32 = covs_eigen[[32]],
  centrality_cov33 = covs_eigen[[33]],
  centrality_cov34 = covs_eigen[[34]],
  centrality_cov35 = covs_eigen[[35]],
  centrality_cov36 = covs_eigen[[36]],
  centrality_cov37 = covs_eigen[[37]],
  centrality_cov38 = as.matrix(covs_eigen[[38]]),
  centrality_cov39 = covs_eigen[[39]],
  centrality_cov40 = as.matrix(covs_eigen[[40]]),
  centrality_cov41 = covs_eigen[[41]],
  centrality_cov42 = as.matrix(covs_eigen[[42]]),
  centrality_cov43 = covs_eigen[[43]],
  centrality_cov44 = as.matrix(covs_eigen[[44]]),
  centrality_cov45 = covs_eigen[[45]]
  )

#### fit model ####
## save workspace
## clean up and save workspace
save.image('step4_centralitytrends/model_ready.RData')
dev.off()
pdf('../outputs/simulation_centralities_modelchecks.pdf')
print('model ready to run')

## load models
eigen_regression_totaleffect <- cmdstan_model('models/eigen_regression_totaleffect.stan')
eigen_regression_external <- cmdstan_model('models/eigen_regression_external.stan')

# ## add standard deviation to data
# sim <- sim %>% 
#   mutate(cent_stdv = NA)
# for(i in 1:n_data){
#   window <- cents_selected[[sim$year[i] - 1972]]
#   elephant <- window[,sim$id[i]]
#   sim$cent_stdv[i] <- sd(elephant)
# }

## fit model
fit_external <- eigen_regression_external$sample(
  data = data_list, 
  chains = n_chains, parallel_chains = n_chains,
  iter_warmup = n_samples, iter_sampling = n_samples
)

#### check outputs ####
fit_external

## check fit
summary <- as.data.frame(fit_external$summary())
hist(summary$rhat)
hist(summary$ess_bulk)
hist(summary$ess_tail)

## extract posterior draws
posterior <- fit_external$draws(format = 'df')

## traceplots
parameters <- posterior %>% 
  dplyr::select(intercept, beta_year, beta_popn, sigma) %>% 
  pivot_longer(cols = everything(), names_to = 'parameter', values_to = 'draw') %>% 
  mutate(position = rep(rep(1:n_samples, each = 4),n_chains),
         chain = rep(1:n_chains, each = 4*n_samples))
node_effects <- posterior %>% 
  dplyr::select(`beta_node[1]`,`beta_node[2]`,`beta_node[3]`,`beta_node[4]`,`beta_node[5]`,
                `beta_node[6]`,`beta_node[7]`,`beta_node[8]`,`beta_node[9]`,`beta_node[10]`)

ggplot(parameters)+
  geom_line(aes(x = position, y = draw, colour = as.factor(chain)))+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none')
node_effects %>% 
  pivot_longer(cols = everything(), names_to = 'parameter', values_to = 'draw') %>% 
  mutate(position = rep(rep(1:n_samples, each = 10), n_chains),
         chain = rep(1:n_chains, each = 10*n_samples)) %>% 
  ggplot()+
  geom_line(aes(x = position, y = draw, colour = as.factor(chain)))+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none')

#### posterior predictive check ####
## define 
n_eles <- ncol(node_effects)
par(mfrow = c(4,ceiling(n_eles/4)))

## create posterior predictive check function
post_pred_check <- function(centrality_matrix, nodes_df, elephant, cent_cov, parameters){
  ## set up plot
  plot(density(centrality_matrix[1, which(nodes_df$node_id == elephant)]),
       las = 1, ylim = c(0,1.5),
       #main = paste0("Posterior check:\nblack = data, blue = predicted, window = ", time_window),
       main = paste0("Posterior check: window = ", time_window),
       col=rgb(0, 0, 0, 0.25))
  
  ## create data required
  eigen_data <- nodes_df %>%
    dplyr::select(node_random, age, window) %>%
    filter(window == time_window)
  num_nodes_window <- nrow(eigen_data)
  
  ## plot lines of predictions
  for (i in 1:100) {
    j <- sample(1:length(parameters$beta_age), 1)
    lines(density(centrality_matrix[j, which(nodes_df$window == time_window)]),
          col = rgb(0, 0, 0, 0.25))
    mu <- parameters$beta_age[j]*eigen_data$age + parameters$intercept[j]
    for(k in 1:length(mu)) {
      mu[k] <- mu[k] + as.numeric(rand_window[j,time_window]) + as.numeric(rand_node[j,eigen_data$node_random[k]])
    }
    sigma <- cent_cov + diag(rep(parameters$sigma[j], num_nodes_window))
    lines(density(MASS::mvrnorm(1, mu, sigma)), col=rgb(0, 0, 1, 0.25))
  }
}

# for(elephant in 1:n_eles){
#   ## create data required
#   ele_data <- sim %>% 
#     filter(node_id == elephant)
#   ele_years <- unique(ele_data$year_id)
#   
#   ## plot first year
#   plot(density(cent_full[, which(sim$node_id == elephant & 
#                                    sim$year_id == min(ele_data$year_id))]),
#        las = 1, #ylim = c(0,1.5),
#        main = paste0("PP check: elephant = ", sim$id[sim$node_id == elephant][1]),
#        col=rgb(0, 0, 0, 0.25))
#   mu <- posterior$intercept + posterior$beta_year*ele_data$year_id[1] + posterior$beta_popn*ele_data$popn_std[1] + node_effects[,elephant]
#   cov <- covs_eigen[[ele_data$year_id[1]]]
#   sigma <- array(NA, dim = c(nrow(cov), ncol(cov), length(mu)))
#   for(j in 1:length(mu)){
#     sigma[,,j] <- cov + diag(rep(posterior$sigma[j], dim(cov)[1]))
#   }
#   lines(density(MASS::mvrnorm(1, mu, sigma[,,1])), col=rgb(0, 0, 1, 0.25))
#   
#   ## plot later years
#   for (i in 1:length(ele_years)) {
#     lines(density(cent_full[, which(sim$node_id == elephant & 
#                                       sim$year_id == ele_years[i])]),
#           col = rgb(0, 0, 0, 0.25))
#     mu <- posterior$intercept + posterior$beta_year*ele_data$year_id[i] + posterior$beta_popn*ele_data$popn_std[i] + node_effects[,elephant]
#     cov <- covs_eigen[[ele_years[i]]]
#     sigma <- array(NA, dim = c(nrow(cov), ncol(cov), length(mu)))
#     for(j in 1:length(mu)){
#       sigma[,,j] <- cov + diag(rep(posterior$sigma[j], dim(cov)[1]))
#     }
#   }
# }

par(mfrow = c(ceiling(n_years/6), 6),
    mai = c(0.1,0.1,0.1,0.1))
for(year_plot in 1:n_years){
  ## identify which data points are relevant
  year_rows <- which(sim$year_id == year_plot)
  
  ## skip years which only have 1 elephant
  if(length(year_rows) > 1){
    ## plot
    plot(density(cent_full[1, year_rows]),
         las = 1, ylim = c(0,5), xlim = c(-5,5),
         main = '', #main = paste0("PP check: year = ", year_plot),
         xlab = '', ylab = '',
         col=rgb(0, 0, 0, 0.25))
    
    ## create data required
    year_data <- sim %>%
      filter(year_id == year_plot)
    num_nodes_year <- nrow(year_data)
    
    ## plot lines of predictions
    rows_to_plot <- sample(1:length(posterior$beta_year), 100, replace = F)
    for (j in rows_to_plot) {
      lines(density(cent_full[j, year_rows]),
            col = rgb(0, 0, 0, 0.25))
      mu <- as.matrix(posterior$intercept[j] + posterior$beta_year[j]*year_plot + posterior$beta_popn[j]*(sim$popn_std[year_rows][1]) + + node_effects[j,sim$node_id[year_rows]])
      colnames(mu) <- sim$node_year[year_rows]
      sigma <- covs_eigen[[year_plot]] + diag(rep(posterior$sigma[j], num_nodes_year))
      lines(density(MASS::mvrnorm(1, mu = mu, Sigma = sigma)),
            col=rgb(0, 0, 1, 0.25))
    }
  }
}
  
  




# for(time_window in 1:n_windows){
#   post_pred_check(centrality_matrix = cents_all,
#                   nodes_df = nodes_all, time_window = time_window,
#                   cent_cov = covs_all[[time_window]],
#                   parameters = params)
# }
# par(mfrow = c(1,1))
# 
# print('posterior predictive complete')
# 



#### predict from model ####
node_draws <- posterior %>% 
  select

pred_mu <- matrix(data = NA, nrow = n_chains*n_samples, ncol = n_data,
                  dimnames = list(1:(n_chains*n_samples),
                                  sim$node_year))
for(j in 1:ncol(pred_mu)){
  pred_mu[,j] <- as.vector(unlist(posterior$intercept + posterior$beta_year * sim$year_std[j] + posterior$beta_popn * sim$popn_std[j] + node_effects[,data_list$node[j]]))
}

pred_full <- pred_mu
for(i in 1:nrow(pred_full)){
  for(j in 1:ncol(pred_full)){
    pred_full[i,j] <- MASS::mvrnorm(1, mu = pred_mu[i,j], Sigma = posterior$sigma[i])
  }
}

save.image('step4_centralitytrends/simulation.RData')






#### extract original values ####
















# ## get mean predictions
# newdata <- nodes_all %>%
#   mutate(age_std = age_std+1)
# mu_predictions_newdata <- get_mean_predictions(predict_df = newdata, parameters = params,
#                                                include_window = TRUE, include_node = TRUE)
# nodes_all$mu_mean_plus1 <- apply(mu_predictions_newdata, 2, mean)
# 
# ## full distribution of predictions using age_std + 1 stdev
# full_predictions_newdata <- matrix(NA, nrow = nrow(params), ncol = nrow(newdata),
#                                    dimnames = list(NULL, nodes_all$node_window))
# for(time_window in 1:n_windows){
#   sigma_window <- sigma_list[[time_window]]
#   columns <- which(nodes_all$window == time_window)
#   for(i in 1:nrow(full_predictions_newdata)){
#     full_predictions_newdata[i,columns] <- MASS::mvrnorm(1,
#                                                          mu_predictions_newdata[i,columns],
#                                                          sigma_window[,,i])
#   }
# }
# 
# ## contrast predictions on standardised scale -- check that this returns the marginal effect presented in the summary
# contrast_std <- full_predictions_newdata - full_predictions    # contrast between predicted values for raw data and all same data but add 1 to age
# head(contrast_std[,1:5])                              # check matrix looks right
# mean(params$beta_age)                                 # output parameter direct from model
# mean(contrast_std)                                    # should be very similar to mean of output parameter
# quantile(contrast_std, prob = c(0.025, 0.975))        # very wide
# 
# ## contrast predictions on output scale -- reportable values of effect of plus 1 year
# contrast <- contrast_std / sd(nodes_all$age)          # convert to outcome scale
# head(contrast[,1:5])                                  # check matrix looks right
# mean(contrast)                                        # effect size on outcome scale
# sd(contrast)                                          # size of uncertainty on outcome scale
# quantile(contrast, prob = c(0.025, 0.975))            # very wide
# 
# ## save workspace
# save.image('anp_nodalregression/anp_short_nodal.RData')
# print('contrasts complete')
# 








#### final clean plots ####