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

### create file of output graphs
pdf('../outputs/centralities_modelprep.pdf', width = 20, height = 15)

#### calculate node centralities ####
## load data
load('step2_edgeweights/workspace_images/edgeweights_fitted_1973.RData')
df <- read_csv('../data_processed/step1_dataprocessing/males/selected_elephants.csv')

## filter down to only first year
eles1973 <- df %>% 
  filter(start_yr < 1974)
unique(eles1973$id) # M7 only
head(nodes) # 53 elephants in time window, only M7 to be included in final analysis

## create function for extracting eigenvector centrality
extract_eigen_centrality <- function(nodes_df, dyads_df, edgeweight_matrix, logit = TRUE){
  ## calculate data size parameters
  num_nodes <- nrow(nodes_df)
  num_dyads <- nrow(dyads_df)
  num_samples <- nrow(edgeweight_matrix)

  ## build adjacency tensor
  dyads_df$node_1_id <- as.integer(as.factor(dyads_df$node_1))
  dyads_df$node_2_id <- as.integer(as.factor(dyads_df$node_2))+1
  adj_tensor <- array(0, c(num_samples, num_nodes, num_nodes),
                      dimnames = list(NULL, nodes_df$id, nodes_df$id))

  ## fill adjacency tensor
  for (dyad_id in 1:num_dyads) {
    dyad_row <- dyads_df[dyad_id, ]
    adj_tensor[, dyad_row$node_1_id, dyad_row$node_2_id] <- edgeweight_matrix[, dyad_id]
    adj_tensor[, dyad_row$node_2_id, dyad_row$node_1_id] <- edgeweight_matrix[, dyad_id]
  }

  ## calculate centrality and store posterior samples in a matrix
  centrality_samples_invlogit <- matrix(0, num_samples, num_nodes,
                                        dimnames = list(NULL, nodes_df$id))
  for(i in 1:(num_samples)){
    centrality_samples_invlogit[i, ] <- sna::evcent(adj_tensor[i,,], gmode = 'graph')
  }

  ## convert to logit scale
  if(logit == TRUE) {
    centrality_samples <- logit(centrality_samples_invlogit)
    return(centrality_samples)
  } else {
    return(centrality_samples_invlogit)
  }
}

## extract centrality
eigen1 <- extract_eigen_centrality(nodes_df = nodes, dyads_df = yr1,
                                   edgeweight_matrix = as.matrix(edge_samples),
                                   logit = TRUE)

## save all outputs
saveRDS(eigen1, '../data_processed/step3_eigenvector/eigenvector_centralities_1973.RDS')
cents_all <- list()
cents_all[[1]] <- eigen1

## filter down to just M7
cents_1973 <- eigen1[,unique(eles1973$id)]
cents_selected <- list()
cents_selected[[1]] <- cents_1973

## extract mean and covariance
cent_means <- list()
cent_covs <- list()
if(length(unique(eles1973$id)) > 1){
  cent_means[[1]] <- apply(cents_1973, 2, mean)
  cent_covs[[1]] <- cov(cents_1973)
} else {
  cent_means[[1]] <- mean(cents_1973)
  cent_covs[[1]] <- var(cents_1973)
}

## add mean estimate per ID to dyads data frame
df$cent_mean <- NA
for(i in 1:length(unique(eles1973$id))){
  df$cent_mean[which(df$id == unique(eles1973$id)[i] & df$year == 1973)] <- cent_means[[1]][i]
}

rm(list = ls()[!ls() %in% c('df','cent_means','cent_covs','cents_all','cents_selected','extract_eigen_centrality')]) ; gc()

## for loop
for(time_window in 1974:2021){
  ## import workspace image
  load(paste0('step2_edgeweights/workspace_images/edgeweights_fitted_',time_window,'.RData'))
  
  eles_year <- df %>% 
    filter(start_yr < (time_window + 1)) %>% 
    filter(end_yr > (time_window + 1))
  
  ## extract centrality
  eigen <- extract_eigen_centrality(nodes_df = nodes, dyads_df = yr,
                                    edgeweight_matrix = as.matrix(edge_samples),
                                    logit = TRUE)
  
  ## save all outputs
  saveRDS(eigen, paste0('../data_processed/step3_eigenvector/eigenvector_centralities_',time_window,'.RDS'))
  cents_all[[time_window-1972]] <- eigen
  
  ## filter down to just those selected elephants
  cents_year <- eigen[,unique(eles_year$id)]
  cents_selected[[time_window-1972]] <- cents_year
  
  ## extract mean and covariance
  if(length(unique(eles_year$id)) > 1){
    cent_means[[time_window-1972]] <- apply(cents_year, 2, mean)
    cent_covs[[time_window-1972]] <- cov(cents_year)
  } else {
    cent_means[[time_window-1972]] <- mean(cents_year)
    cent_covs[[time_window-1972]] <- var(cents_year)
  }
  
  ## add mean estimate per ID to dyads data frame
  for(i in 1:length(unique(eles_year$id))){
    df$cent_mean[which(df$id == unique(eles_year$id)[i] & df$year == time_window)] <- cent_means[[time_window-1972]][i]
  }
  ## add progress marker
  print(time_window)
  rm(list = ls()[!ls() %in% c('df','cent_means','cent_covs','cents_all','cents_selected','extract_eigen_centrality','time_window')]) ; gc()
}

## clean up and save workspace
save.image('step4_centralitytrends/centralities_calculated.RData')
print('centralities calculated')

#### plot selected centralities against age, year and population size ####
## plot all data points
df2 <- df %>% 
  #filter(cent_mean != -Inf) %>% 
  filter( !is.na(cent_mean) )

df2 %>% 
  ggplot(aes(x = age, y = cent_mean, colour = id))+
  geom_point()+
  geom_line()+
  scale_colour_viridis_d()+
  theme_bw()
df2 %>% 
  ggplot(aes(x = year, y = cent_mean, colour = id))+
  geom_point()+
  geom_line()+
  scale_colour_viridis_d()+
  theme_bw()

## cut down to only half so not reusing sightings of same elephants within multiple data points
df3 <- df2 %>% 
  filter((start_yr %% 2 == 0 & year %% 2 == 0 |
            start_yr %% 2 != 0 & year %% 2 != 0))

df3 %>% 
  ggplot(aes(x = age, y = cent_mean, colour = id))+
  geom_point()+
  geom_line()+
  scale_colour_viridis_d()+
  theme_bw()
df3 %>% 
  ggplot(aes(x = year, y = cent_mean, colour = id))+
  geom_point()+
  geom_line()+
  scale_colour_viridis_d()+
  theme_bw()

## plot number of elephants in population vs centrality
n_years <- 2021-1972
eles_per_year <- rep(NA, n_years)
df3$population_size <- NA
for(year_id in 1:n_years){
  eles_per_year[year_id] <- ncol(cents_all[[year_id]])
  df3$population_size[df3$year == (1973:2021)[year_id]] <- ncol(cents_all[[year_id]])
}
df3 %>% 
  ggplot(aes(x = population_size, y = cent_mean,
             colour = id))+
  geom_point()+
  geom_smooth(se = F, method = 'lm')+
  scale_colour_viridis_d()+
  theme_bw()
df3 %>% 
  ggplot(aes(x = year, y = population_size))+
  geom_point()+
  geom_smooth()+
  theme_bw()

## plot against ID
df3 %>% 
  ggplot(aes(x = id, y = cent_mean))+
  geom_boxplot(notch = T)+
  theme_bw()

#### prior predictive check -- total effect of year / personality ####
df3$year_std <- (df3$year - mean(df3$year)) / sd(df3$year)
df3$year_id <- df3$year - 1973

n <- 100
beta_node <- rnorm(n, 0, 0.8)
beta_year <- rnorm(n, 0, 0.8)
min_raw <- min(df3$cent_mean)
max_raw <- max(df3$cent_mean)
intercept <- rnorm(n, LaplacesDemon::logit(0.05), 0.8) # taking the intercept from the logit of results from Chiyo 2011 (doesn't state mean/median centrality so estimated from graph based on where correlation line would cross x = 0)
plot(NULL, las = 1, xlab = 'year', ylab = 'eigenvector',
     ylim = c(min_raw-3, max_raw+3),
     xlim = c(min(df3$year_std), max(df3$year_std)))
abline(h = min_raw, lty = 2) ; abline(h = max_raw, lty = 2)
for(i in 1:n){
  lines(x = seq(min(df3$year_std), max(df3$year_std), length.out = 2),
        y = intercept[i] + 
          beta_year[i]*c(min(df3$year_std), max(df3$year_std)) + beta_node[i],
        col = rgb(0,0,1,0.4))
}

#### prior predictive check -- year after controlling for population size ####
df3$popn_std <- (df3$population_size - mean(df3$population_size)) / sd(df3$population_size)

beta_node <- rnorm(n, 0, 0.6)
beta_year <- rnorm(n, 0, 0.6)
beta_popn <- rnorm(n, 0, 0.6)

plot(NULL, las = 1, xlab = 'year', ylab = 'eigenvector',
     ylim = c(min_raw-3, max_raw+3),
     xlim = c(min(df3$year_std), max(df3$year_std)))
abline(h = min_raw, lty = 2) ; abline(h = max_raw, lty = 2)
for(i in 1:n){
  lines(x = seq(min(df3$year_std), max(df3$year_std), length.out = 2),
        y = intercept[i] + 
          beta_year[i]*c(min(df3$year_std), max(df3$year_std)) + 
          beta_popn[i]*c(min(df3$popn_std), max(df3$popn_std)) + 
          beta_node[i],
        col = rgb(0,0,1,0.4))
}

#### normal approximation ####
# mean_eigen <- unlist(cent_means[[1]]) %>% 
#   as.data.frame()
# colnames(mean_eigen) <- 'mean_eigen'
# mean_eigen$id <- rownames(mean_eigen)
# mean_eigen$year <- 1973
# 
# for(year_id in 2:n_years){
#   year_eigen <- unlist(cent_means[[year_id]]) %>% 
#     as.data.frame()
#   colnames(year_eigen) <- 'mean_eigen'
#   year_eigen$id <- rownames(year_eigen)
#   year_eigen$year <- (1973:2021)[year_id]
#   mean_eigen <- rbind(mean_eigen, year_eigen)
# }
# 
# covs_eigen <- unlist(cent_covs[[2]]) %>% 
#   as.data.frame()
# covs_eigen$year <- 1973
# 
# for(year_id in 2:n_years){
#   year_eigen <- unlist(cent_means[[year_id]]) %>% 
#     as.data.frame()
#   colnames(year_eigen) <- 'mean_eigen'
#   year_eigen$id <- rownames(year_eigen)
#   year_eigen$year <- (1973:2021)[year_id]
#   mean_eigen <- rbind(mean_eigen, year_eigen)
# }
mean_eigen <- unlist(cent_means[[2]]) %>% 
  as.data.frame()
colnames(mean_eigen) <- 'mean_eigen'
mean_eigen$id <- rownames(mean_eigen)
mean_eigen$year <- 1973

par(mfrow = c(7,7),
    mai = c(0.1,0.1,0.1,0.1))
for(i in 2:47){
  plot(density(cents_selected[[i]]),
       main = paste0('centralities ID 1, ',(1973:2021)[i]))
  lines(density(MASS::mvrnorm(1000,
                              mu = cent_means[[i]],
                              Sigma = cent_covs[[i]])),
        col = 'blue')
}
par(mfrow = c(1,1),
    mai = c(1,1,1,1))

#### run model ####
## load models
eigen_regression_totaleffect <- cmdstan_model('models/eigen_regression_totaleffect.stan')
eigen_regression_external <- cmdstan_model('models/eigen_regression_external.stan')

## create node integer values
n_data <- nrow(df3)
df3 <- df3 %>% 
  mutate(node = sample(x = 1:n_data, size = n_data, replace = F),
         cent_stdv = NA)
for(i in 1:n_data){
  window <- cents_selected[[df3$year[i] - 1972]]
  elephant <- window[,df3$id[i]]
  df3$cent_stdv[i] <- sd(elephant)
}

## create data list
data_list <- list(n_data = n_data,
                  n_years = length(unique(df3$year)),
                  n_nodes = length(unique(df3$id)),
                  node = df3$node,
                  #age = df3$age,
                  year = df3$year_std,
                  population_size = df3$popn_std,
                  centrality_mean = df3$cent_mean,
                  centrality_stdv = df3$cent_stdv) # changed to using a normal distribution instead of a multinormal, because then I can run all windows in the same model formula rather than having a separate for loop for 49 windows and very few data points per window. This does mean that I've removed the interdependencies between network measures, so it's not ideal... ask Dan if he has a better plan!

## save workspace
## clean up and save workspace
save.image('step4_centralitytrends/model_ready.RData')
dev.off()
pdf('../outputs/centralities_modelchecks.pdf')
print('model ready to run')

## create parameters
n_chains <- 4
n_samples <- 1000

## fit model
fit_external <- eigen_regression_external$sample(
  data = data_list, 
  chains = n_chains, parallel_chains = n_chains,
  iter_warmup = n_samples, iter_sampling = n_samples
)




