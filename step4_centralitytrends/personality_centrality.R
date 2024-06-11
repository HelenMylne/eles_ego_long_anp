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
#set_cmdstan_path('/Users/helen/.cmdstan/cmdstan-2.32.1')
#set_cmdstan_path('../../packages/.cmdstan/cmdstan-2.31.0')

### set seed
set.seed(12345)

### create file of output graphs
pdf('../outputs/centralities.pdf', width = 20, height = 15)

#### calculate node centralities ####
## load data
load('step2_edgeweights/workspace_images/edgeweights_fitted_1973.RData')
df <- read_csv('../data_processed/step1_dataprocessing/selected_elephants.csv')

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

## for loop
for(time_window in c(1974:1980,1982:1984,1988,1991:1998)){ #1974:2021){
  ## import workspace image
  load(paste0('step2_edgeweights/workspace_images/edgeweights_fitted_',time_window,'.RData'))
  
  eles_year <- df %>% 
    filter(start_yr < (time_window + 1))
  
  ## extract centrality
  eigen <- extract_eigen_centrality(nodes_df = nodes, dyads_df = yr1,
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
    cent_means[[time_window]] <- apply(cents_year, 2, mean)
    cent_covs[[time_window]] <- cov(cents_year)
  } else {
    cent_means[[time_window]] <- mean(cents_year)
    cent_covs[[time_window]] <- var(cents_year)
  }
  
  ## add mean estimate per ID to dyads data frame
  for(i in 1:length(unique(eles_year$id))){
    df$cent_mean[which(df$id == unique(eles_year$id)[i] & df$year == time_window)] <- cent_means[[time_window]][i]
  }
  ## add progress marker
  print(time_window)
}

## clean up and save workspace
rm(list = ls()[ ! ls() %in% c('cent_covs','cent_means','cents_all','cents_selected','df')]) ; gc()
save.image('step4_centralitytrends/centralities_calculated.RData')
print('centralities calculated')

#### plot selected centralities against age ####
df2 <- df %>% 
  #filter(cent_mean != -Inf) %>% 
  filter( !is.na(cent_mean) )

df2 %>% 
  ggplot(aes(x = age, y = cent_mean, colour = id))+
  geom_point()+
  geom_line()+
  scale_colour_viridis_d()+
  theme_bw()

#### run model ####
## load model
eigen_regression <- cmdstan_model('models/eigen_regression.stan')

## create data list
data_list <- list(n_data = nrow(df2),
                  n_years = length(unique(df2$year)),
                  n_nodes = length(unique(df2$id)),
                  node = df2$id,
                  age = df2$age,
                  year = df2$year,
                  centrality_mu = df2$cent_mean,
                  centrality_cov = )







