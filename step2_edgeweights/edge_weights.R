#### information ####
# script to extract edge weights for the ANP population per year for egocentric networks for 10 elephants

#### set up ####
### load packages
# library(tidyverse) ; library(dplyr) ; library(cmdstanr) ; library(igraph) ; library(janitor) ; library(lubridate) ; library(hms)
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
pdf('../outputs/edge_weights_fullpopn.pdf', width = 20, height = 15)

### set up weight model
edge_binary <- cmdstan_model("models/edge_binary_basic.stan")   # load model
edge_binary                                                     # check model priors etc.
n_chains <- 4                                                   # number of MCMC chains to run
n_samples <- 1000                                               # number of samples per chain

#### import data ####
start_years <- 1973:2021
yr1 <- readRDS('../data_processed/step1_dataprocessing/aggregated_dyad_pairs/dyad_pairs_aggregated_1973.RDS')

#### create nodes data frame ####
nodes <- yr1 %>% 
  select(id_1, node_1, byr_1, sightings_1) %>% 
  rename(id = id_1, node = node_1, byr = byr_1, sightings = sightings_1) %>% 
  distinct()
id1 <- unique(yr1$id_1)
id2 <- yr1 %>% 
  filter(! id_2 %in% id1) %>% 
  ungroup() %>% 
  select(id_2, node_2, byr_2, sightings_2) %>% 
  rename(id = id_2, node = node_2, byr = byr_2, sightings = sightings_2) %>% 
  distinct()
nodes <- rbind(nodes, id2)
rm(id1, id2) ; gc()

nodes$age <- 1973 - nodes$byr

#### create data list ####
n_dyads <- nrow(yr1)                   # number of dyads in time window
counts_ls <- list(                     # create data list
  n_dyads    = n_dyads,                # total number of times one or other of the dyad was observed
  dyad_ids   = yr1$dyad_id,            # identifier for each dyad
  together   = yr1$event_count,        # count number of sightings seen together
  count_dyad = yr1$total_sightings     # count total number of times seen
)

#### fit model ####
## fit
fit_edges <- edge_binary$sample(
  data = counts_ls, 
  chains = n_chains, parallel_chains = n_chains,
  iter_warmup = n_samples, iter_sampling = n_samples)

## check model
fit_edges

## save progress
save.image('step2_edgeweights/workspace_images/edgeweights_fitted_1973.RData')

#### extract posterior ####
## extract
edge_samples <- fit_edges$draws("edge_weight", format = "df") %>% 
  select(-`.chain`, -`.iteration`,-`.draw`)

## convert to long format
yr1$dyad_rank <- as.integer(as.factor(yr1$dyad_id))
edges <- pivot_longer(edge_samples, cols = everything(),
                      values_to = 'edge_draw', names_to = 'parameter' ) %>% 
  mutate(dyad_rank = rep(1:n_dyads, (n_samples*n_chains))) %>% ## NEED TO WORK OUT WHY, DESPITE USING THE SAME CODES, THIS WORKS HERE BUT NOT FOR ANP CHAPTER 1 -- IN THAT I USED DYAD_RANK = AS.INTER(AS.FACTOR(PARAMETER)), BUT HERE THAT ENTIRELY RANDOMISES MY WHOLE OUTPUT
  left_join(yr1[,c('dyad_id','dyad_rank')], by = 'dyad_rank') %>% 
  mutate(chain = rep(1:n_chains, each = n_samples*n_dyads),
         position = rep(rep(1:n_samples, each = n_dyads), n_chains))

## save edges
saveRDS(edges, '../data_processed/step2_edgeweights/yr1973_edgedistributions.RDS')

#### check posterior ####
## assign random set of columns to check
if(length(which(yr1$event_count >= 1)) >= 200){ n_test <- 200 } else { n_test <- length(which(yr1$event_count >= 1)) } # set number of dyads to sample
plot_dyads <- c(sample(yr1$dyad_id[yr1$event_count >= 1], # sample dyads seen together
                       size = n_test, replace = F),
                sample(yr1$dyad_id[yr1$event_count == 0],
                       size = n_test, replace = F))       # sample never  together
plot_edges <- edges[edges$dyad_id %in% plot_dyads,]       # select edges for sampled dyads
plot_edges$seen_together <- NA ; for(i in 1:length(plot_dyads)){    # set up for loop
  plot_edges$seen_together[plot_edges$dyad_id == plot_dyads[i]] <- ifelse(yr1$event_count[yr1$dyad_id == plot_dyads[i]] > 0, 1, 0) # the value of seen_together is 1 if the dyad has ever been seen in the same group, and 0 if they have not
}

### build traceplots
ggplot(data = plot_edges[plot_edges$seen_together == 1,],   # plot only dyads that have been seen together
       aes(y = edge_draw, x = position, colour = chain))+   # plot all chains over each other for each dyad to check mixing
  geom_line()+                                              # draw as line plot
  facet_wrap(. ~ dyad_id)+                                     # new panel per dyad as each pair has own edge weight parameter
  theme_classic()+                                          # make it look nicer
  theme(legend.position = 'none',                           # remove legend
        strip.background = element_blank(), strip.text = element_blank())    # remove facet strips
ggplot(data = plot_edges[plot_edges$seen_together == 0,],   # repeat for dyads that have never been seen together
       aes(y = edge_draw, x = position, colour = chain))+
  geom_line()+
  facet_wrap(. ~ dyad_id)+
  theme_classic() + theme(legend.position = 'none', strip.background = element_blank(), strip.text = element_blank())

### density plots
plot(NULL, xlim = c(0,1), ylim = c(0,30), las = 1, xlab = 'edge weight', ylab = 'density')         # set up plot window
for(i in 1:length(plot_dyads)){                                                                    # plot randomly sampled dyads
  x <- plot_edges[plot_edges$dyad_id == plot_dyads[i],]                                            # select data to plot
  lines(density(x$edge_draw), col = ifelse(x$seen_together == 1, rgb(0,0,1,0.1), rgb(1,0,0,0.1)))  # draw edge weight probability plot. blue = seen together at least once, red = never seen together
}

#### plot network ####
### create custom network plotting function
plot_network_threshold <- function (edge_samples, dyad_data, lwd = 2, threshold = 0.3,
                                    label.colour = 'transparent', label.font = 'Helvetica', 
                                    node.size = 4, node.colour = 'seagreen1',
                                    link.colour1 = 'black', link.colour2 = rgb(0, 0, 0, 0.3))
{
  dyad_name <- do.call(paste, c(dyad_data[c("node_1", "node_2")], sep=" <-> "))
  edge_lower <- apply(edge_samples, 2, function(x) quantile(x, probs=0.025))
  edge_upper <- apply(edge_samples, 2, function(x) quantile(x, probs=0.975))
  edge_median <- apply(edge_samples, 2, function(x) quantile(x, probs=0.5))
  edge_list <- cbind(
    "median"=round(edge_median, 3), 
    "2.5%"=round(edge_lower, 3), 
    "97.5%"=round(edge_upper, 3)
  )
  rownames(edge_list) <- dyad_name
  edgelist <- as.data.frame(edge_list)
  edgelist$node_1 <- as.character(dyad_data$node_1)
  edgelist$node_2 <- as.character(dyad_data$node_2)
  edgelist <- edgelist[,c(4:5,1:3)]
  #net_all <- igraph::graph_from_edgelist(as.matrix(edgelist[, 1:2]), directed = F)
  threshold_edges <- edgelist[edgelist$median >= threshold,]
  if(nrow(threshold_edges) == 0) { stop('No edges above threshold') }
  net <- igraph::graph_from_edgelist(as.matrix(threshold_edges[, 1:2]), directed = F)
  
  if(is.data.frame(node.size) == TRUE ) {
    nodes_list <- data.frame(node = rep(NA, length(unique(c(threshold_edges$node_1, threshold_edges$node_2)))), #as.numeric(names(net_all[[1]])),
                             sightings = NA)
    for(i in 1:nrow(nodes_list)){
      nodes_all <- rep(NA, 2*nrow(threshold_edges))  
      for(a in 1:2){
        for(b in 1:nrow(threshold_edges)){
          nodes_all[a + (b-1)*2] <- threshold_edges[b,a]
        }
      }
      nodes_list$node <- unique(nodes_all)
      nodes_list$sightings[i] <- nodes$sightings[which(nodes$node == nodes_list$node[i])]
    }
    node_sightings <- nodes_list$sightings #log(nodes_list$sightings)*5
  } else { node_sightings <- node.size }
  
  if(is.data.frame(node.colour) == TRUE ) {
    nodes_list <- data.frame(node = rep(NA, length(unique(c(threshold_edges$node_1, threshold_edges$node_2)))), #as.numeric(names(net_all[[1]])),
                             age = NA)
    for(i in 1:nrow(nodes_list)){
      nodes_all <- rep(NA, 2*nrow(threshold_edges))  
      for(a in 1:2){
        for(b in 1:nrow(threshold_edges)){
          nodes_all[a + (b-1)*2] <- threshold_edges[b,a]
        }
      }
      nodes_list$node <- unique(nodes_all)
      nodes_list$age[i] <- nodes$age[which(nodes$node == nodes_list$node[i])]
    }
    node_age <- nodes_list$age
  } else { node_age <- node.colour }
  
  md <- threshold_edges[, 3]
  ub <- threshold_edges[, 5]
  coords <- igraph::layout_nicely(net)
  igraph::plot.igraph(net, layout = coords,
                      vertex.label.color = ifelse(is.null(label.colour) == TRUE,
                                                  ifelse(node_age < 20, 'black', 'white'),
                                                  label.colour),
                      label.family = label.font,
                      vertex.color = ifelse(node_age < 15, '#FDE725FF',
                                            ifelse(node_age < 20, '#55C667FF',
                                                   ifelse(node_age < 30, '#1F968BFF', 
                                                          ifelse(node_age < 40, '#39568CFF', '#440154FF')))), 
                      vertex.size = log(node_sightings)*5,
                      frame.color = NA,
                      frame.width = 0,
                      edge.color = NA,
                      edge.arrow.size = 0,
                      edge.width = 0)
  igraph::plot.igraph(net, layout = coords, add = TRUE,
                      vertex.label = NA,
                      vertex.color = 'transparent',
                      vertex.size = 0, 
                      frame.color = NA,
                      frame.width = 0,
                      edge.color = link.colour1,
                      edge.arrow.size = 0,
                      edge.width = md * lwd)
  igraph::plot.igraph(net, layout = coords, add = TRUE,
                      vertex.label = NA,
                      vertex.color = 'transparent',
                      vertex.size = 0, 
                      frame.color = NA,
                      frame.width = 0,
                      edge.color = link.colour2,
                      edge.arrow.size = 0,
                      edge.width = ub * lwd)
}

### create single matrix of edge samples
colnames(edge_samples) <- yr1$dyad_id                                        # match to dyad ID numbers

### plot network across 6 different threshold values for comparison to other networks
par(mai = c(0.1,0.1,0.1,0.1))
plot_network_threshold(edge_samples = edge_samples,
                       dyad_data = yr1, threshold = 0.05,
                       node.size = nodes, node.colour = nodes,
                       lwd = 15)
plot_network_threshold(edge_samples = edge_samples,
                       dyad_data = yr1, threshold = 0.10,
                       node.size = nodes, node.colour = nodes,
                       lwd = 15)
plot_network_threshold(edge_samples = edge_samples,
                       dyad_data = yr1, threshold = 0.15,
                       node.size = nodes, node.colour = nodes,
                       lwd = 15)
plot_network_threshold(edge_samples = edge_samples,
                       dyad_data = yr1, threshold = 0.20,
                       node.size = nodes, node.colour = nodes,
                       lwd = 15)
plot_network_threshold(edge_samples = edge_samples,
                       dyad_data = yr1, threshold = 0.25,
                       node.size = nodes, node.colour = nodes,
                       lwd = 15)
plot_network_threshold(edge_samples = edge_samples,
                       dyad_data = yr1, threshold = 0.30,
                       node.size = nodes, node.colour = nodes,
                       lwd = 15)

### clean workspace
rm(counts_ls, x, i, j) ; gc()

### save image
save.image('step2_edgeweights/workspace_images/edgeweights_fitted_1973.RData')

########################### CONVERT TO LOOP ###########################
#### set up loop ####
## create vector to loop through
start_years <- 1973:2021

## create custom network plotting function
plot_network_threshold <- function (edge_samples, dyad_data, lwd = 2, threshold = 0.3,
                                    label.colour = 'transparent', label.font = 'Helvetica', 
                                    node.size = 4, node.colour = 'seagreen1',
                                    link.colour1 = 'black', link.colour2 = rgb(0, 0, 0, 0.3))
{
  dyad_name <- do.call(paste, c(dyad_data[c("node_1", "node_2")], sep=" <-> "))
  edge_lower <- apply(edge_samples, 2, function(x) quantile(x, probs=0.025))
  edge_upper <- apply(edge_samples, 2, function(x) quantile(x, probs=0.975))
  edge_median <- apply(edge_samples, 2, function(x) quantile(x, probs=0.5))
  edge_list <- cbind(
    "median"=round(edge_median, 3), 
    "2.5%"=round(edge_lower, 3), 
    "97.5%"=round(edge_upper, 3)
  )
  rownames(edge_list) <- dyad_name
  edgelist <- as.data.frame(edge_list)
  edgelist$node_1 <- as.character(dyad_data$node_1)
  edgelist$node_2 <- as.character(dyad_data$node_2)
  edgelist <- edgelist[,c(4:5,1:3)]
  #net_all <- igraph::graph_from_edgelist(as.matrix(edgelist[, 1:2]), directed = F)
  threshold_edges <- edgelist[edgelist$median >= threshold,]
  if(nrow(threshold_edges) == 0) { stop('No edges above threshold') }
  net <- igraph::graph_from_edgelist(as.matrix(threshold_edges[, 1:2]), directed = F)
  
  if(is.data.frame(node.size) == TRUE ) {
    nodes_list <- data.frame(node = rep(NA, length(unique(c(threshold_edges$node_1, threshold_edges$node_2)))), #as.numeric(names(net_all[[1]])),
                             sightings = NA)
    for(i in 1:nrow(nodes_list)){
      nodes_all <- rep(NA, 2*nrow(threshold_edges))  
      for(a in 1:2){
        for(b in 1:nrow(threshold_edges)){
          nodes_all[a + (b-1)*2] <- threshold_edges[b,a]
        }
      }
      nodes_list$node <- unique(nodes_all)
      nodes_list$sightings[i] <- nodes$sightings[which(nodes$node == nodes_list$node[i])]
    }
    node_sightings <- nodes_list$sightings #log(nodes_list$sightings)*5
  } else { node_sightings <- node.size }
  
  if(is.data.frame(node.colour) == TRUE ) {
    nodes_list <- data.frame(node = rep(NA, length(unique(c(threshold_edges$node_1, threshold_edges$node_2)))), #as.numeric(names(net_all[[1]])),
                             age = NA)
    for(i in 1:nrow(nodes_list)){
      nodes_all <- rep(NA, 2*nrow(threshold_edges))  
      for(a in 1:2){
        for(b in 1:nrow(threshold_edges)){
          nodes_all[a + (b-1)*2] <- threshold_edges[b,a]
        }
      }
      nodes_list$node <- unique(nodes_all)
      nodes_list$age[i] <- nodes$age[which(nodes$node == nodes_list$node[i])]
    }
    node_age <- nodes_list$age
  } else { node_age <- node.colour }
  
  md <- threshold_edges[, 3]
  ub <- threshold_edges[, 5]
  coords <- igraph::layout_nicely(net)
  igraph::plot.igraph(net, layout = coords,
                      vertex.label.color = ifelse(is.null(label.colour) == TRUE,
                                                  ifelse(node_age < 20, 'black', 'white'),
                                                  label.colour),
                      label.family = label.font,
                      vertex.color = ifelse(node_age < 15, '#FDE725FF',
                                            ifelse(node_age < 20, '#55C667FF',
                                                   ifelse(node_age < 30, '#1F968BFF', 
                                                          ifelse(node_age < 40, '#39568CFF', '#440154FF')))), 
                      vertex.size = log(node_sightings)*5,
                      frame.color = NA,
                      frame.width = 0,
                      edge.color = NA,
                      edge.arrow.size = 0,
                      edge.width = 0)
  igraph::plot.igraph(net, layout = coords, add = TRUE,
                      vertex.label = NA,
                      vertex.color = 'transparent',
                      vertex.size = 0, 
                      frame.color = NA,
                      frame.width = 0,
                      edge.color = link.colour1,
                      edge.arrow.size = 0,
                      edge.width = md * lwd)
  igraph::plot.igraph(net, layout = coords, add = TRUE,
                      vertex.label = NA,
                      vertex.color = 'transparent',
                      vertex.size = 0, 
                      frame.color = NA,
                      frame.width = 0,
                      edge.color = link.colour2,
                      edge.arrow.size = 0,
                      edge.width = ub * lwd)
}

#### create loop ####
for( year_id in 1:length(start_years)){
  #### import data ####
  yr <- readRDS(paste0('../data_processed/step1_dataprocessing/aggregated_dyad_pairs/dyad_pairs_aggregated_',start_years[year_id],'.RDS'))
  
  #### create nodes data frame ####
  nodes <- yr %>% 
    select(id_1, node_1, byr_1, sightings_1) %>% 
    rename(id = id_1, node = node_1, byr = byr_1, sightings = sightings_1) %>% 
    distinct()
  id1 <- unique(yr$id_1)
  id2 <- yr %>% 
    filter(! id_2 %in% id1) %>% 
    ungroup() %>% 
    select(id_2, node_2, byr_2, sightings_2) %>% 
    rename(id = id_2, node = node_2, byr = byr_2, sightings = sightings_2) %>% 
    distinct()
  nodes <- rbind(nodes, id2)
  rm(id1, id2) ; gc()
  
  nodes$age <- yr$start_yr[1] - nodes$byr
  
  #### create data list ####
  n_dyads <- nrow(yr)                   # number of dyads in time window
  counts_ls <- list(                     # create data list
    n_dyads    = n_dyads,                # total number of times one or other of the dyad was observed
    dyad_ids   = yr$dyad_id,            # identifier for each dyad
    together   = yr$event_count,        # count number of sightings seen together
    count_dyad = yr$total_sightings     # count total number of times seen
  )
  
  #### fit model ####
  ## fit
  fit_edges <- edge_binary$sample(
    data = counts_ls, 
    chains = n_chains, parallel_chains = n_chains,
    iter_warmup = n_samples, iter_sampling = n_samples)
  
  ## check model
  fit_edges
  
  ## save progress
  save.image(paste0('step2_edgeweights/workspace_images/edgeweights_fitted_',start_years[year_id],'.RData'))
  
  #### extract posterior ####
  ## extract
  edge_samples <- fit_edges$draws("edge_weight", format = "df") %>% 
    select(-`.chain`, -`.iteration`,-`.draw`)
  
  ## convert to long format
  yr$dyad_rank <- as.integer(as.factor(yr$dyad_id))
  edges <- pivot_longer(edge_samples, cols = everything(),
                        values_to = 'edge_draw', names_to = 'parameter' ) %>% 
    mutate(dyad_rank = rep(1:n_dyads, (n_samples*n_chains))) %>% ## NEED TO WORK OUT WHY, DESPITE USING THE SAME CODES, THIS WORKS HERE BUT NOT FOR ANP CHAPTER 1 -- IN THAT I USED DYAD_RANK = AS.INTER(AS.FACTOR(PARAMETER)), BUT HERE THAT ENTIRELY RANDOMISES MY WHOLE OUTPUT
    left_join(yr[,c('dyad_id','dyad_rank')], by = 'dyad_rank') %>% 
    mutate(chain = rep(1:n_chains, each = n_samples*n_dyads),
           position = rep(rep(1:n_samples, each = n_dyads), n_chains))
  
  ## save edges
  saveRDS(edges, paste0('../data_processed/step2_edgeweights/yr',start_years[year_id],'_edgedistributions.RDS'))
  
  #### check posterior ####
  ## assign random set of columns to check
  if(length(which(yr$event_count >= 1)) >= 200){ n_test <- 200 } else { n_test <- length(which(yr$event_count >= 1)) } # set number of dyads to sample
  plot_dyads <- c(sample(yr$dyad_id[yr$event_count >= 1], # sample dyads seen together
                         size = n_test, replace = F),
                  sample(yr$dyad_id[yr$event_count == 0],
                         size = n_test, replace = F))       # sample never  together
  plot_edges <- edges[edges$dyad_id %in% plot_dyads,]       # select edges for sampled dyads
  plot_edges$seen_together <- NA ; for(i in 1:length(plot_dyads)){    # set up for loop
    plot_edges$seen_together[plot_edges$dyad_id == plot_dyads[i]] <- ifelse(yr$event_count[yr$dyad_id == plot_dyads[i]] > 0, 1, 0) # the value of seen_together is 1 if the dyad has ever been seen in the same group, and 0 if they have not
  }
  
  ### build traceplots
  ggplot(data = plot_edges[plot_edges$seen_together == 1,],   # plot only dyads that have been seen together
         aes(y = edge_draw, x = position, colour = chain))+   # plot all chains over each other for each dyad to check mixing
    geom_line()+                                              # draw as line plot
    facet_wrap(. ~ dyad_id)+                                     # new panel per dyad as each pair has own edge weight parameter
    theme_classic()+                                          # make it look nicer
    theme(legend.position = 'none',                           # remove legend
          strip.background = element_blank(), strip.text = element_blank())    # remove facet strips
  ggplot(data = plot_edges[plot_edges$seen_together == 0,],   # repeat for dyads that have never been seen together
         aes(y = edge_draw, x = position, colour = chain))+
    geom_line()+
    facet_wrap(. ~ dyad_id)+
    theme_classic() + theme(legend.position = 'none', strip.background = element_blank(), strip.text = element_blank())
  
  ### density plots
  plot(NULL, xlim = c(0,1), ylim = c(0,30), las = 1, xlab = 'edge weight', ylab = 'density')         # set up plot window
  for(i in 1:length(plot_dyads)){                                                                    # plot randomly sampled dyads
    x <- plot_edges[plot_edges$dyad_id == plot_dyads[i],]                                            # select data to plot
    lines(density(x$edge_draw), col = ifelse(x$seen_together == 1, rgb(0,0,1,0.1), rgb(1,0,0,0.1)))  # draw edge weight probability plot. blue = seen together at least once, red = never seen together
  }
  
  #### plot network ####
  ### create single matrix of edge samples
  colnames(edge_samples) <- yr$dyad_id                                        # match to dyad ID numbers
  
  ### plot network across 6 different threshold values for comparison to other networks
  par(mai = c(0.1,0.1,0.1,0.1))
  plot_network_threshold(edge_samples = edge_samples,
                        dyad_data = yr, threshold = 0.05,
                        node.size = nodes, node.colour = nodes,
                        lwd = 15)
  plot_network_threshold(edge_samples = edge_samples,
                         dyad_data = yr, threshold = 0.10,
                         node.size = nodes, node.colour = nodes,
                         lwd = 15)
  # plot_network_threshold(edge_samples = edge_samples,
  #                        dyad_data = yr, threshold = 0.15,
  #                        node.size = nodes, node.colour = nodes,
  #                        lwd = 15)
  # plot_network_threshold(edge_samples = edge_samples,
  #                        dyad_data = yr, threshold = 0.20,
  #                        node.size = nodes, node.colour = nodes,
  #                        lwd = 15)
  #plot_network_threshold(edge_samples = edge_samples,
  #                       dyad_data = yr, threshold = 0.25,
  #                       node.size = nodes, node.colour = nodes,
  #                       lwd = 15)
  #plot_network_threshold(edge_samples = edge_samples,
  #                       dyad_data = yr, threshold = 0.30,
  #                       node.size = nodes, node.colour = nodes,
  #                       lwd = 15)
  
  ### clean workspace
  rm(counts_ls, x, i) ; gc()
  
  ### save image
  save.image(paste0('step2_edgeweights/workspace_images/edgeweights_fitted_',start_years[year_id],'.RData'))
  
  ### clean workspace
  rm(list = ls() [ !ls() %in% c('edge_binary','n_chains','n_samples','year_id','plot_network_threshold','start_years')])
  
}
dev.off()
