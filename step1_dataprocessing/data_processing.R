#### information ####
## script to produce data for egocentric network analysis

#### set up ####
library(tidyverse)

#### identify individuals of interest ####
## import data
ate <- read_csv('../../data_processed/step1_dataprocessing/anp_sightings_rawcombined.csv')
nodes <- readxl::read_excel('../data_raw/Raw_ATE_LifeHistoryData_Fishlock220808.xlsx') %>% 
  janitor::clean_names() %>% 
  distinct() %>% 
  mutate(id = paste0('M',casename)) %>% 
  filter(id %in% ate$id)

## count sightings per ID
nodes$total_sightings <- NA
for(i in 1:nrow(nodes)){
  x <- ate %>% filter(id == nodes$id[i])
  nodes$total_sightings[i] <- nrow(x)
}
hist(nodes$total_sightings)
length(which(nodes$total_sightings > 200))

## count sightings per year
ate <- ate %>% 
  left_join(nodes[,c('id','byr')], by = 'id') %>% 
  mutate(age = year(obs_date) - byr,
         year = year(obs_date))
hist(ate$age)
oldest_elephant <- max(ate$age)
years <- data.frame(id = rep(nodes$id, each = oldest_elephant),
                    age = 1:oldest_elephant,
                    year_sightings = NA,
                    first_sighting = NA,
                    last_sighting = NA, 
                    age_diff_first_last = NA) %>% 
  left_join(nodes[,c('id','total_sightings')], by = 'id') %>% 
  relocate(total_sightings, .after = id)
for(i in 1:nrow(nodes)){
  x <- ate %>% filter(id == nodes$id[i])
  counts <- as.data.frame(table(x$age))
  for(j in 1:oldest_elephant){
    if(j %in% counts$Var1) {
      years$year_sightings[years$id == nodes$id[i] & years$age == j] <- counts$Freq[counts$Var1 == j]
    } else { years$year_sightings[years$id == nodes$id[i] & years$age == j] <- 0 }
  }
  years$first_sighting[years$id == nodes$id[i]] <- as.numeric(levels(counts$Var1)[1])
  years$last_sighting[years$id == nodes$id[i]] <- as.numeric(levels(counts$Var1)[nrow(counts)])
}
years$age_diff_first_last <- years$last_sighting - years$first_sighting

years %>% 
  select(id, age_diff_first_last) %>% 
  distinct() %>% 
  ggplot()+
  geom_histogram(aes(x = as.numeric(age_diff_first_last)))

possible_candidates <- years %>% 
  filter(age_diff_first_last >= oldest_elephant/2) %>% 
  filter(age > first_sighting & age < last_sighting)
length(unique(possible_candidates$id))
possible_candidates %>% 
  select(id, total_sightings) %>% 
  distinct() %>% 
  ggplot()+
  geom_histogram(aes(x = total_sightings), colour = 'black', fill = 'white')
ggplot(possible_candidates)+
  geom_boxplot(aes(x = id, y = year_sightings, fill = total_sightings))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

## selecting elephants: M234,M169,M221,M250,M105,M177,M032,M007,M321,M005 
selected <- c('M234','M169','M221','M250','M105','M177','M032','M007','M321','M005',
              'M093','M192','M276','M272','M319','M281','M331','M305','M162','M273','M355','M373')
possible_candidates %>%
  mutate(id = factor(id, levels = selected)) %>% 
  ggplot()+
  geom_boxplot(aes(x = id, y = year_sightings, fill = total_sightings))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_viridis_c(option = 'B')

selected <- c('M234','M169','M221','M250','M105','M177','M032','M007','M321','M005')
elephants_ego <- years %>% 
  filter(id %in% selected) %>% 
  left_join(nodes[nodes$id %in% selected,
                  c('casename','byr','dyr','id')], by = 'id') %>% 
  relocate(casename, .after = id) %>% 
  relocate(byr, .after = age) %>% 
  relocate(dyr, .after = byr)
rm(counts, possible_candidates, x, years, i, j, oldest_elephant) ; gc()

observed <- elephants_ego %>% 
  filter(year_sightings > 0)
table(observed$id)
min(observed$age_diff_first_last)
min(observed$total_sightings)
min(observed$age) ; max(observed$age) # youngest age is 7, but only sighting that year and literature indicates that 8 is about as young as will start to disperse so drop that and start at 8 years old

elephants_ego <- elephants_ego %>% 
  mutate(year = byr + age,
         first_sighting = ifelse(first_sighting == 7, 8, first_sighting)) %>% 
  relocate(year, .after = dyr)

#### create data frames per elephant ####
for(i in 1:length(selected)){
  # extract data per elephant
  elephant_of_interest <- elephants_ego %>% 
    filter(id == selected[i])
  
  # calculate year to start extracting observations
  age_first_sighting <- elephant_of_interest$first_sighting[1]
  if(age_first_sighting %% 2 != 0){
    age_first_sighting <- age_first_sighting + 1
  } 
  elephant_of_interest$start_yr <- elephant_of_interest$year[elephant_of_interest$age == age_first_sighting]
  
  # calculate year to stop extracting observations
  age_last_sighting <- elephant_of_interest$last_sighting[1]
  if((age_last_sighting - age_first_sighting) %% 2 != 0){
    elephant_of_interest$end_yr <- elephant_of_interest$year[elephant_of_interest$age == age_last_sighting]
  } else {
    elephant_of_interest$end_yr <- elephant_of_interest$year[elephant_of_interest$age == age_last_sighting - 1]
  }
  
  # add start and end years to main data frame
  elephants_ego$start_yr[elephants_ego$id == selected[i]] <- elephant_of_interest$start_yr[1]
  elephants_ego$end_yr[elephants_ego$id == selected[i]] <- elephant_of_interest$end_yr[1]
  
  # extract observations and save to file
  sightings <- ate %>% 
    filter(year >= elephant_of_interest$start_yr[1]) %>% 
    filter(year <= elephant_of_interest$end_yr[1])
  saveRDS(sightings, file = paste0('../data_processed/males/sightings_',selected[i],'.RDS'))
}

write_csv(elephants_ego, '../data_processed/males/selected_elephants.csv')

rm(elephant_of_interest, sightings, age_first_sighting, age_last_sighting, i) ; gc()
save.image('data_processing.RData')

#### convert data frames to gbi matrix ####
rm(list = ls() [! ls() %in% c('selected', 'observed')])
for(i in 1:length(selected)){
  ## import sightings data
  s <- readRDS(file = paste0('../data_processed/sightings_',selected[i],'.RDS')) %>% 
    mutate(obs_id_std = as.integer(as.factor(obs_id)))
  asnipe <- s %>% 
    select(id, obs_id_std) %>% 
    rename(ID = id, group = obs_id_std) %>% 
    data.table::setDT()
  gbi_matrix <- spatsoc::get_gbi(DT = asnipe, group = 'group', id = 'ID')
  
  ## save output
  saveRDS(gbi_matrix, file = paste0('../data_processed/males/gbimatrix_',selected[i],'.RDS'))
  
  ## progress report
  print(paste0('gbi_matrix for ', selected[i],' created at ', Sys.time()))
}

#### extract dyadic data frames of sightings per time window ####
rm(list = ls()); gc()
load('data_processing.RData')

ate$obs_id_std <- as.integer(as.factor(ate$obs_id))
colnames(ate)

nodes <- nodes %>% 
  left_join(distinct(ate[,c('id','node_id')]), by = 'id') %>% 
  mutate(node_1 = node_id,
         node_2 = node_id)

all_sightings_dyadic <- readRDS('../../data_processed/step1_dataprocessing/anp_allpairwiseevents/anp_bayesian_allpairwiseevents.RDS') %>% 
  rename(obs_id_std = obs_id) %>% 
  left_join(distinct(ate[,c('obs_id','obs_id_std','year','obs_date','corrected_time')]),
            by = 'obs_id_std') %>% 
  left_join(nodes[,c('id','node_1')], by = 'node_1') %>% 
  rename(id_1 = id) %>% 
  left_join(nodes[,c('id','node_2')], by = 'node_2') %>% 
  rename(id_2 = id)

## break down into data frames per elephant/time
length(unique(elephants_ego$id))
sort(unique(elephants_ego$start_yr)) # 1973 1974 1976 1978 1979 1982 1983 1984

all_df_needed <- elephants_ego %>% 
  select(id, byr, dyr, total_sightings, start_yr, end_yr) %>% 
  distinct() %>% 
  mutate(num_blocks = (end_yr - (start_yr-1))/2) # need data frames starting every 2 years from 1973-2021 and 1974-2020

odd_start_years <- seq(1973,2023, by = 2)
for(i in 1:(length(odd_start_years)-1)){
  two_years <- all_sightings_dyadic %>% 
    filter(year >= odd_start_years[i] & year < odd_start_years[i+1])
  saveRDS(two_years, file = paste0('../data_processed/step1_dataprocessing/long_dyad_pairs/dyad_pairs_',odd_start_years[i],'.RDS'))
}

even_start_years <- seq(1974,2022, by = 2)
for(i in 1:(length(even_start_years)-1)){
  two_years <- all_sightings_dyadic %>% 
    filter(year >= even_start_years[i] & year < even_start_years[i+1])
  saveRDS(two_years, file = paste0('../data_processed/step1_dataprocessing/long_dyad_pairs/dyad_pairs_',even_start_years[i],'.RDS'))
}

#### prep for edge weight models ####
rm(all_sightings_dyadic, observed, two_years, odd_start_years, even_start_years, i) ; gc()
start_years <- 1973:2021

for(year_id in start_years){
  ## import dyad data ----
  dyads <- readRDS(file = paste0('../data_processed/step1_dataprocessing/long_dyad_pairs/dyad_pairs_',year_id,'.RDS')) %>% 
    distinct()
  
  ## summarise ----
  dyads_split <- dyads %>%
    group_by(node_1, node_2) %>%
    summarise(event_count = sum(social_event),
              dyad_id = cur_group_id()) %>% 
    left_join(distinct(dyads[,c('node_1','node_2','id_1','id_2')]),
              by = c('node_1','node_2')) %>% 
    mutate(dyad = paste0(id_1, '_', id_2))
  head(dyads_split)
  
  ## add data about nodes ----
  nodes$ego <- ifelse(nodes$id %in% selected, 'yes', 'no')
  dyads_split <- dyads_split %>% 
    mutate(start_yr = year_id, end_yr = (year_id+1)) %>% 
    left_join(nodes[,c('node_1','byr','dyr','ego')], by = 'node_1') %>% 
    rename(byr_1 = byr, dyr_1 = dyr, ego_1 = ego) %>% 
    left_join(nodes[,c('node_2','byr','dyr','ego')], by = 'node_2') %>% 
    rename(byr_2 = byr, dyr_2 = dyr, ego_2 = ego)
  head(dyads_split)
  
  ## remove dyads where one or other pair has not been born yet or has already died ----
  dyads_alive <- dyads_split %>% 
    filter(byr_1 < year_id) %>% filter(dyr_1 > (year_id+1)) %>% 
    filter(byr_2 < year_id) %>% filter(dyr_2 > (year_id+1))
  
  ## count sightings within time frame ----
  sightings <- ate %>% 
    filter(year >= year_id) %>% 
    filter(year <= (year_id+1))
  eles <- sort(unique(c(dyads_alive$id_1, dyads_alive$id_2)))
  nodes_year <- nodes %>% 
    select(id) %>%
    filter(id %in% eles) %>% 
    mutate(sightings_period = NA)
  for(i in 1:nrow(nodes_year)){
    nodes_year$sightings_period[i] <- length(which(sightings$id == nodes_year$id[i]))
  }
  
  ## remove nodes never seen in that time frame ----
  nodes_year <- nodes_year %>% 
    filter(sightings_period > 0)
  dyads_present <- dyads_alive %>% 
    filter(id_1 %in% nodes_year$id) %>% 
    filter(id_2 %in% nodes_year$id)
  
  ## add count data ----
  nodes_year <- nodes_year %>% 
    mutate(id_1 = id) %>% mutate(id_2 = id)
  dyads_present <- dyads_present %>% 
    left_join(nodes_year[,c('id_1', 'sightings_period')], by = 'id_1') %>% 
    rename(sightings_1 = sightings_period) %>% 
    left_join(nodes_year[,c('id_2', 'sightings_period')], by = 'id_2') %>% 
    rename(sightings_2 = sightings_period)
  
  ## calculate total sightings ----
  dyads_present <- dyads_present %>% 
    mutate(total_sightings = (sightings_1 + sightings_2) - event_count)
  
  ## check for issues ----
  length(which(dyads_present$event_count > dyads_present$total_sightings)) # no dyads coming up with more events together than total number of times individuals observed
  
  ## save to file ----
  saveRDS(dyads_present, file = paste0('../data_processed/step1_dataprocessing/aggregated_dyad_pairs/dyad_pairs_aggregated_',year_id,'.RDS'))
  
  ## clean environment ----
  rm(list = ls()[! ls() %in% c('all_df_needed','ate','elephants_ego','nodes','selected','start_years','year_id')])
}
