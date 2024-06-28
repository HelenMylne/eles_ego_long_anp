data {
  int n_data;
  int n_years;
  int n_nodes;
  array[n_data] int node;
  array[n_data] int year;
  //vector[n_data] age;
  vector[n_data] centrality_mu;
  vector[n_data] centrality_cov;
}

parameters {
  real intercept;
  vector[n_nodes] beta_node;
  real beta_year;
  //real beta_age;
  //real beta_interaction;
  //vector[n_years] year_random;
  //real<lower=0> sigma_year;
}

transformed parameters {
  //vector[n_nodes] year_random_effect;
  //year_random_effect = year_random * sigma_year;
  
  vector[n_data] predictor;
  for(i in 1:n_data){
    predictor[i] = intercept + beta_node[node[i]] + year[i] * beta_year; // + age[i] * beta_age; //year_random_effect[year[i]]; // + age * node * beta_interaction;
  }
}

model {
  // priors
  intercept ~ normal(logit(0.05),0.8);
  beta_node ~ normal(0,0.8);
  beta_year ~ normal(0,0.8);
  //beta_age ~ normal(0,1);
  // year_random ~ normal(0,1);
  // sigma_year ~ exponential(2);
  
  // model
  centrality_mu ~ normal(predictor, centrality_cov);
}

