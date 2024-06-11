data {
  int n_data;
  int n_years;
  int n_nodes;
  vector[n_data] node;
  vector[n_data] age;
  vector[n_data] year;
  vector[n_data] centrality_mu;
  vector[n_data] centrality_cov;
}

parameters {
  real intercept;
  real beta_age;
  vector[n_nodes] beta_node;
  //real beta_interaction;
  vector[n_years] year_random;
}

transformed parameters {
  vector[n_data] predictor;
  for(i in 1:n_data){
    predictor[i] = intercept + age * beta_age + node * beta_node + year_random[year[i]]; // + age * node * beta_interaction
  }
  
}

model {
  centrality_mu ~ normal(predictor, centrality_cov);
}

