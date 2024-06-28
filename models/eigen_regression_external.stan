data {
  // global parameters
  int n_data;
  int n_years;
  int n_nodes;
  
  // explanatory variables
  array[n_data] int node;
  array[n_data] int year;
  array[n_data] int population_size;
  
  // number of elephants per year
  int n_nodes1;
  int n_nodes2;
  int n_nodes3;
  int n_nodes4;
  int n_nodes5;
  int n_nodes6;
  int n_nodes7;
  int n_nodes8;
  int n_nodes9;
  int n_nodes10;
  int n_nodes11;
  int n_nodes12;
  int n_nodes13;
  int n_nodes14;
  int n_nodes15;
  int n_nodes16;
  int n_nodes17;
  int n_nodes18;
  int n_nodes19;
  int n_nodes20;
  int n_nodes21;
  int n_nodes22;
  int n_nodes23;
  int n_nodes24;
  int n_nodes25;
  int n_nodes26;
  int n_nodes27;
  int n_nodes28;
  int n_nodes29;
  int n_nodes30;
  int n_nodes31;
  int n_nodes32;
  int n_nodes33;
  int n_nodes34;
  int n_nodes35;
  int n_nodes36;
  int n_nodes37;
  int n_nodes38;
  int n_nodes39;
  int n_nodes40;
  int n_nodes41;
  int n_nodes42;
  int n_nodes43;
  int n_nodes44;
  int n_nodes45;
  
  // number of elephants in previous years
  int prev_eles1;
  int prev_eles2;
  int prev_eles3;
  int prev_eles4;
  int prev_eles5;
  int prev_eles6;
  int prev_eles7;
  int prev_eles8;
  int prev_eles9;
  int prev_eles10;
  int prev_eles11;
  int prev_eles12;
  int prev_eles13;
  int prev_eles14;
  int prev_eles15;
  int prev_eles16;
  int prev_eles17;
  int prev_eles18;
  int prev_eles19;
  int prev_eles20;
  int prev_eles21;
  int prev_eles22;
  int prev_eles23;
  int prev_eles24;
  int prev_eles25;
  int prev_eles26;
  int prev_eles27;
  int prev_eles28;
  int prev_eles29;
  int prev_eles30;
  int prev_eles31;
  int prev_eles32;
  int prev_eles33;
  int prev_eles34;
  int prev_eles35;
  int prev_eles36;
  int prev_eles37;
  int prev_eles38;
  int prev_eles39;
  int prev_eles40;
  int prev_eles41;
  int prev_eles42;
  int prev_eles43;
  int prev_eles44;
  int prev_eles45;
  
  // // normal approximation of centrality: all together
  // vector[n_data] centrality_mean;
  // vector[n_data] centrality_stdv;
  
  // normal approximation of centrality: split by time window
  vector[n_nodes1] centrality_mean1;
  vector[n_nodes2] centrality_mean2;
  vector[n_nodes3] centrality_mean3;
  vector[n_nodes4] centrality_mean4;
  vector[n_nodes5] centrality_mean5;
  vector[n_nodes6] centrality_mean6;
  vector[n_nodes7] centrality_mean7;
  vector[n_nodes8] centrality_mean8;
  vector[n_nodes9] centrality_mean9;
  vector[n_nodes10] centrality_mean10;
  vector[n_nodes11] centrality_mean11;
  vector[n_nodes12] centrality_mean12;
  vector[n_nodes13] centrality_mean13;
  vector[n_nodes14] centrality_mean14;
  vector[n_nodes15] centrality_mean15;
  vector[n_nodes16] centrality_mean16;
  vector[n_nodes17] centrality_mean17;
  vector[n_nodes18] centrality_mean18;
  vector[n_nodes19] centrality_mean19;
  vector[n_nodes20] centrality_mean20;
  vector[n_nodes21] centrality_mean21;
  vector[n_nodes22] centrality_mean22;
  vector[n_nodes23] centrality_mean23;
  vector[n_nodes24] centrality_mean24;
  vector[n_nodes25] centrality_mean25;
  vector[n_nodes26] centrality_mean26;
  vector[n_nodes27] centrality_mean27;
  vector[n_nodes28] centrality_mean28;
  vector[n_nodes29] centrality_mean29;
  vector[n_nodes30] centrality_mean30;
  vector[n_nodes31] centrality_mean31;
  vector[n_nodes32] centrality_mean32;
  vector[n_nodes33] centrality_mean33;
  vector[n_nodes34] centrality_mean34;
  vector[n_nodes35] centrality_mean35;
  vector[n_nodes36] centrality_mean36;
  vector[n_nodes37] centrality_mean37;
  vector[n_nodes38] centrality_mean38;
  vector[n_nodes39] centrality_mean39;
  vector[n_nodes40] centrality_mean40;
  vector[n_nodes41] centrality_mean41;
  vector[n_nodes42] centrality_mean42;
  vector[n_nodes43] centrality_mean43;
  vector[n_nodes44] centrality_mean44;
  vector[n_nodes45] centrality_mean45;
  
  matrix[n_nodes1,n_nodes1] centrality_cov1;
  matrix[n_nodes2,n_nodes2] centrality_cov2;
  matrix[n_nodes3,n_nodes3] centrality_cov3;
  matrix[n_nodes4,n_nodes4] centrality_cov4;
  matrix[n_nodes5,n_nodes5] centrality_cov5;
  matrix[n_nodes6,n_nodes6] centrality_cov6;
  matrix[n_nodes7,n_nodes7] centrality_cov7;
  matrix[n_nodes8,n_nodes8] centrality_cov8;
  matrix[n_nodes9,n_nodes9] centrality_cov9;
  matrix[n_nodes10,n_nodes10] centrality_cov10;
  matrix[n_nodes11,n_nodes11] centrality_cov11;
  matrix[n_nodes12,n_nodes12] centrality_cov12;
  matrix[n_nodes13,n_nodes13] centrality_cov13;
  matrix[n_nodes14,n_nodes14] centrality_cov14;
  matrix[n_nodes15,n_nodes15] centrality_cov15;
  matrix[n_nodes16,n_nodes16] centrality_cov16;
  matrix[n_nodes17,n_nodes17] centrality_cov17;
  matrix[n_nodes18,n_nodes18] centrality_cov18;
  matrix[n_nodes19,n_nodes19] centrality_cov19;
  matrix[n_nodes20,n_nodes20] centrality_cov20;
  matrix[n_nodes21,n_nodes21] centrality_cov21;
  matrix[n_nodes22,n_nodes22] centrality_cov22;
  matrix[n_nodes23,n_nodes23] centrality_cov23;
  matrix[n_nodes24,n_nodes24] centrality_cov24;
  matrix[n_nodes25,n_nodes25] centrality_cov25;
  matrix[n_nodes26,n_nodes26] centrality_cov26;
  matrix[n_nodes27,n_nodes27] centrality_cov27;
  matrix[n_nodes28,n_nodes28] centrality_cov28;
  matrix[n_nodes29,n_nodes29] centrality_cov29;
  matrix[n_nodes30,n_nodes30] centrality_cov30;
  matrix[n_nodes31,n_nodes31] centrality_cov31;
  matrix[n_nodes32,n_nodes32] centrality_cov32;
  matrix[n_nodes33,n_nodes33] centrality_cov33;
  matrix[n_nodes34,n_nodes34] centrality_cov34;
  matrix[n_nodes35,n_nodes35] centrality_cov35;
  matrix[n_nodes36,n_nodes36] centrality_cov36;
  matrix[n_nodes37,n_nodes37] centrality_cov37;
  matrix[n_nodes38,n_nodes38] centrality_cov38;
  matrix[n_nodes39,n_nodes39] centrality_cov39;
  matrix[n_nodes40,n_nodes40] centrality_cov40;
  matrix[n_nodes41,n_nodes41] centrality_cov41;
  matrix[n_nodes42,n_nodes42] centrality_cov42;
  matrix[n_nodes43,n_nodes43] centrality_cov43;
  matrix[n_nodes44,n_nodes44] centrality_cov44;
  matrix[n_nodes45,n_nodes45] centrality_cov45;
}

parameters {
  real intercept;
  real beta_year;
  real beta_popn;
  vector[n_nodes] beta_node;
  real sigma;
}

transformed parameters {
  // vector[n_data] predictor;
  // for(i in 1:n_data){
  //   predictor[i] = intercept + beta_node[node[i]] + year[i] * beta_year + population_size[i] * beta_popn;
  // }
  vector[n_nodes1] predictor1;
  for(i in 1:n_nodes1){
    predictor1[i] = intercept + beta_node[node[i + prev_eles1]] + year[i + prev_eles1] * beta_year + population_size[i + prev_eles1] * beta_popn;
  }
  vector[n_nodes2] predictor2;
  for(i in 1:n_nodes2){
    predictor2[i] = intercept + beta_node[node[i + prev_eles2]] + year[i + prev_eles2] * beta_year + population_size[i + prev_eles2] * beta_popn;
  }
  vector[n_nodes3] predictor3;
  for(i in 1:n_nodes3){
    predictor3[i] = intercept + beta_node[node[i + prev_eles3]] + year[i + prev_eles3] * beta_year + population_size[i + prev_eles3] * beta_popn;
  }
  vector[n_nodes4] predictor4;
  for(i in 1:n_nodes4){
    predictor4[i] = intercept + beta_node[node[i + prev_eles4]] + year[i + prev_eles4] * beta_year + population_size[i + prev_eles4] * beta_popn;
  }
  vector[n_nodes5] predictor5;
  for(i in 1:n_nodes5){
    predictor5[i] = intercept + beta_node[node[i + prev_eles5]] + year[i + prev_eles5] * beta_year + population_size[i + prev_eles5] * beta_popn;
  }
  vector[n_nodes6] predictor6;
  for(i in 1:n_nodes6){
    predictor6[i] = intercept + beta_node[node[i + prev_eles6]] + year[i + prev_eles6] * beta_year + population_size[i + prev_eles6] * beta_popn;
  }
  vector[n_nodes7] predictor7;
  for(i in 1:n_nodes7){
    predictor7[i] = intercept + beta_node[node[i + prev_eles7]] + year[i + prev_eles7] * beta_year + population_size[i + prev_eles7] * beta_popn;
  }
  vector[n_nodes8] predictor8;
  for(i in 1:n_nodes8){
    predictor8[i] = intercept + beta_node[node[i + prev_eles8]] + year[i + prev_eles8] * beta_year + population_size[i + prev_eles8] * beta_popn;
  }
  vector[n_nodes9] predictor9;
  for(i in 1:n_nodes9){
    predictor9[i] = intercept + beta_node[node[i + prev_eles9]] + year[i + prev_eles9] * beta_year + population_size[i + prev_eles9] * beta_popn;
  }
  vector[n_nodes10] predictor10;
  for(i in 1:n_nodes10){
    predictor10[i] = intercept + beta_node[node[i + prev_eles10]] + year[i + prev_eles10] * beta_year + population_size[i + prev_eles10] * beta_popn;
  }
  vector[n_nodes11] predictor11;
  for(i in 1:n_nodes11){
    predictor11[i] = intercept + beta_node[node[i + prev_eles11]] + year[i + prev_eles11] * beta_year + population_size[i + prev_eles11] * beta_popn;
  }
  vector[n_nodes12] predictor12;
  for(i in 1:n_nodes12){
    predictor12[i] = intercept + beta_node[node[i + prev_eles12]] + year[i + prev_eles12] * beta_year + population_size[i + prev_eles12] * beta_popn;
  }
  vector[n_nodes13] predictor13;
  for(i in 1:n_nodes13){
    predictor13[i] = intercept + beta_node[node[i + prev_eles13]] + year[i + prev_eles13] * beta_year + population_size[i + prev_eles13] * beta_popn;
  }
  vector[n_nodes14] predictor14;
  for(i in 1:n_nodes14){
    predictor14[i] = intercept + beta_node[node[i + prev_eles14]] + year[i + prev_eles14] * beta_year + population_size[i + prev_eles14] * beta_popn;
  }
  vector[n_nodes15] predictor15;
  for(i in 1:n_nodes15){
    predictor15[i] = intercept + beta_node[node[i + prev_eles15]] + year[i + prev_eles15] * beta_year + population_size[i + prev_eles15] * beta_popn;
  }
  vector[n_nodes16] predictor16;
  for(i in 1:n_nodes16){
    predictor16[i] = intercept + beta_node[node[i + prev_eles16]] + year[i + prev_eles16] * beta_year + population_size[i + prev_eles16] * beta_popn;
  }
  vector[n_nodes17] predictor17;
  for(i in 1:n_nodes17){
    predictor17[i] = intercept + beta_node[node[i + prev_eles17]] + year[i + prev_eles17] * beta_year + population_size[i + prev_eles17] * beta_popn;
  }
  vector[n_nodes18] predictor18;
  for(i in 1:n_nodes18){
    predictor18[i] = intercept + beta_node[node[i + prev_eles18]] + year[i + prev_eles18] * beta_year + population_size[i + prev_eles18] * beta_popn;
  }
  vector[n_nodes19] predictor19;
  for(i in 1:n_nodes19){
    predictor19[i] = intercept + beta_node[node[i + prev_eles19]] + year[i + prev_eles19] * beta_year + population_size[i + prev_eles19] * beta_popn;
  }
  
  vector[n_nodes20] predictor20;
  for(i in 1:n_nodes20){
    predictor20[i] = intercept + beta_node[node[i + prev_eles20]] + year[i + prev_eles20] * beta_year + population_size[i + prev_eles20] * beta_popn;
  }
  vector[n_nodes21] predictor21;
  for(i in 1:n_nodes21){
    predictor21[i] = intercept + beta_node[node[i + prev_eles21]] + year[i + prev_eles21] * beta_year + population_size[i + prev_eles21] * beta_popn;
  }
  vector[n_nodes22] predictor22;
  for(i in 1:n_nodes22){
    predictor22[i] = intercept + beta_node[node[i + prev_eles22]] + year[i + prev_eles22] * beta_year + population_size[i + prev_eles22] * beta_popn;
  }
  vector[n_nodes23] predictor23;
  for(i in 1:n_nodes23){
    predictor23[i] = intercept + beta_node[node[i + prev_eles23]] + year[i + prev_eles23] * beta_year + population_size[i + prev_eles23] * beta_popn;
  }
  vector[n_nodes24] predictor24;
  for(i in 1:n_nodes24){
    predictor24[i] = intercept + beta_node[node[i + prev_eles24]] + year[i + prev_eles24] * beta_year + population_size[i + prev_eles24] * beta_popn;
  }
  vector[n_nodes25] predictor25;
  for(i in 1:n_nodes25){
    predictor25[i] = intercept + beta_node[node[i + prev_eles25]] + year[i + prev_eles25] * beta_year + population_size[i + prev_eles25] * beta_popn;
  }
  vector[n_nodes26] predictor26;
  for(i in 1:n_nodes26){
    predictor26[i] = intercept + beta_node[node[i + prev_eles26]] + year[i + prev_eles26] * beta_year + population_size[i + prev_eles26] * beta_popn;
  }
  vector[n_nodes27] predictor27;
  for(i in 1:n_nodes27){
    predictor27[i] = intercept + beta_node[node[i + prev_eles27]] + year[i + prev_eles27] * beta_year + population_size[i + prev_eles27] * beta_popn;
  }
  vector[n_nodes28] predictor28;
  for(i in 1:n_nodes28){
    predictor28[i] = intercept + beta_node[node[i + prev_eles28]] + year[i + prev_eles28] * beta_year + population_size[i + prev_eles28] * beta_popn;
  }
  vector[n_nodes29] predictor29;
  for(i in 1:n_nodes29){
    predictor29[i] = intercept + beta_node[node[i + prev_eles29]] + year[i + prev_eles29] * beta_year + population_size[i + prev_eles29] * beta_popn;
  }
  
  vector[n_nodes30] predictor30;
  for(i in 1:n_nodes30){
    predictor30[i] = intercept + beta_node[node[i + prev_eles30]] + year[i + prev_eles30] * beta_year + population_size[i + prev_eles30] * beta_popn;
  }
  vector[n_nodes31] predictor31;
  for(i in 1:n_nodes31){
    predictor31[i] = intercept + beta_node[node[i + prev_eles31]] + year[i + prev_eles31] * beta_year + population_size[i + prev_eles31] * beta_popn;
  }
  vector[n_nodes32] predictor32;
  for(i in 1:n_nodes32){
    predictor32[i] = intercept + beta_node[node[i + prev_eles32]] + year[i + prev_eles32] * beta_year + population_size[i + prev_eles32] * beta_popn;
  }
  vector[n_nodes33] predictor33;
  for(i in 1:n_nodes33){
    predictor33[i] = intercept + beta_node[node[i + prev_eles33]] + year[i + prev_eles33] * beta_year + population_size[i + prev_eles33] * beta_popn;
  }
  vector[n_nodes34] predictor34;
  for(i in 1:n_nodes34){
    predictor34[i] = intercept + beta_node[node[i + prev_eles34]] + year[i + prev_eles34] * beta_year + population_size[i + prev_eles34] * beta_popn;
  }
  vector[n_nodes35] predictor35;
  for(i in 1:n_nodes35){
    predictor35[i] = intercept + beta_node[node[i + prev_eles35]] + year[i + prev_eles35] * beta_year + population_size[i + prev_eles35] * beta_popn;
  }
  vector[n_nodes36] predictor36;
  for(i in 1:n_nodes36){
    predictor36[i] = intercept + beta_node[node[i + prev_eles36]] + year[i + prev_eles36] * beta_year + population_size[i + prev_eles36] * beta_popn;
  }
  vector[n_nodes37] predictor37;
  for(i in 1:n_nodes37){
    predictor37[i] = intercept + beta_node[node[i + prev_eles37]] + year[i + prev_eles37] * beta_year + population_size[i + prev_eles37] * beta_popn;
  }
  vector[n_nodes38] predictor38;
  for(i in 1:n_nodes38){
    predictor38[i] = intercept + beta_node[node[i + prev_eles38]] + year[i + prev_eles38] * beta_year + population_size[i + prev_eles38] * beta_popn;
  }
  vector[n_nodes39] predictor39;
  for(i in 1:n_nodes39){
    predictor39[i] = intercept + beta_node[node[i + prev_eles39]] + year[i + prev_eles39] * beta_year + population_size[i + prev_eles39] * beta_popn;
  }
  
  vector[n_nodes40] predictor40;
  for(i in 1:n_nodes40){
    predictor40[i] = intercept + beta_node[node[i + prev_eles40]] + year[i + prev_eles40] * beta_year + population_size[i + prev_eles40] * beta_popn;
  }
  vector[n_nodes41] predictor41;
  for(i in 1:n_nodes41){
    predictor41[i] = intercept + beta_node[node[i + prev_eles41]] + year[i + prev_eles41] * beta_year + population_size[i + prev_eles41] * beta_popn;
  }
  vector[n_nodes42] predictor42;
  for(i in 1:n_nodes42){
    predictor42[i] = intercept + beta_node[node[i + prev_eles42]] + year[i + prev_eles42] * beta_year + population_size[i + prev_eles42] * beta_popn;
  }
  vector[n_nodes43] predictor43;
  for(i in 1:n_nodes43){
    predictor43[i] = intercept + beta_node[node[i + prev_eles43]] + year[i + prev_eles43] * beta_year + population_size[i + prev_eles43] * beta_popn;
  }
  vector[n_nodes44] predictor44;
  for(i in 1:n_nodes44){
    predictor44[i] = intercept + beta_node[node[i + prev_eles44]] + year[i + prev_eles44] * beta_year + population_size[i + prev_eles44] * beta_popn;
  }
  vector[n_nodes45] predictor45;
  for(i in 1:n_nodes45){
    predictor45[i] = intercept + beta_node[node[i + prev_eles45]] + year[i + prev_eles45] * beta_year + population_size[i + prev_eles45] * beta_popn;
  }
  
}

model {
  // priors
  intercept ~ normal(logit(0.05),0.8);
  beta_node ~ normal(0,0.6);
  beta_year ~ normal(0,0.6);
  beta_popn ~ normal(0,0.6);
  sigma ~ exponential(1);
  
  // model
  //centrality_mean ~ normal(predictor, centrality_stdv);
  centrality_mean1 ~ multi_normal(predictor1, centrality_cov1 + diag_matrix(rep_vector(sigma, n_nodes1)));
  centrality_mean2 ~ multi_normal(predictor2, centrality_cov2 + diag_matrix(rep_vector(sigma, n_nodes2)));
  centrality_mean3 ~ multi_normal(predictor3, centrality_cov3 + diag_matrix(rep_vector(sigma, n_nodes3)));
  centrality_mean4 ~ multi_normal(predictor4, centrality_cov4 + diag_matrix(rep_vector(sigma, n_nodes4)));
  centrality_mean5 ~ multi_normal(predictor5, centrality_cov5 + diag_matrix(rep_vector(sigma, n_nodes5)));
  centrality_mean6 ~ multi_normal(predictor6, centrality_cov6 + diag_matrix(rep_vector(sigma, n_nodes6)));
  centrality_mean7 ~ multi_normal(predictor7, centrality_cov7 + diag_matrix(rep_vector(sigma, n_nodes7)));
  centrality_mean8 ~ multi_normal(predictor8, centrality_cov8 + diag_matrix(rep_vector(sigma, n_nodes8)));
  centrality_mean9 ~ multi_normal(predictor9, centrality_cov9 + diag_matrix(rep_vector(sigma, n_nodes9)));
  centrality_mean10 ~ multi_normal(predictor10, centrality_cov10 + diag_matrix(rep_vector(sigma, n_nodes10)));
  centrality_mean11 ~ multi_normal(predictor11, centrality_cov11 + diag_matrix(rep_vector(sigma, n_nodes11)));
  centrality_mean12 ~ multi_normal(predictor12, centrality_cov12 + diag_matrix(rep_vector(sigma, n_nodes12)));
  centrality_mean13 ~ multi_normal(predictor13, centrality_cov13 + diag_matrix(rep_vector(sigma, n_nodes13)));
  centrality_mean14 ~ multi_normal(predictor14, centrality_cov14 + diag_matrix(rep_vector(sigma, n_nodes14)));
  centrality_mean15 ~ multi_normal(predictor15, centrality_cov15 + diag_matrix(rep_vector(sigma, n_nodes15)));
  centrality_mean16 ~ multi_normal(predictor16, centrality_cov16 + diag_matrix(rep_vector(sigma, n_nodes16)));
  centrality_mean17 ~ multi_normal(predictor17, centrality_cov17 + diag_matrix(rep_vector(sigma, n_nodes17)));
  centrality_mean18 ~ multi_normal(predictor18, centrality_cov18 + diag_matrix(rep_vector(sigma, n_nodes18)));
  centrality_mean19 ~ multi_normal(predictor19, centrality_cov19 + diag_matrix(rep_vector(sigma, n_nodes19)));
  centrality_mean20 ~ multi_normal(predictor20, centrality_cov20 + diag_matrix(rep_vector(sigma, n_nodes20)));
  centrality_mean21 ~ multi_normal(predictor21, centrality_cov21 + diag_matrix(rep_vector(sigma, n_nodes21)));
  centrality_mean22 ~ multi_normal(predictor22, centrality_cov22 + diag_matrix(rep_vector(sigma, n_nodes22)));
  centrality_mean23 ~ multi_normal(predictor23, centrality_cov23 + diag_matrix(rep_vector(sigma, n_nodes23)));
  centrality_mean24 ~ multi_normal(predictor24, centrality_cov24 + diag_matrix(rep_vector(sigma, n_nodes24)));
  centrality_mean25 ~ multi_normal(predictor25, centrality_cov25 + diag_matrix(rep_vector(sigma, n_nodes25)));
  centrality_mean26 ~ multi_normal(predictor26, centrality_cov26 + diag_matrix(rep_vector(sigma, n_nodes26)));
  centrality_mean27 ~ multi_normal(predictor27, centrality_cov27 + diag_matrix(rep_vector(sigma, n_nodes27)));
  centrality_mean28 ~ multi_normal(predictor28, centrality_cov28 + diag_matrix(rep_vector(sigma, n_nodes28)));
  centrality_mean29 ~ multi_normal(predictor29, centrality_cov29 + diag_matrix(rep_vector(sigma, n_nodes29)));
  centrality_mean30 ~ multi_normal(predictor30, centrality_cov30 + diag_matrix(rep_vector(sigma, n_nodes30)));
  centrality_mean31 ~ multi_normal(predictor31, centrality_cov31 + diag_matrix(rep_vector(sigma, n_nodes31)));
  centrality_mean32 ~ multi_normal(predictor32, centrality_cov32 + diag_matrix(rep_vector(sigma, n_nodes32)));
  centrality_mean33 ~ multi_normal(predictor33, centrality_cov33 + diag_matrix(rep_vector(sigma, n_nodes33)));
  centrality_mean34 ~ multi_normal(predictor34, centrality_cov34 + diag_matrix(rep_vector(sigma, n_nodes34)));
  centrality_mean35 ~ multi_normal(predictor35, centrality_cov35 + diag_matrix(rep_vector(sigma, n_nodes35)));
  centrality_mean36 ~ multi_normal(predictor36, centrality_cov36 + diag_matrix(rep_vector(sigma, n_nodes36)));
  centrality_mean37 ~ multi_normal(predictor37, centrality_cov37 + diag_matrix(rep_vector(sigma, n_nodes37)));
  centrality_mean38 ~ multi_normal(predictor38, centrality_cov38 + diag_matrix(rep_vector(sigma, n_nodes38)));
  centrality_mean39 ~ multi_normal(predictor39, centrality_cov39 + diag_matrix(rep_vector(sigma, n_nodes39)));
  centrality_mean40 ~ multi_normal(predictor40, centrality_cov40 + diag_matrix(rep_vector(sigma, n_nodes40)));
  centrality_mean41 ~ multi_normal(predictor41, centrality_cov41 + diag_matrix(rep_vector(sigma, n_nodes41)));
  centrality_mean42 ~ multi_normal(predictor42, centrality_cov42 + diag_matrix(rep_vector(sigma, n_nodes42)));
  centrality_mean43 ~ multi_normal(predictor43, centrality_cov43 + diag_matrix(rep_vector(sigma, n_nodes43)));
  centrality_mean44 ~ multi_normal(predictor44, centrality_cov44 + diag_matrix(rep_vector(sigma, n_nodes44)));
  centrality_mean45 ~ multi_normal(predictor45, centrality_cov45 + diag_matrix(rep_vector(sigma, n_nodes45)));
  
}

