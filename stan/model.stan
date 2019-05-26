data {
  int<lower=0>  N;
  int<lower=0>  max_t;
  int<lower=0>  dim_y;
  int           Time[N];
  int           Y[dim_y];
}

parameters {
  vector[max_t] beta0;
  real<lower=0> sd_beta0;
}

transformed parameters {
  vector[dim_y] theta;
  for(i in 0:(N -1)){
    int time = Time[i + 1];
    for(j in 1:time){
      theta[i + j] = inv_logit(beta0[j]);
    }  
  }
}

model {
  // Likelihood
  for(i in 0:(N -1)){
    int time = Time[i + 1];
    for(j in 1:time){
      Y[i + j] ~ binomial(1, theta[i + j]);
    }  
  }
  // Prior
  for(i in 2:max_t){
    beta0[i] ~ normal(beta0[i - 1], sd_beta0);
  }
  sd_beta0 ~ gamma(0.001, 0.001);
  beta0[1] ~ normal(0, 0.01);
}

