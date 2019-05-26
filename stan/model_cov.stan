data {
  int<lower=0>  N;
  int<lower=0>  max_t;
  int<lower=0>  dim_y;
  int           Time[N];
  int           Y[dim_y];
  int           ncov;
  matrix[dim_y, ncov] X;
}

parameters {
  vector[max_t] beta0;
  vector[ncov]  beta;
  real<lower=0> sd_beta0;
}

transformed parameters {
  real<lower = 0, upper = 1> theta[dim_y];
  for(i in 0:(N -1)){
    int time = Time[i + 1];
    for(j in 1:time){
      theta[i + j] = inv_logit(beta0[j] + dot_product(row(X, i + j), beta));
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

