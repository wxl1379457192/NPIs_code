data{
  int <lower=1> m; 
  int <lower=1> k1; // number of policy variables 7
  int <lower=1> k2; // number of states variables 5
  matrix[m, k1] X1; // policy variables
  matrix[m, k2] X2; // state variables
  vector[m] Y; //Y1
  vector[m] Y0;//Y0
}
parameters{
  real<lower=0> alpha_hier[k1]; // real priors
  real<lower=0> beta_hier[k2]; // real priors
  real<lower=0> uni[k1+k2]; // real priors
  real<lower=0> sigma;
  real<lower=0> sigma0;
}
transformed parameters {
  vector[k1] alpha;
  vector[k2] beta;
  vector[m] Y_mu;

  {
    for(i in 1:k1){
      alpha[i] = alpha_hier[i] - log(1.000382)/7 + uni[i];
    }
    for(j in 1:k2){
      beta[j] = beta_hier[j] - log(1.09)/5 + uni[k1+j];
    }
    Y_mu = -X1*alpha-X2*beta;
  }
}

model{
  alpha_hier ~ gamma(0.1667,1);
  beta_hier ~ gamma(0.1667,1);
  uni ~ normal(0,sigma0);
  for(i in 1:m){
    (1/Y[i]) ~ gamma(1/(exp(Y_mu[i])*Y0[i]),sigma);
  }
}



