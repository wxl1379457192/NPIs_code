data{
  int <lower=1> m; // number of countries
  int <lower=1> n;
  int <lower=1> k1; // number of policy variables 9
  int <lower=1> k2; // number of states variables 9
  matrix[m, k1] X1; // policy variables
  matrix[m, k2] X2; // policy environment variables
  vector[m] Y; //Y1
  vector[m] Y0;//Y0
  vector[n] Y0_new;//
  matrix[n, k1] X1_new; // prediction policy variables
  matrix[n, k2] X2_new; // prediction environment variables
}
parameters{
  real<lower=0> alpha_hier[k1]; // real priors
  real<lower=0> beta_hier[k2]; // real priors
  real<lower=0> sigma;
}
transformed parameters {
  vector[k1] alpha;
  vector[k2] beta;
  vector[m] Y_mu;
  vector[n] Y_mu_new;

  {
    for(i in 1:k1){
      alpha[i] = alpha_hier[i] - log(1.000382)/7;
    }
    for(i in 1:k2){
      beta[i] = beta_hier[i] - log(1.09)/5;
    }
    Y_mu = -X1*alpha-X2*beta;
    Y_mu_new = -X1_new*alpha-X2_new*beta;
  }

}

model{
  alpha_hier ~ gamma(.1667,1);
  beta_hier ~ gamma(.1667,1);
  for(i in 1:m){
    (1/Y[i]) ~ gamma(1/(exp(Y_mu[i])*Y0[i]),sigma);
  }
}

generated quantities {
  vector[n] y_new;

  {
  for(j in 1:n){
    y_new[j] = 1/(gamma_rng(1/(exp(Y_mu_new[j])*Y0_new[j]),sigma));
    }
  }
}

