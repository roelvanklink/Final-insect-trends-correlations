// generated with brms 2.16.1
functions {
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  real meanResponse; //mean value of data
  real sdResponse; //sd value of data
}
transformed data {
  int Kc = K - 1;
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // population-level effects
  real Intercept;  // temporary intercept for centered predictors
  real<lower=0> sigma;  // dispersion parameter
}
transformed parameters {
}
model {
  // likelihood including constants
  target += normal_id_glm_lpdf(Y | Xc, Intercept, b, sigma);
  // priors including constants
  target += normal_lpdf(b | 0, 1);
  target += student_t_lpdf(Intercept | 3, meanResponse, sdResponse);
  target += student_t_lpdf(sigma | 3, 0, sdResponse)
  - 1 * student_t_lccdf(0 | 3, 0, sdResponse);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
}
