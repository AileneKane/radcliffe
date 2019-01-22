// soil moisture phenology  analysis
// with test data

// 3 level model for budburst day (or other phenophase)  as a function of above-ground temperature, soil moisture, and their interaction
// Levels: Species, site (crossed with species), and year nested within site

// generated with brms 2.1.8
// soil moisture phenology  analysis
// with test data

// 4 level model for budburst day (or other phenophase)  as a function of above-ground temperature, soil moisture, and their interaction
// Levels: Species, site (crossed), and year (nested within site)

// generated with brms 2.1.8
functions { 
} 
data { 
  int<lower=1> N;  // total number of observations 
  vector[N] Y;  // response variable 
  int<lower=1> K;  // number of population-level effects 
  matrix[N, K] X;  // population-level design matrix 
  // data for group-level effects of ID 1
  int<lower=1> J_1[N];
  int<lower=1> N_1;
  int<lower=1> M_1;
  vector[N] Z_1_1;
  // data for group-level effects of ID 2
  int<lower=1> J_2[N];
  int<lower=1> N_2;
  int<lower=1> M_2;
  vector[N] Z_2_1;
  // data for group-level effects of ID 3
  int<lower=1> J_3[N];
  int<lower=1> N_3;
  int<lower=1> M_3;
  vector[N] Z_3_1;
  vector[N] Z_3_2;
  vector[N] Z_3_3;
  vector[N] Z_3_4;
  int<lower=1> NC_3;
  int prior_only;  // should the likelihood be ignored? 
} 
transformed data { 
  int Kc = K - 1; 
  matrix[N, K - 1] Xc;  // centered version of X 
  vector[K - 1] means_X;  // column means of X before centering 
  for (i in 2:K) { 
    means_X[i - 1] = mean(X[, i]); 
    Xc[, i - 1] = X[, i] - means_X[i - 1]; 
  } 
} 
parameters { 
  vector[Kc] b;  // population-level effects 
  real temp_Intercept;  // temporary intercept 
  real<lower=0> sigma;  // residual SD 
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];  // unscaled group-level effects
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  vector[N_2] z_2[M_2];  // unscaled group-level effects
  vector<lower=0>[M_3] sd_3;  // group-level standard deviations
  matrix[M_3, N_3] z_3;  // unscaled group-level effects
  // cholesky factor of correlation matrix
  cholesky_factor_corr[M_3] L_3;
} 
transformed parameters { 
  // group-level effects 
  vector[N_1] r_1_1 = sd_1[1] * (z_1[1]);
  // group-level effects 
  vector[N_2] r_2_1 = sd_2[1] * (z_2[1]);
  // group-level effects 
  matrix[N_3, M_3] r_3 = (diag_pre_multiply(sd_3, L_3) * z_3)';
  vector[N_3] r_3_1 = r_3[, 1];
  vector[N_3] r_3_2 = r_3[, 2];
  vector[N_3] r_3_3 = r_3[, 3];
  vector[N_3] r_3_4 = r_3[, 4];
} 
model { 
  vector[N] mu = Xc * b + temp_Intercept; 
  for (n in 1:N) { 
    mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_2_1[J_2[n]] * Z_2_1[n] + r_3_1[J_3[n]] * Z_3_1[n] + r_3_2[J_3[n]] * Z_3_2[n] + r_3_3[J_3[n]] * Z_3_3[n] + r_3_4[J_3[n]] * Z_3_4[n];
  } 
  // priors including all constants 
  target += student_t_lpdf(temp_Intercept | 3, 94, 26); 
  target += student_t_lpdf(sigma | 3, 0, 26)
    - 1 * student_t_lccdf(0 | 3, 0, 26); 
  target += student_t_lpdf(sd_1 | 3, 0, 26)
    - 1 * student_t_lccdf(0 | 3, 0, 26); 
  target += normal_lpdf(z_1[1] | 0, 1);
  target += student_t_lpdf(sd_2 | 3, 0, 26)
    - 1 * student_t_lccdf(0 | 3, 0, 26); 
  target += normal_lpdf(z_2[1] | 0, 1);
  target += student_t_lpdf(sd_3 | 3, 0, 26)
    - 4 * student_t_lccdf(0 | 3, 0, 26); 
  target += normal_lpdf(to_vector(z_3) | 0, 1);
  target += lkj_corr_cholesky_lpdf(L_3 | 1); 
  // likelihood including all constants 
  if (!prior_only) { 
    target += normal_lpdf(Y | mu, sigma); 
  } 
} 
generated quantities { 
  // actual population-level intercept 
  real b_Intercept = temp_Intercept - dot_product(means_X, b); 
  corr_matrix[M_3] Cor_3 = multiply_lower_tri_self_transpose(L_3);
  vector<lower=-1,upper=1>[NC_3] cor_3;
  // take only relevant parts of correlation matrix
  cor_3[1] = Cor_3[1,2]; 
  cor_3[2] = Cor_3[1,3]; 
  cor_3[3] = Cor_3[2,3]; 
  cor_3[4] = Cor_3[1,4]; 
  cor_3[5] = Cor_3[2,4]; 
  cor_3[6] = Cor_3[3,4]; 
} 