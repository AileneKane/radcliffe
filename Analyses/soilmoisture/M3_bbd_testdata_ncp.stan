// soil moisture phenology  analysis
// fake data

// 2 level model for budburst day (or other phenophase)  as a function of above-ground temperature, soil moisture, and their interaction
// Levels: just Species for now, and intercept only

data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] temp; 	// predictor
	vector[N] mois; 	// predictor
	
	}

transformed data {
  vector[N] inter_tm;           
  inter_tm    = temp .* mois;  
  	 
  	 }
  	 
parameters {
  real mu_a_sp;// intercept for species
  real mu_b_temp_sp;   
  real mu_b_mois_sp;      
  real mu_b_tm_sp; // slope of temp x mois effect
  real<lower=0> sigma_a_sp;
  real<lower=0> sigma_b_temp_sp;
  real<lower=0> sigma_b_mois_sp;
  real<lower=0> sigma_b_tm_sp;
  real<lower=0> sigma_y; 

  real a_sp[n_sp]; // intercept for species
  real b_temp[n_sp]; // species-level effect of temp
  real b_mois[n_sp]; // species-level effect of mois
  //real b_tm[n_sp]; // species-level interaction of temp*mois//removed for ncp
  real b_tm_ncp[n_sp];//added for ncp
	}

transformed parameters {
  vector[n_sp] b_tm;
for (i in 1:n_sp){
  b_tm[i] = mu_b_tm_sp + sigma_b_tm_sp*b_tm_ncp[i]; // you cannot do scalar * vector so if above had been vector in parameters block (see my code) had been vector (instead of real) it probably would have run; but this loop Andrew added is also a work-around!
}
	}

model {
  real yhat[N];
  for(i in 1:N){
          yhat[i] = a_sp[sp[i]] + // indexed with species
		      b_temp[sp[i]] * temp[i] + 
	      	b_mois[sp[i]] * mois[i] +
          b_tm[sp[i]] * inter_tm[i];
			     	}
	  a_sp ~ normal(mu_a_sp, sigma_a_sp); 
    mu_a_sp ~ normal(0, 200);
    sigma_a_sp ~ normal(0, 10);
	  b_temp ~ normal(mu_b_temp_sp, sigma_b_temp_sp); 
    mu_b_temp_sp ~ normal(0, 10);
    sigma_b_temp_sp ~ normal(0, 5);
    b_mois ~ normal(mu_b_mois_sp, sigma_b_mois_sp); 
    mu_b_mois_sp ~ normal(0, 10);
    sigma_b_mois_sp ~ normal(0, 5);
    //b_tm ~ normal(mu_b_tm_sp, sigma_b_tm_sp);
    //mu_b_tm_sp ~ normal(0, 10);
    //sigma_b_tm_sp ~ normal(0, 5);
    b_tm_ncp ~ normal(0,35);
	  y ~ normal(yhat, sigma_y);
}

