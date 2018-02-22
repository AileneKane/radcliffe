// soil moisture phenology  analysis
// with test data

// 3 level model for budburst day (or other phenophase)  as a function of above-ground temperature, soil moisture, and their interaction
// Levels: Species and site (crossed)

data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	int<lower=1> n_site;
	int<lower=1, upper=n_site> site[N];
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
  real mu_a_site;
  real<lower=0> sigma_a_site;
  real<lower=0> sigma_y; 
  
  real a_sp[n_sp]; // intercept for species
  real b_temp[n_sp]; // species-level effect of temp
  real b_mois[n_sp]; // species-level effect of mois
  real b_tm[n_sp]; // species-level interaction of temp*mois
  real a_site[n_site]; // intercept for sites
	}

transformed parameters {
   real yhat[N];
       	for(i in 1:N){
          yhat[i] = a_sp[sp[i]] + // indexed with species
          a_site[site[i]] + // indexed with site
		      b_temp[sp[i]] * temp[i] + 
	      	b_mois[sp[i]] * mois[i] +
          b_tm[sp[i]] * inter_tm[i];
			     	}
	}

model {
  
	  a_sp ~ normal(mu_a_sp, sigma_a_sp); 
    mu_a_sp ~ normal(0, 200);
    sigma_a_sp ~ normal(0, 10);
    a_site ~ normal(mu_a_site, sigma_a_site); 
    mu_a_site ~ normal(0, 200);
    sigma_a_site ~ normal(0, 10);
	  b_temp ~ normal(mu_b_temp_sp, sigma_b_temp_sp); 
    mu_b_temp_sp ~ normal(0, 10);
    sigma_b_temp_sp ~ normal(0, 5);
    b_mois ~ normal(mu_b_mois_sp, sigma_b_mois_sp); 
    mu_b_mois_sp ~ normal(0, 10);
    sigma_b_mois_sp ~ normal(0, 5);
    b_tm ~ normal(mu_b_tm_sp, sigma_b_tm_sp);
    mu_b_tm_sp ~ normal(0, 10);
    sigma_b_tm_sp ~ normal(0, 5);
	  y ~ normal(yhat, sigma_y);
}
