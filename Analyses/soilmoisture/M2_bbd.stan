// soil moisture phenology  analysis
// Started by ailene September 2017

//2 level model for budburst day (or other phenophase)  as a function of above-ground temperature, soil moisture, and their interaction
// Levels: Species (random slopes for temp and sm), not including site yet

data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	//int<lower=1> n_site;
	//int<lower=1, upper=n_site> site[N];
	vector[N] y; 		// response
	vector[N] temp; 	// predictor
	vector[N] mois; 	// predictor
	
	}

transformed data {
  vector[N] inter_tm;           
  inter_tm    = temp .* mois;  
  	 }

parameters {
  real mu_a_sp;
  real mu_b_temp_sp;   
  real mu_b_mois_sp;
  real<lower=0> sigma_a_sp;
  real<lower=0> sigma_b_temp_sp; 
  real<lower=0> sigma_b_mois_sp;
  //real mu_a_site;
  //real mu_b_temp_site;   
  //real mu_b_mois_site;
  //real<lower=0> sigma_a_site;
  real<lower=0> sigma_y; 

  real a_sp[n_sp]; // intercept for species
  real b_temp[n_sp]; // slope of air temp effect for species
  real b_mois[n_sp]; // slope of soil moisture effect for species
  real b_tm; // slope of temp x mois effect- do i need to index this by species?
  //real a_site[n_site]; // intercept for sites

	}

transformed parameters {
   real yhat[N];
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_temp[sp[i]] * temp[i] + 
	      	b_mois[sp[i]] * mois[i] +
                b_tm * inter_tm[i];
			     	}
	}

model {

	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	b_temp ~ normal(mu_b_temp_sp, sigma_b_temp_sp); 
	b_mois ~ normal(mu_b_mois_sp, sigma_b_mois_sp); 
  //a_site ~ normal(mu_a_site, sigma_a_site); 
  mu_a_sp ~ normal(0, 200);
  sigma_a_sp ~ normal(0, 10);
  ///sigma_a_site ~ normal(0, 10);
	
	b_tm ~ normal(0, 10);
  mu_b_temp_sp ~ normal(0, 50);
  sigma_b_temp_sp ~ normal(0, 10);
  mu_b_mois_sp ~ normal(0, 50);
  sigma_b_mois_sp ~ normal(0, 10);

	y ~ normal(yhat, sigma_y);
}

