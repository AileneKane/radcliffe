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
  real b_temp;   
  real b_mois;      
  real b_tm; // slope of temp x mois effect
  real<lower=0> sigma_a_sp;
  real<lower=0> sigma_y; 

  real a_sp[n_sp]; // intercept for species
	}

transformed parameters {
   real yhat[N];
       	for(i in 1:N){
           yhat[i] = a_sp[sp[i]] + // indexed with species
		      b_temp * temp[i] + 
	      	b_mois * mois[i] +
          b_tm * inter_tm[i];
			     	}
	}

model {

	  a_sp ~ normal(mu_a_sp, sigma_a_sp); 
    mu_a_sp ~ normal(0, 200);
    sigma_a_sp ~ normal(0, 10);
	  b_temp ~ normal(0, 10);
    b_mois ~ normal(0, 10);
    b_tm ~ normal(0, 10);
	  y ~ normal(yhat, sigma_y);
}

