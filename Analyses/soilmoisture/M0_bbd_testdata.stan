// soil moisture phenology  analysis
// test data

//  model for budburst day (or other phenophase)  as a function of above-ground temperature with no levels- simple model to try to figure out problems

data {
	int<lower=1> N;
	vector[N] y; 		// response
	vector[N] temp; 	// predictor
	vector[N] mois; 	// predictor
	
	}


parameters {
  real b_temp;   
  real b_mois;      
  real<lower=0> sigma_y; 

  real a; // intercept 
	}

transformed parameters {
   real yhat[N];
       	for(i in 1:N){
          yhat[i] = a[i] + 
		      b_temp * temp[i] + 
	      	b_mois * mois[i];
			     	}
	}

model {

	   a ~ normal(0, 35); 
	   b_temp ~ normal(0, 10);
    b_mois ~ normal(0, 10);
	  y ~ normal(yhat, sigma_y);
}

