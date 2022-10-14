# Scripts for stan models for the I119 Project
# Author: Zheng Zhou, zhezhou@iu.edu
# Date: Dec 07 2021

# One pooled------
modelstring_onepool_nocov <- "
  // data in long format
  data {
    int<lower=0> N;                // total number of subjects
    vector[N] y;                     // Dependent variable, change in MET/wk
    real nu;                       // degree of freedom for the error
    int<lower=0> C;                // number of coaches
    matrix[N,C] mtx_X;               // design matrix 
    
    // hyperparameters
    real prior_dsigma[2];
    real prior_dbeta1[2];
  }
  
    transformed data{
    vector[N] dy;               // standardized y
    dy = (y - mean(y))/sd(y); 
  }

  parameters{
    real<lower=0> dsigma;
    vector[C] dbeta1;
  }
  
  model{
    // Priors
    dsigma ~ cauchy(prior_dsigma[1],prior_dsigma[2]);
    dbeta1 ~ normal(prior_dbeta1[1],prior_dbeta1[2]);
    
    // likelihood
      dy ~ student_t (nu,mtx_X * dbeta1 , dsigma);
  }
  
  generated quantities{
    real<lower=0> sigma;
    vector[C] beta1;
    
    sigma = sd(y) * dsigma;
    beta1 = dbeta1 * sd(y);
  }
  
"

modelstring_onepool_cov <- "
  // data in long format
  data {
    int<lower=0> N;                // total number of subjects
    vector[N] y;                     // Dependent variable, change in MET/wk
    real nu;                       // degree of freedom for the error
    int<lower=0> k;                // number of covariates
    matrix[N,k] mtx_X;               // design matrix 
    
    // hyperparameters
    real prior_dsigma[2];
    real prior_dbeta1[2];
  }
  
  transformed data{
    vector[N] dy;               // standardized y
    dy = (y - mean(y))/sd(y); 
  }

  parameters{
    real<lower=0> dsigma;
    vector[k] dbeta1;
  }
  
  model{
    // Priors
    dsigma ~ cauchy(prior_dsigma[1],prior_dsigma[2]);
    dbeta1 ~ normal(prior_dbeta1[1],prior_dbeta1[2]);
    
    // likelihood
    dy ~ student_t (nu,mtx_X * dbeta1 , dsigma);
  }
  
  generated quantities{
    real<lower=0> sigma;
    vector[k] beta1;
    
    sigma = sd(y) * dsigma;
    beta1 = dbeta1 * sd(y);
  }
  
"

# Mixed effects---------
modelstring_hier_mean_cov <- "
  data{
    int<lower=0> N;           // total number of data
    int<lower=0> C;          // number of Coaches
    int<lower=0> K;            // number of covariates
    int<lower=0> Te;          // number of Terms
    real y[N];              // response
    row_vector[K] mtx_X[N];        // covariate matrix
    int<lower=0,upper=C> Coach[N];   // CoachID
    int<lower=0,upper=Te> Term[N];     // Term
    //matrix[N,C] mtx_CoachID;        // matrix of coachID
    real nu;                   // students't df
    
    // hyperparameters
    real prior_dsigma[2];
    real prior_dbeta1[2];
    real prior_dbeta2[2];
    real prior_dbeta3[2];
  }
  
  //transformed data{
  //  real dy[N];              // standardized y
  //  for(n in 1:N){
  //    dy[n] = (y[n] - mean(y))/ sd(y);
  //  }
  //}
  
  parameters{
    real<lower=0> dsigma;     // Z transformed sigma
    vector[K] dbeta1;              // coefficient of covariates
    vector[C] dbeta2;           // Coach specific intercept
    vector[Te] dbeta3;          // Term specific
  }
  
  model{
    // priors
    dsigma ~ cauchy(prior_dsigma[1],prior_dsigma[2]);
    dbeta1 ~ normal(prior_dbeta1[1],prior_dbeta1[2]);
    dbeta2 ~ normal(prior_dbeta2[1],prior_dbeta2[2]);
    dbeta3 ~ normal(prior_dbeta3[1],prior_dbeta3[2]);
    
    // likelihood
    for(n in 1:N){
      y[n] ~ student_t (nu,
      mtx_X[n] * dbeta1 + dbeta2[Coach[n]] + dbeta3[Term[n]],
      dsigma);
    }
  }
  
  //generated quantities{
  //  real<lower=0> sigma;
  //  vector[K] beta1;
  //  vector[C] beta2;
  //  vector[Te] beta3;
  //  sigma = sd(y) * dsigma + mean(y);
  //  beta1 = dbeta1 * sd(y);
  //  beta2 = sd(y) * dbeta2;
  //  beta3 = sd(y) * dbeta3;
  //}

"

modelstring_hier_mean_cov_alt <- "
  data{
    int<lower=0> N;                       // number of observations
    // This model is mixed effect model
    // contains both fixed effects and two random effects
    int<lower=1> K;                       // number of fixed covariate
    // random effect: Coach
    int<lower=0> C;                       // number of coach
    // random effect: Term
    int<lower=0> Te;                       // number of terms
    real y[N];                            // outcomes
    int<lower=0,upper=C> Coach[N];      // Coach ID index
    int<lower=0,upper=Te> Term[N];         // Term index
    row_vector[K] mtx_X[N];               // matrix of fixed covariates dummy
    row_vector[C] mtx_Coach[N];           // matrix of coach dummy
    row_vector[Te] mtx_Term[N];            // matrix of term dummy
    int<lower=0> nu;                      // degree of freedom

    // hyperparameters    
    real prior_dsigma[2];
    real prior_dbeta1[2];
    real prior_dbeta2[2];
    real prior_dbeta3[2];
    
  }
  
  transformed data{
    real dy[N];
    for(n in 1:N){
      dy[n] = (y[n] - mean(y)) / sd(y);
    }
  }
  
  parameters{
    real<lower=0> dsigma;                 // standardized sigma
    vector[K] dbeta1;                     // Fixed effects
    vector[C] dbeta2[C];                  // Coach specific effect
    vector[Te] dbeta3[Te];                  // Term specific effect
  }
  
  model{
    dbeta1 ~ normal(prior_dbeta1[1],prior_dbeta1[2]);
    for(c in 1:C){
      dbeta2[c] ~ normal(prior_dbeta2[1],prior_dbeta2[2]);
    }
    for(te in 1:Te){
      dbeta3[te] ~ normal(prior_dbeta3[1],prior_dbeta3[2]);
    }
    dsigma ~ cauchy(prior_dsigma[1],prior_dsigma[2]);
    
    for(n in 1:N){
      dy[n] ~ student_t (nu,
      mtx_X[n] * dbeta1 + mtx_Coach[n] * dbeta2[Coach[n]] + mtx_Term[n] * dbeta3[Term[n]],
      dsigma);
    }
  }
  
  // generated quantities{
  //   real<lower=0> sigma;
  //   vector[K] beta1;
  //   vector[C] beta2[C];
  //   vector[Te] beta3[Te];
  //   sigma = sd(y) * dsigma + mean(y);
  //   beta1 = sd(y) * dbeta1;
  //   for(c in 1:C){
  //     beta2[c] = sd(y) * dbeta2[c];
  //   }
  //   for(te in 1:Te){
  //     beta3[te] = sd(y) * dbeta3[te];
  //   }
  // }

"
