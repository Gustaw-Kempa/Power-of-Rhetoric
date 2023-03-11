rm(list = ls())
library(tidyverse) #general data wrangling
library(rstan) #fitting Stan models
library(lubridate) #making the dates easier
library(BBmisc) # data scaling and normalizing
library(reshape2) # melting data for plots
library(loo)
library(arm)


set.seed(123)
model_data <- read.csv('datasets/model_data.csv')

# TODO: maybe implement some transformation on cases (log, ^2) - data is super skewed
hist(model_data$cases)


# in order to perform bayesian analysis with priors we need to normalize the data
model_data <- model_data %>% na.omit()
model_data <- tibble(date = model_data$date, country = model_data$country,week =  model_data$week,
                     normalize(model_data[,2:(ncol(model_data)-6)], method = "range", range = c(0,1)),
                     normalize(model_data[, (ncol(model_data)-2):ncol(model_data)-2], 
                                      method = "range", range = c(-1,1)),
                    cases = normalize(model_data$cases, method = "range", range = c(0,1)))


model_data$country_id <- as.integer(as.factor(model_data$country))


hist(model_data$diff1)

##### Prior predictive check ####
#use prior distributions to simulate hypothetical data and
#check whether it makes sense (compare with real data)

sim_diff<- normalize(rt(150, df=20), method = 'range', range = c(-1,1))
plot_df <- melt(tibble(obs = model_data$diff1,sim = sim_diff))
ggplot(plot_df, aes(x=value, fill = variable)) + geom_density()


#hist(model_data$sentiment, breaks = 7)
#hist(model_data$informativeness, breaks = 7)
#hist(model_data$persuasion, breaks = 7)

#### Model ####
# generating the Stan code



model_code <- 
  '
  
data {
  int<lower=0> N; // number of observations

  vector[N] sent; // vector of values of sentiment for each observation
  vector[N] info; // vector of values of informativeness for each observation
  vector[N] pers; // vector of values of persuasion for each observation
  vector[N] ang; // vector of values of sentiment for each observation
  vector[N] surp; // vector of values of sentiment for each observation
  vector[N] hap; // vector of values of sentiment for each observation
  vector[N] fear;
  vector[N] cases;
 vector[N] diff2;

 
  int country_id[N];
  int N_country;


  vector[N] V; // vector of changes in support
  
}


parameters {
  real alpha; //static value that denotes the general slope
  real<lower=0> sigma; // standard deviation of V
  real psi_sent; // influence of sentiment
  real psi_info; // influence of informativeness
  real psi_pers; // influence of persuasion
  real psi_ang; // influence of anger
  real psi_surp; // influence of surprise
  real psi_hap; // influence of happiness
  real psi_fear;
  real beta;
  real theta; //measure of cases
  
  vector[N_country] eta;
  real<lower = 0> sigma_eta;
}

transformed parameters{
  vector[N] eta_star;
  vector[N] mu;
  vector[N_country] eta_tilde;
  
  eta_tilde = eta*sigma_eta;
  
  for (i in 1:N){
  
  eta_star[i] = eta[country_id[i]]*sigma_eta; // non-centered parameterisation random effects 

  mu[i] = alpha +
       psi_sent*sent[i]+psi_info*info[i]+psi_pers*pers[i] +
       psi_ang*ang[i]+psi_surp*surp[i]+psi_hap*hap[i]+psi_fear*fear[i] + 
       beta*diff2[i] + theta*cases[i] +
       eta_star[i];
    }
}

model {

  alpha ~ normal(0,1);
  psi_sent ~ normal(0,1);
  psi_info ~ normal(0,1);
  psi_pers ~ normal(0,1);
  psi_ang ~ normal(0,1);
  psi_surp ~ normal(0,1);
  psi_hap ~ normal(0,1);
  psi_fear ~normal(0,1);
  beta ~ normal(0,1);
  sigma ~ normal(0,1);
  theta ~ normal(0,1);
  
  eta ~ normal(0,1);
  sigma_eta ~ normal(0,1);
  
  
  V ~ normal(mu,sigma);
    
}

generated quantities {
  vector[N] log_lik; // we need to generate log likelyhoods for LOO
   vector[N] y_rep; //sims of values
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(V[i] | alpha+psi_sent*sent[i]+psi_info*info[i]+psi_pers*pers[i]+
  psi_ang*ang[i]+psi_surp*surp[i]+psi_hap*hap[i]+psi_fear*fear[i]+theta*cases[i], sigma);
  }
  for (i in 1:N){
  y_rep[i] = normal_rng(mu[i],sigma);
  }}

'





model_code_tstud <-   '
data {
  int<lower=0> N; // number of observations

  vector[N] sent; // vector of values of sentiment for each observation
  vector[N] info; // vector of values of informativeness for each observation
  vector[N] pers; // vector of values of persuasion for each observation
  vector[N] ang; // vector of values of sentiment for each observation
  vector[N] surp; // vector of values of sentiment for each observation
  vector[N] hap; // vector of values of sentiment for each observation
  vector[N] fear;
  vector[N] cases;
 vector[N] diff2;
 
  int country_id[N];
  int N_country;


  vector[N] V; // vector of changes in support
  
}


parameters {
  real alpha; //static value that denotes the general slope
  real<lower=0> sigma; // standard deviation of V
  real psi_sent; // influence of sentiment
  real psi_info; // influence of informativeness
  real psi_pers; // influence of persuasion
  real psi_ang; // influence of anger
  real psi_surp; // influence of surprise
  real psi_hap; // influence of happiness
  real psi_fear;
  real beta;
  real nu;
  real theta; 
  
  vector[N_country] eta;
  real<lower = 0> sigma_eta;
}

transformed parameters{
  vector[N] eta_star;
  vector[N] mu;
  vector[N_country] eta_tilde;
  
  eta_tilde = eta*sigma_eta;
  
  for (i in 1:N){
  
  eta_star[i] = eta[country_id[i]]*sigma_eta; // non-centered parameterisation random effects 

  mu[i] = alpha +
       psi_sent*sent[i]+psi_info*info[i]+psi_pers*pers[i] +
       psi_ang*ang[i]+psi_surp*surp[i]+psi_hap*hap[i]+psi_fear*fear[i] + 
       beta*diff2[i] + theta*cases[i] +
       eta_star[i];
    }
}

model {

  alpha ~ normal(0,1);
  psi_sent ~ normal(0,1);
  psi_info ~ normal(0,1);
  psi_pers ~ normal(0,1);
  psi_ang ~ normal(0,1);
  psi_surp ~ normal(0,1);
  psi_hap ~ normal(0,1);
  psi_fear ~normal(0,1);
  beta ~ normal(0,1);
  sigma ~ normal(0,1);
  theta ~ normal(0,1);
  
  eta ~ normal(0,1);
  sigma_eta ~ normal(0,1);
  nu ~ normal(7,3);
  
  V ~ student_t(nu,mu,sigma);
    
}

generated quantities {
  vector[N] log_lik; // we need to generate log likelyhoods for LOO
  vector[N] y_rep;
  for (i in 1:N) {
    log_lik[i] = student_t_lpdf(V[i] | nu, alpha+psi_sent*sent[i]+psi_info*info[i]+psi_pers*pers[i]+
  psi_ang*ang[i]+psi_surp*surp[i]+psi_hap*hap[i]+psi_fear*fear[i]+theta*cases[i], sigma);
  }
    for (i in 1:N){
  y_rep[i] = student_t_rng(nu, mu[i],sigma);}}

'

# Defining the model data - for the mean values:

data <- list(N =nrow(model_data), sent = model_data$mean_sentiment,
             info = model_data$mean_informativeness,
             pers = model_data$mean_persuasion, ang = model_data$mean_anger,
             surp = model_data$mean_surprise, hap = model_data$mean_happiness,
             fear = model_data$mean_fear,
             diff2 = model_data$diff2,
             country_id = model_data$country_id,
             N_country = max(model_data$country_id),
             cases = model_data$cases,
             V = model_data$diff1)

# For the sd's
# data <- list(N =nrow(model_data), sent = model_data$sd_sentiment,
#              info = model_data$sd_informativeness,
#              pers = model_data$sd_persuasion, ang = model_data$sd_anger,
#              surp = model_data$sd_surprise, hap = model_data$sd_happiness,
#              fear = model_data$sd_fear,
#              diff2 = model_data$diff2,
#              country_id = model_data$country_id,
#              N_country = max(model_data$country_id),
#              V = model_data$diff1)


#pars <- c('alpha','psi_sent', 'psi_info', 'psi_pers','sigma', 'log_lik')

# Fitting the model 
fit <- stan(model_code = model_code, 
            #pars = pars,
            data = data, 
            iter = 1000,
            warmup = 500,
            refresh = 10,
            thin = 4, # thinning serves to reduce the autocorrelation between posterior samples for each parameter - overall serves to `stimulate convergence` earlier than otherwise 
            cores = 8,
            chains = 8,
            verbose = T)

fit2 <- stan(model_code = model_code_tstud, 
             #pars = pars,
             data = data, 
             iter = 1000,
             warmup = 500,
             refresh = 10,
             thin = 4, # thinning serves to reduce the autocorrelation between posterior samples for each parameter - overall serves to `stimulate convergence` earlier than otherwise 
             cores = 8,
             chains = 8,
             verbose = T)
# First evaluation of the model
fit_summary = summary(fit)
print(fit_summary$summary)
rstan::traceplot(fit, inc_warmup = TRUE)

fit_summary2 = summary(fit2)
print(fit_summary2$summary)
rstan::traceplot(fit2, inc_warmup = TRUE)




#### Convergence diagnostics ####
# (source: arXiv:1903.08008):
# 1. Rhat - Comparison of between chain and within chain estimations. Let's us decide whether the chains 
# have mixed properly. Is calculated for each variable. Should be around 1 (less than 1.1)
param.sims = 
  rstan::extract(
    fit,
    #pars = pars,
    permuted = TRUE, 
    inc_warmup = FALSE,
    include = TRUE)
param.sims2 = 
  rstan::extract(
    fit2,
    #pars = pars,
    permuted = TRUE, 
    inc_warmup = FALSE,
    include = TRUE)

array_of_draws <- as.array(fit)
head(array_of_draws)

# Rhat for alpha
Rhat(array_of_draws[,,1])
# sigma
Rhat(array_of_draws[,,2])
# psi_sent
Rhat(array_of_draws[,,3])
# psi_info
Rhat(array_of_draws[,,4])
#psi_pers
Rhat(array_of_draws[,,5])

# 2. ESS_tail - Bulk Effective Sample Size - measures sampling efficiency in the bulk of the distribution.
# Is useful to measure sampling of the mean/median estimations. Minimum ~ 400

# ESS bulk for Alpha
ess_bulk(array_of_draws[,,1])
# sigma
ess_bulk(array_of_draws[,,2])
# psi_sent
ess_bulk(array_of_draws[,,3])
# psi_info
ess_bulk(array_of_draws[,,4])
# psi_pers
ess_bulk(array_of_draws[,,5])

# 3. ESS_tail - Tail Effective Sample Size - calculates minimum of effective sample size for 5% and 95%
# of the sample. Measures efficiency of variance and tail quantiles. Less important - minumum ~400

# ESS tail for Alpha
ess_tail(array_of_draws[,,1])
# sigma
ess_tail(array_of_draws[,,2])
# psi_sent
ess_tail(array_of_draws[,,3])
# psi_info
ess_tail(array_of_draws[,,4])
#psi_pers
ess_tail(array_of_draws[,,5])

#### Coefficient analysis ####
#### model 1 ####
#confronting the posteriors with priors, checking whether they make sense and 
# are interpretable/ have effect on the model
plot(fit, show_density = TRUE, ci_level = 0.5, fill_color = "pink")


hist(param.sims$alpha,xlim = c(-1,1),xlab = 'alpha',main = 'posterior distirbution of\nbaseline rate of change in support')
abline(v=0, lty = 2)
legend(
  'topleft',
  legend = paste('Pr(alpha >0):',
                 round(mean(param.sims$alpha>0),3)
  )
)
median(param.sims$alpha)

# alpha has a small significant negative effect on the dependent variable 

hist(param.sims$psi_sent,
     xlim = c(-1.5,1.5),xlab = 'psi_sent',main = 'posterior distirbution of\neffect of sentiment')
abline(v=0, lty = 2)
legend(
  'topleft',
  legend = paste('Pr(psi_sent >0):',
                 round(mean(param.sims$psi_sent>0),3)
  )
)
median(param.sims$psi_sent)

# no big effect of sentiment

hist(param.sims$psi_info,
     xlim = c(-1.5,1.5),xlab = 'psi_info',main = 'posterior distirbution of\neffect of informativeness')
abline(v=0, lty = 2)
legend(
  'topleft',
  legend = paste('Pr(psi_info >0):',
                 round(mean(param.sims$psi_info>0),3)
  )
)
median(param.sims$psi_info)



hist(param.sims$psi_pers,
     xlim = c(-1.5,1.5),xlab = 'psi_pers',main = 'posterior distirbution of\neffect of persuasion')
abline(v=0, lty = 2)
legend(
  'topleft',
  legend = paste('Pr(psi_pers >0):',
                 round(mean(param.sims$psi_pers>0),3)
  )
)
median(param.sims$psi_pers)

hist(param.sims$psi_surp,
     xlim = c(-1.5,1.5),xlab = 'psi_surp',main = 'posterior distirbution of\neffect of surprise')
abline(v=0, lty = 2)
legend(
  'topleft',
  legend = paste('Pr(psi_surp >0):',
                 round(mean(param.sims$psi_surp>0),3)
  )
)
median(param.sims$psi_surp) 


hist(param.sims$psi_hap,
     xlim = c(-1.5,1.5),xlab = 'psi_surp',main = 'posterior distirbution of\neffect of happiness')
abline(v=0, lty = 2)
legend(
  'topleft',
  legend = paste('Pr(psi_hap >0):',
                 round(mean(param.sims$psi_hap>0),3)
  )
)
median(param.sims$psi_hap)



hist(param.sims$psi_ang,
     xlim = c(-1.5,1.5),xlab = 'psi_ang',main = 'posterior distirbution of\neffect of anger')
abline(v=0, lty = 2)
legend(
  'topleft',
  legend = paste('Pr(psi_ang >0):',
                 round(mean(param.sims$psi_ang>0),3)
  )
)
median(param.sims$psi_ang)

hist(param.sims$theta,
     xlim = c(-1.5,1.5),xlab = 'theta',main = 'posterior distirbution of\neffect of COVID cases control variable')
abline(v=0, lty = 2)
legend(
  'topleft',
  legend = paste('Pr(theta >0):',
                 round(mean(param.sims$theta>0),3)
  )
)
median(param.sims$theta)


hist(param.sims$beta,
     xlim = c(-1.5,1.5),xlab = 'trend.control',main = 'posterior distirbution of\neffect of 2nd difference')
abline(v=0, lty = 2)
legend(
  'topleft',
  legend = paste('Pr(beta >0):',
                 round(mean(param.sims$beta>0),3)
  )
)



for(i in 1:6){
hist(param.sims$eta_tilde[,i],
     xlim = c(-1.5,1.5),
     ylim = c(0,500),
     col = adjustcolor(i,0.2),
     xlab = 'trend.control',
     main = 'posterior distirbution of country effects')
  par(new = T)
}


#### model 1 ####
#confronting the posteriors with priors, checking whether they make sense and 
# are interpretable/ have effect on the model
plot(fit2, show_density = TRUE, ci_level = 0.5, fill_color = "pink")


hist(param.sims2$alpha,xlim = c(-1,1),xlab = 'alpha',main = 'posterior distirbution of\nbaseline rate of change in support')
abline(v=0, lty = 2)
legend(
  'topleft',
  legend = paste('Pr(alpha >0):',
                 round(mean(param.sims2$alpha>0),3)
  )
)
median(param.sims2$alpha)

# alpha has a small significant negative effect on the dependent variable 

hist(param.sims2$psi_sent,
     xlim = c(-1.5,1.5),xlab = 'psi_sent',main = 'posterior distirbution of\neffect of sentiment')
abline(v=0, lty = 2)
legend(
  'topleft',
  legend = paste('Pr(psi_sent >0):',
                 round(mean(param.sims2$psi_sent>0),3)
  )
)
median(param.sims2$psi_sent)

# no big effect of sentiment

hist(param.sims2$psi_info,
     xlim = c(-1.5,1.5),xlab = 'psi_info',main = 'posterior distirbution of\neffect of informativeness')
abline(v=0, lty = 2)
legend(
  'topleft',
  legend = paste('Pr(psi_info >0):',
                 round(mean(param.sims2$psi_info>0),3)
  )
)
median(param.sims2$psi_info)



hist(param.sims2$psi_pers,
     xlim = c(-1.5,1.5),xlab = 'psi_pers',main = 'posterior distirbution of\neffect of persuasion')
abline(v=0, lty = 2)
legend(
  'topleft',
  legend = paste('Pr(psi_pers >0):',
                 round(mean(param.sims2$psi_pers>0),3)
  )
)
median(param.sims2$psi_pers)

hist(param.sims2$psi_surp,
     xlim = c(-1.5,1.5),xlab = 'psi_surp',main = 'posterior distirbution of\neffect of surprise')
abline(v=0, lty = 2)
legend(
  'topleft',
  legend = paste('Pr(psi_surp >0):',
                 round(mean(param.sims2$psi_surp>0),3)
  )
)
median(param.sims2$psi_surp) 


hist(param.sims2$psi_hap,
     xlim = c(-1.5,1.5),xlab = 'psi_surp',main = 'posterior distirbution of\neffect of happiness')
abline(v=0, lty = 2)
legend(
  'topleft',
  legend = paste('Pr(psi_hap >0):',
                 round(mean(param.sims2$psi_hap>0),3)
  )
)
median(param.sims2$psi_hap)



hist(param.sims2$psi_ang,
     xlim = c(-1.5,1.5),xlab = 'psi_ang',main = 'posterior distirbution of\neffect of anger')
abline(v=0, lty = 2)
legend(
  'topleft',
  legend = paste('Pr(psi_ang >0):',
                 round(mean(param.sims2$psi_ang>0),3)
  )
)
median(param.sims2$psi_ang)

hist(param.sims2$theta,
     xlim = c(-1.5,1.5),xlab = 'theta',main = 'posterior distirbution of\neffect of COVID cases control variable')
abline(v=0, lty = 2)
legend(
  'topleft',
  legend = paste('Pr(theta >0):',
                 round(mean(param.sims2$theta>0),3)
  )
)
median(param.sims2$theta)


hist(param.sims2$beta,
     xlim = c(-1.5,1.5),xlab = 'trend.control',main = 'posterior distirbution of\neffect of 2nd difference')
abline(v=0, lty = 2)
legend(
  'topleft',
  legend = paste('Pr(beta >0):',
                 round(mean(param.sims2$beta>0),3)
  )
)



for(i in 1:6){
  hist(param.sims2$eta_tilde[,i],
       xlim = c(-1.5,1.5),
       ylim = c(0,500),
       col = adjustcolor(i,0.2),
       xlab = 'trend.control',
       main = 'posterior distirbution of country effects')
  par(new = T)
}












#### Posterioir predictive check ####
# plot testing dataset and measure the errors

#### Measures of fit ####

# Log density  up to a constant (https://www.jax.org/news-and-insights/jax-blog/2015/october/lp-in-stan-output)

tail(fit_summary$summary,1)

# Log likelihood
# measure of goodness of fit - probability of observing data that the model generates
log_lik_1 <- extract_log_lik(fit, merge_chains = FALSE)
log_lik_2 <- extract_log_lik(fit2, merge_chains = FALSE)

# EFF - estimate of effective number of parameters in a Bayesian model, measure of complexity
r_eff <- relative_eff(exp(log_lik_1), cores = 8) 
r_eff2 <- relative_eff(exp(log_lik_2), cores = 8) 

loo_1 <- loo(log_lik_1, r_eff = r_eff, cores = 8)
loo_2 <- loo(log_lik_2, r_eff = r_eff2, cores = 8)

plot(loo_1)

# TODO: the data seems to stop fitting after a certain point! Why that can be?
plot(loo_2)


# LOO interpretations!
# https://mc-stan.org/loo/reference/loo-glossary.html
# Estimates - Estimates and Standard Errors are provided for:
# elpd_loo - estimate of the expected log pointwise predictive density
# p_loo - effective number of parameters - how more difficult it is to predict 
# new data than the observed data
# in 'well behaving cases' - p_loo < N and <p (number of parameters)
# looic - LOO information criterion - can be used for model comparison - 
# summarizes goodness of fit across all observations - the smaller looic the better
# evaluates the balance between fit and complexity




# LOO IC + LOO R^2

# LOO Rsquared
#model 1
var_fit1 <- apply(param.sims$y_rep, 1 ,var)
var_res1 <- as.matrix(fit, pars = c("sigma"))^2
rsq1 <- var_fit1 / (var_fit1 + var_res1)

hist(rsq1, sub = paste('Rsquared model 1 mean: ',
                      round(mean(rsq1),3)))
abline(v=median(rsq), lty =2, col = 'blue')


# model 2 
var_fit2 <- apply(param.sims2$y_rep, 1 ,var)
var_res2 <- as.matrix(fit2, pars = c("sigma"))^2
rsq2 <- var_fit2 / (var_fit2 + var_res2)

hist(rsq2, sub = paste('Rsquared model 2 mean: ',
                       round(mean(rsq2),3)))
abline(v=median(rsq), lty =2, col = 'blue')




# Comparison of models -> model2 is better!
loo_compare(loo_1, loo_2)



# TODO: could apply this: https://www.projectpro.io/recipes/tune-hyper-parameters-grid-search-r

