##############################################
rm(list=ls()) # Clear the workspace
library(R2jags)
require(dplyr)
require(ggplot2)
require(GGally)
require(magrittr)
set.seed(123)

# Description of the Bayesian model fitted in this file
# Notation:
# y_ij = examination mark for observation i=1,..,N_{j} in group j = 1,..,4
# x1 = Percenatge remediated 
# x2 = Leaving Certificate Mathematics Points 
# beta0, beta1 and beta2: intercept and slope parameters to be estimated
# sigma = residual standard deviation
# sigma_QM = standard deviation for the level average quiz mark grouping

# Likelihood:
# y_ij ~ N(beta0[j] + beta1[j] * x1[i] + beta2[j] * x2[i], sigma^2)
# Prior
# beta0[j] ~ N(mu_0[j], sigma_QM^2) 
# mu_0 ~ N(0, 100^2)
# beta1[j] ~ N(mu_1[j], sigma_QM^2) 
# mu_1 ~ N(0, 100^2)
# beta2[j] ~ N(mu_2[j], sigma_QM^2) 
# mu_2 ~ N(0, 100^2)
# sigma_QM  ~ half-cauchy(0, 10)
# sigma  ~ half-cauchy(0, 10)

# Load Maths for Business Data
# This data is not provided for ethical reasons.

# Make sure each variable is of correct type
math$remPercent = as.numeric(as.character(math$remPercent)) 
math = filter(math, remPercent > -1)
math$Maths.Points = as.numeric(as.character(math$Maths.Points)) 
math$quiz.group = factor(math$quiz.group,
                              levels=c("Lower","Middle.L", "Middle.H", "Higher"))

# For convergence: subtract the mean
math$remPercent = math$remPercent - mean(math$remPercent)
math$Maths.Points = math$Maths.Points - mean(math$Maths.Points)

# Jags code to fit the model 
model_code = '
model
{
  # Likelihood
  for(t in 1:N){
  y[t] ~ dnorm(y_hat[t], sigma^-2)
  y_hat[t] = beta0[QM[t]] + (beta1[QM[t]] * x1[t]) + (beta2[QM[t]] * x2[t]) 
  }
  
  # Priors
  for(i in 1:Q){
  beta2[i] ~ dnorm(b2_h[i], sigma_QM^-2)
  beta1[i] ~ dnorm(b1_h[i], sigma_QM^-2)
  beta0[i] ~ dnorm(b0_h[i], sigma_QM^-2)
  b2_h[i] = mu.b2
  b1_h[i] = mu.b1
  b0_h[i] = mu.b0
  }
  
  sigma_QM ~  dt(0, 10^-2, 1)T(0,)
  sigma ~  dt(0, 10^-2, 1)T(0,)
  mu.b2 ~ dnorm(0, 100^-2)
  mu.b1 ~ dnorm(0, 100^-2)
  mu.b0 ~ dnorm(0, 100^-2)
  
}
'

# Set up the data
math_data = with(math,
                 list(y = exam,
                      N = nrow(math),
                      x1 = remPercent,
                      x2 = Maths.Points,
                      Q = 4, # number of quiz mark groups
                      QM = quiz.group))

# Choose the parameters to watch
model_parameters =  c("sigma_QM", "sigma", "beta0", "beta1", "beta2")

# Run the model
model_run = jags(data = math_data,
                 parameters.to.save = model_parameters,
                 model.file=textConnection(model_code),
                 n.chains=4,
                 n.iter=8000,
                 n.burnin=2000,
                 n.thin=2)

# plot(model_run)
print(model_run)    

# traceplot(model_run) 
plot(as.mcmc(model_run))

# Get the posterior samples
post = model_run$BUGSoutput$sims.list

####################################################
# Plotting the coefficient values (Figure 3 in the paper)

# Extracting the values needed
model_run.mcmc = as.mcmc(model_run)
model_run.mat = as.matrix(model_run.mcmc)
model_run.dat = as.data.frame(model_run.mat)
model_run.dat = model_run.dat[,5:8]

coef = apply(model_run.dat, 2, mean)
lower = apply(model_run.dat, 2, function(x) quantile(x, probs = c(0.025)))
upper = apply(model_run.dat, 2, function(x) quantile(x, probs = c(0.975)))
names = c("% Remediated (0-2 marks)",
          "% Remediated (2-3 marks)",
          "% Remediated (3-4 marks)",
          "% Remediated (4-5 marks)")

# Creating the dataframe to store the values
data = data.frame(coef, lower, upper, names)[c(1:4), ]

# Plotting the values
ggplot(data = data, aes(x = coef, y = names)) + 
  geom_point(aes(color="red", size=0.8)) + 
  scale_x_continuous(breaks=seq(-0.04, 0.3, 0.04)) + 
  expand_limits(y=c(-0.1,0.3)) +
  theme_bw() +
  geom_segment(aes(x = lower, xend = upper, y = names, yend = names)) +
  geom_vline(xintercept = 0, colour = "blue", linetype = 2) + 
  xlab("Posterior Estimates for the Coefficient of Percentage Remediated") + 
  ylab('Posterior\nPredicted\nValues') +
  theme(
    plot.title = element_text(lineheight=0, face="bold",size=13),
    axis.text.x = element_text(color='black', size=11),
    axis.text.y = element_text(colour = 'black', size=11),
    axis.title.x=element_text(angle=0,color='black', face='bold',size=13),
    axis.title.y=element_text(angle=0, vjust = 1, color='black',face='bold', size=13)
  ) + theme(legend.position="none")


