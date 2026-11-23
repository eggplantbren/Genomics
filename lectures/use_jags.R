model = "model
{
    log_sigma ~ dunif(-10, 10)
    sigma <- exp(log_sigma)

    log_diversity ~ dunif(-10, 10)
    diversity <- exp(log_diversity)

    p_effect ~ dunif(0, 1)

    for(i in 1:length(y))
    {
        effect_if_nonzero[i] ~ dnorm(0, 1/diversity^2)
        flag[i] ~ dbern(p_effect)
        mu[i] <- effect_if_nonzero[i]*flag[i]

        y[i] ~ dnorm(mu[i], 1/sigma^2)
    }
}
"

# A dataset
data = read.csv("genes.csv")

# Variables to monitor
variable_names = c("p_effect", "mu", "sigma", "diversity")

# How many proper steps?
steps = 100000

# Thinning?
thin = 10

# Import the rjags library
library("rjags")

# Create a JAGS model object
jm = jags.model(textConnection(model), data)

# Do some MCMC
results = coda.samples(jm, variable_names, steps, thin=thin)

# Extract chains as data frame
results = as.data.frame(results[[1]])
