# Hierarchical model
set.seed(123)

N = 1000

# True hyperparameter values
p_on = 0.1
grand_mean = 0.0
diversity = 1.0

sigma = 0.1

mus = rnorm(N, grand_mean, diversity)
mus[runif(N) > p_on] = 0.0

ys = mus + sigma*rnorm(N)

pdf("genes.pdf")
plot(mus, type="l", ylim=c(-3.5, 3.5),
        xlab="Gene", ylab="Measured Effect")
points(ys)
dev.off()

df = data.frame(y=ys)
write.csv(df, "genes.csv", row.names=FALSE)

# Classical hypothesis test of mu[i] = 0 assuming sigma=0.1
p_values = numeric(N)
for(i in 1:N)
{
    p_values[i] = 2*pnorm(-abs(ys[i]), mean=0, sd=0.1)
}

mean(p_values < 0.05)

