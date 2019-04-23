args = commandArgs(TRUE)

NSIM = as.integer(args[1])
true_case = as.integer(args[2])
true_gamma = as.numeric(args[3])
njob = as.integer(args[4])
wd = args[5]
seed = as.integer(args[6])
true_beta = as.numeric(args[7])

N = 4000
n = 400
prob_Z = 0.5
if (true_case == 1) {
    pX0 = pX1 = 0.7
} else if (true_case == 2) {
    pX0 = 0.5
    pX1 = 0.9
} else if (true_case == 3) {
    pX0 = 0.1
    pX1 = 0.5
}

# library(twophase, lib.loc="/home/users/taor/Research/two_phase_GLM/R_package/v8_Rlibs")
library(twophase, lib.loc="/nas/longleaf/home/taor/tmp/v8_Rlibs")

set.seed(seed)
setwd(wd)
sink(paste("beta", true_beta, "_gamma", true_gamma, "_case", true_case, ".tab", sep=""))
cat("SRS\tODS\tRDS\tOPT\n")

for (nsim in 1:NSIM) {

    # generate data
    simZ = rbinom(N, 1, prob_Z)
    indZ0 = which(simZ == 0)
    nZ0 = length(indZ0)
    indZ1 = which(simZ == 1)
    nZ1 = length(indZ1)
    simX = rep(NA, N)
    simX[indZ0] = rbinom(nZ0, 1, pX0)
    simX[indZ1] = rbinom(nZ1, 1, pX1)
	simY = true_beta*simX+true_gamma*simZ+rnorm(N)

    # simple random sample
    phase2_id = sample(N, n)

    Y = as.vector(c(simY[phase2_id], simY[-phase2_id]))
	X = as.matrix(simX[phase2_id])
	Z = as.matrix(c(simZ[phase2_id], simZ[-phase2_id]))
    res = mle1(Y=Y, X=X, Z=Z, hn=1./sqrt(N), N_SIEVE=100)
    cat(res$theta[1])
    cat('\t')

    # ODS
	order_resy = order(simY)
	phase2_id = c(order_resy[1:(n/2)], order_resy[(N-(n/2)+1):N])

	Y = as.vector(c(simY[phase2_id], simY[-phase2_id]))
	X = as.matrix(simX[phase2_id])
	Z = as.matrix(c(simZ[phase2_id], simZ[-phase2_id]))
	res = mle1(Y=Y, X=X, Z=Z, hn=1./sqrt(N), N_SIEVE=100, noSE=TRUE)
    cat(res$theta[1])
    cat('\t')

    # RDS
	resy = residuals(lm(simY~simZ))
	order_resy = order(resy)
	phase2_id = c(order_resy[1:(n/2)], order_resy[(N-(n/2)+1):N])

	Y = as.vector(c(simY[phase2_id], simY[-phase2_id]))
	X = as.matrix(simX[phase2_id])
	Z = as.matrix(c(simZ[phase2_id], simZ[-phase2_id]))
	res = mle1(Y=Y, X=X, Z=Z, hn=1./sqrt(N), N_SIEVE=100, noSE=TRUE)
	cat(res$theta[1])
	cat('\t')

    # OPT
    resy = residuals(lm(simY~simZ))
    resy[which(simZ==0)] = resy[which(simZ==0)]*sqrt(pX0*(1-pX0))
    resy[which(simZ==1)] = resy[which(simZ==1)]*sqrt(pX1*(1-pX1))
    order_resy = order(resy)
	phase2_id = c(order_resy[1:(n/2)], order_resy[(N-(n/2)+1):N])

	Y = as.vector(c(simY[phase2_id], simY[-phase2_id]))
	X = as.matrix(simX[phase2_id])
	Z = as.matrix(c(simZ[phase2_id], simZ[-phase2_id]))
	res = mle1(Y=Y, X=X, Z=Z, hn=1./sqrt(N), N_SIEVE=100, noSE=TRUE)
    cat(res$theta[1])
    cat('\n')
}

sink()
