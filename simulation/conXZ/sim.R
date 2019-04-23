args = commandArgs(TRUE)

NSIM = as.integer(args[1])
tau = as.numeric(args[2])
true_gamma = as.numeric(args[3])
njob = as.integer(args[4])
wd = args[5]
seed = as.integer(args[6])
rho = as.numeric(args[7])
true_beta = as.numeric(args[8])

N = 4000
n = 400
n0 = 200
n1 = 100
prob_Z = 0.5
var_est_n = 10

library(twophase, lib.loc="/nas/longleaf/home/taor/tmp/v8_Rlibs")

set.seed(seed)
setwd(wd)
sink(paste("tau", tau, "_beta", true_beta, "_gamma", true_gamma, "_rho", rho, ".tab", sep=""))
cat("SRS\tODS\tRDS\tOPS\tODS_SRS\tRDS_SRS\tPDS\tOPS_SRS\n")

for (nsim in 1:NSIM) {

	simZ = runif(N)
	simX = rho*simZ+sqrt(1+tau*simZ)*rnorm(N)
	simY = true_beta*simX+true_gamma*simZ+rnorm(N)

    # SRS
    phase2_id = sample(N, n)

    Y = as.vector(c(simY[phase2_id], simY[-phase2_id]))
	X = as.matrix(simX[phase2_id])
	Z = as.matrix(c(simZ[phase2_id], simZ[-phase2_id]))
    res = mle1(Y=Y, X=X, Z=Z, hn=1./sqrt(N), N_SIEVE=10, noSE=TRUE)
    cat(res$theta[1], "\t", sep="")

    # ODS
    order_resy = order(simY)
	phase2_id = c(order_resy[1:(n/2)], order_resy[(N-(n/2)+1):N])

	Y = as.vector(c(simY[phase2_id], simY[-phase2_id]))
	X = as.matrix(simX[phase2_id])
	Z = as.matrix(c(simZ[phase2_id], simZ[-phase2_id]))
	res = mle1(Y=Y, X=X, Z=Z, hn=1./sqrt(N), N_SIEVE=10, noSE=TRUE)
    cat(res$theta[1], '\t', sep="")

    # RDS
    resy = residuals(lm(simY~simZ))
	order_resy = order(resy)
	phase2_id = c(order_resy[1:(n/2)], order_resy[(N-(n/2)+1):N])

	Y = as.vector(c(simY[phase2_id], simY[-phase2_id]))
	X = as.matrix(simX[phase2_id])
	Z = as.matrix(c(simZ[phase2_id], simZ[-phase2_id]))
	res = mle1(Y=Y, X=X, Z=Z, hn=1./sqrt(N), N_SIEVE=10, noSE=TRUE)
    cat(res$theta[1], '\t', sep="")

    # OPS
    resy = residuals(lm(simY~simZ))
    resy = resy*sqrt(1+tau*simZ)
    order_resy = order(resy)
    phase2_id = c(order_resy[1:(n/2)], order_resy[(N-(n/2)+1):N])

	Y = as.vector(c(simY[phase2_id], simY[-phase2_id]))
	X = as.matrix(simX[phase2_id])
	Z = as.matrix(c(simZ[phase2_id], simZ[-phase2_id]))
	res = mle1(Y=Y, X=X, Z=Z, hn=1./sqrt(N), N_SIEVE=10, noSE=TRUE)
    cat(res$theta[1], "\t", sep="")

	## designs with a SRS
    phase2_n0 = sample(N, n0)
    dat_n0 = data.frame(Y=simY[phase2_n0], X=simX[phase2_n0], Z=simZ[phase2_n0])
    dat_other = data.frame(Y=simY[-phase2_n0], Z=simZ[-phase2_n0])

    # ODS_SRS
    order_resy = order(dat_other$Y)
    phase2_id = c(phase2_n0, (1:N)[-phase2_n0][order_resy[1:n1]],
    (1:N)[-phase2_n0][order_resy[(N-n0-n1+1):(N-n0)]])

    Y = as.vector(c(simY[phase2_id], simY[-phase2_id]))
    X = as.matrix(simX[phase2_id])
	Z = as.matrix(c(simZ[phase2_id], simZ[-phase2_id]))
    res = mle1(Y=Y, X=X, Z=Z, hn=1./sqrt(N), N_SIEVE=10, noSE=TRUE)
    cat(res$theta[1], "\t",  sep="")

    # RDS_SRS
    resy = residuals(lm(Y~Z, data=dat_other))
    order_resy = order(resy)
    phase2_id = c(phase2_n0, (1:N)[-phase2_n0][order_resy[1:n1]],
        (1:N)[-phase2_n0][order_resy[(N-n0-n1+1):(N-n0)]])

    Y = as.vector(c(simY[phase2_id], simY[-phase2_id]))
    X = as.matrix(simX[phase2_id])
	Z = as.matrix(c(simZ[phase2_id], simZ[-phase2_id]))
    res = mle1(Y=Y, X=X, Z=Z, hn=1./sqrt(N), N_SIEVE=10, noSE=TRUE)
    cat(res$theta[1], "\t",  sep="")

    # PDS
    x_model = lm(X~Y+Z, data=dat_n0)
    x_pred = predict(x_model, dat_other)
    order_x_pred = order(x_pred)
    phase2_id = c(phase2_n0, (1:N)[-phase2_n0][order_x_pred[1:n1]],
        (1:N)[-phase2_n0][order_x_pred[(N-n0-n1+1):(N-n0)]])

    Y = as.vector(c(simY[phase2_id], simY[-phase2_id]))
    X = as.matrix(simX[phase2_id])
	Z = as.matrix(c(simZ[phase2_id], simZ[-phase2_id]))
    res = mle1(Y=Y, X=X, Z=Z, hn=1./sqrt(N), N_SIEVE=10, noSE=TRUE)
    cat(res$theta[1], "\t",  sep="")

    ## OPS_SRS
    # sqrt_var0 = rep(NA, var_est_n)
    # for (i in 1:(var_est_n-1)) {
    #     sqrt_var0[i] = sd(dat_n0$X[which(dat_n0$Z >= 0.1*(i-1) & dat_n0$Z < 0.1*i)])
    # }
    # sqrt_var0[var_est_n] = sd(dat_n0$X[which(dat_n0$Z >= 0.1*(var_est_n-1) & dat_n0$Z <= 0.1*var_est_n)])
	
    resy = residuals(lm(Y~Z, data=dat_other))
    
	# sqrt_var = rep(NA, nrow(dat_other))
	# for (i in 1:(var_est_n-1)) {
	#     sqrt_var[which(dat_other$Z >= 0.1*(i-1) & dat_other$Z < 0.1*i)] = sqrt_var0[i]
	# }
	# sqrt_var[which(dat_other$Z >= 0.1*(var_est_n-1) & dat_other$Z <= 0.1*var_est_n)] = sqrt_var0[var_est_n]
	# 
	# resy = resy*sqrt_var
	resy = resy*sqrt(1+tau*dat_other$Z)
    order_resy = order(resy)
    phase2_id = c(phase2_n0, (1:N)[-phase2_n0][order_resy[1:n1]],
        (1:N)[-phase2_n0][order_resy[(N-n0-n1+1):(N-n0)]])

    Y = as.vector(c(simY[phase2_id], simY[-phase2_id]))
    X = as.matrix(simX[phase2_id])
	Z = as.matrix(c(simZ[phase2_id], simZ[-phase2_id]))
    res = mle1(Y=Y, X=X, Z=Z, hn=1./sqrt(N), N_SIEVE=10, noSE=TRUE)
    cat(res$theta[1], "\n",  sep="")
}

sink()

