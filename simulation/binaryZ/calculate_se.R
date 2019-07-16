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

set.seed(seed)
setwd(wd)
sink(paste("se_tau", tau, "_beta", true_beta, "_gamma", true_gamma, "_rho", rho, ".tab", sep=""))
cat("SRS\tODS\tRDS\tOPT\tODS_SRS\tRDS_SRS\tPDS\tOPT_SRS\n")

for (nsim in 1:NSIM) {

	simZ = rbinom(N, 1, prob_Z)
	simX = rho*simZ+sqrt(1+tau*simZ)*rnorm(N)
	simY = true_beta*simX+true_gamma*simZ+rnorm(N)
	
	resy = residuals(lm(simY~simZ))
	resy2 = resy*sqrt(1+tau*simZ)

    # SRS
    phase2_id = sample(N, n)

    resy2.phase2 = resy2[phase2_id]
    simZ.phase2 = simZ[phase2_id]
    se.phase2 = sqrt((var(resy2.phase2[which(simZ.phase2 == 0)])*sum(simZ.phase2==0)
                      +var(resy2.phase2[which(simZ.phase2 == 1)])*sum(simZ.phase2==1))^(-1))
    cat(se.phase2)
    cat('\t')

    # ODS
    order_resy = order(simY)
	phase2_id = c(order_resy[1:(n/2)], order_resy[(N-(n/2)+1):N])

	resy2.phase2 = resy2[phase2_id]
	simZ.phase2 = simZ[phase2_id]
	se.phase2 = sqrt((var(resy2.phase2[which(simZ.phase2 == 0)])*sum(simZ.phase2==0)
	                  +var(resy2.phase2[which(simZ.phase2 == 1)])*sum(simZ.phase2==1))^(-1))
	cat(se.phase2)
	cat('\t')

    # RDS
	order_resy = order(resy)
	phase2_id = c(order_resy[1:(n/2)], order_resy[(N-(n/2)+1):N])

	resy2.phase2 = resy2[phase2_id]
	simZ.phase2 = simZ[phase2_id]
	se.phase2 = sqrt((var(resy2.phase2[which(simZ.phase2 == 0)])*sum(simZ.phase2==0)
	                  +var(resy2.phase2[which(simZ.phase2 == 1)])*sum(simZ.phase2==1))^(-1))
	cat(se.phase2)
	cat('\t')

    # OPT
    order_resy = order(resy2)
    phase2_id = c(order_resy[1:(n/2)], order_resy[(N-(n/2)+1):N])

    resy2.phase2 = resy2[phase2_id]
    simZ.phase2 = simZ[phase2_id]
    se.phase2 = sqrt((var(resy2.phase2[which(simZ.phase2 == 0)])*sum(simZ.phase2==0)
                      +var(resy2.phase2[which(simZ.phase2 == 1)])*sum(simZ.phase2==1))^(-1))
    cat(se.phase2)
    cat('\t')

	## designs with a SRS
    phase2_n0 = sample(N, n0)
    dat_n0 = data.frame(Y=simY[phase2_n0], X=simX[phase2_n0], Z=simZ[phase2_n0])
    dat_other = data.frame(Y=simY[-phase2_n0], Z=simZ[-phase2_n0])

    # ODS_SRS
    order_resy = order(dat_other$Y)
    phase2_id = c(phase2_n0, (1:N)[-phase2_n0][order_resy[1:n1]],
    (1:N)[-phase2_n0][order_resy[(N-n0-n1+1):(N-n0)]])

    resy2.phase2 = resy2[phase2_id]
    simZ.phase2 = simZ[phase2_id]
    se.phase2 = sqrt((var(resy2.phase2[which(simZ.phase2 == 0)])*sum(simZ.phase2==0)
                      +var(resy2.phase2[which(simZ.phase2 == 1)])*sum(simZ.phase2==1))^(-1))
    cat(se.phase2)
    cat('\t')

    # RDS_SRS
    resy.other = residuals(lm(Y~Z, data=dat_other))
    order_resy = order(resy.other)
    phase2_id = c(phase2_n0, (1:N)[-phase2_n0][order_resy[1:n1]],
        (1:N)[-phase2_n0][order_resy[(N-n0-n1+1):(N-n0)]])

    resy2.phase2 = resy2[phase2_id]
    simZ.phase2 = simZ[phase2_id]
    se.phase2 = sqrt((var(resy2.phase2[which(simZ.phase2 == 0)])*sum(simZ.phase2==0)
                      +var(resy2.phase2[which(simZ.phase2 == 1)])*sum(simZ.phase2==1))^(-1))
    cat(se.phase2)
    cat('\t')

    # PDS
    x_model0 = lm(X~Y, data=dat_n0, subset=which(dat_n0$Z == 0))
	x_model1 = lm(X~Y, data=dat_n0, subset=which(dat_n0$Z == 1))
    x_pred = rep(NA, nrow(dat_other))
    x_pred[which(dat_other$Z == 0)] = predict(x_model0, dat_other[which(dat_other$Z == 0),])
	x_pred[which(dat_other$Z == 1)] = predict(x_model1, dat_other[which(dat_other$Z == 1),])
    order_x_pred = order(x_pred)
    phase2_id = c(phase2_n0, (1:N)[-phase2_n0][order_x_pred[1:n1]],
        (1:N)[-phase2_n0][order_x_pred[(N-n0-n1+1):(N-n0)]])

    resy2.phase2 = resy2[phase2_id]
    simZ.phase2 = simZ[phase2_id]
    se.phase2 = sqrt((var(resy2.phase2[which(simZ.phase2 == 0)])*sum(simZ.phase2==0)
                      +var(resy2.phase2[which(simZ.phase2 == 1)])*sum(simZ.phase2==1))^(-1))
    cat(se.phase2)
    cat('\t')

    # OPT_SRS
    sqrt_var0 = sd(dat_n0$X[which(dat_n0$Z == 0)])
	sqrt_var1 = sd(dat_n0$X[which(dat_n0$Z == 1)])
    resy2.other = residuals(lm(Y~Z, data=dat_other))
	sqrt_var = rep(NA, nrow(dat_other))
	sqrt_var[which(dat_other$Z == 0)] = sqrt_var0
	sqrt_var[which(dat_other$Z == 1)] = sqrt_var1
	resy2.other = resy2.other*sqrt_var
    order_resy = order(resy2.other)
    phase2_id = c(phase2_n0, (1:N)[-phase2_n0][order_resy[1:n1]],
        (1:N)[-phase2_n0][order_resy[(N-n0-n1+1):(N-n0)]])

    resy2.phase2 = resy2[phase2_id]
    simZ.phase2 = simZ[phase2_id]
    se.phase2 = sqrt((var(resy2.phase2[which(simZ.phase2 == 0)])*sum(simZ.phase2==0)
                      +var(resy2.phase2[which(simZ.phase2 == 1)])*sum(simZ.phase2==1))^(-1))
    cat(se.phase2)
    cat('\n')
}

sink()

