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

varx_Z0 = pX0*(1-pX0)
varx_Z1 = pX1*(1-pX1)

set.seed(seed)
setwd(wd)
sink(paste("var_beta", true_beta, "_gamma", true_gamma, "_case", true_case, ".tab", sep=""))
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

	resy = residuals(lm(simY~simZ))

    # simple random sample
    phase2_id = sample(N, n)
    
    resy.phase2 = resy[phase2_id]
    simZ.phase2 = simZ[phase2_id]
    se.phase2 = sqrt((var(resy.phase2[which(simZ.phase2 == 0)])*varx_Z0*sum(simZ.phase2==0)
                     +var(resy.phase2[which(simZ.phase2 == 1)])*varx_Z1*sum(simZ.phase2==1))^(-1))
    cat(se.phase2)
    cat('\t')

    # ODS
	order_resy = order(simY)
	phase2_id = c(order_resy[1:(n/2)], order_resy[(N-(n/2)+1):N])

	resy.phase2 = resy[phase2_id]
	simZ.phase2 = simZ[phase2_id]
	se.phase2 = sqrt((var(resy.phase2[which(simZ.phase2 == 0)])*varx_Z0*sum(simZ.phase2==0)
	                  +var(resy.phase2[which(simZ.phase2 == 1)])*varx_Z1*sum(simZ.phase2==1))^(-1))
	cat(se.phase2)
	cat('\t')

    # RDS
	order_resy = order(resy)
	phase2_id = c(order_resy[1:(n/2)], order_resy[(N-(n/2)+1):N])

	resy.phase2 = resy[phase2_id]
	simZ.phase2 = simZ[phase2_id]
	se.phase2 = sqrt((var(resy.phase2[which(simZ.phase2 == 0)])*varx_Z0*sum(simZ.phase2==0)
	                  +var(resy.phase2[which(simZ.phase2 == 1)])*varx_Z1*sum(simZ.phase2==1))^(-1))
	cat(se.phase2)
	cat('\t')

    # OPT
    resy.opt = resy
    resy.opt[indZ0] = resy.opt[indZ0]*sqrt(varx_Z0)
    resy.opt[indZ1] = resy.opt[indZ1]*sqrt(varx_Z1)
    order_resy = order(resy.opt)
	phase2_id = c(order_resy[1:(n/2)], order_resy[(N-(n/2)+1):N])

	resy.phase2 = resy[phase2_id]
	simZ.phase2 = simZ[phase2_id]
	se.phase2 = sqrt((var(resy.phase2[which(simZ.phase2 == 0)])*varx_Z0*sum(simZ.phase2==0)
	                  +var(resy.phase2[which(simZ.phase2 == 1)])*varx_Z1*sum(simZ.phase2==1))^(-1))
	cat(se.phase2)
	cat('\n')
}

sink()
