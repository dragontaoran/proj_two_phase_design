args = commandArgs(TRUE)
true_beta = as.numeric(args[1])
true_gamma = as.numeric(args[2])
wd = args[3]
true_case = as.integer(args[4])
seed = as.integer(args[5])
prob_Z = as.numeric(args[6])

# ## RT test block ##
# true_beta = 0.2
# true_gamma = 0.3
# wd = "/nas02/home/t/a/taor/20171211_survival"
# true_case = 2
# seed = 12345
# prob_Z = 0.5
# # library(survival)
# ## RT test block ##

library(TwoPhaseReg, lib.loc="~/tmp/TwoPhaseReg_Rlibs")

#### constants ################################################################################################################
N = 2000
NSIM = 100
true_alpha = log(0.1)
weibull_k = 0.7
if (true_case == 1) {
    pX0 = pX1 = 0.7
} else if (true_case == 2) {
    pX0 = 0.5
    pX1 = 0.9
} else if (true_case == 3) {
    pX0 = 0.1
    pX1 = 0.5
}
#### constants ################################################################################################################

set.seed(seed)
setwd(wd)
file_out = paste("beta", true_beta, "_gamma", true_gamma, "_case", true_case, "_probZ", prob_Z, "_yds.tab", sep="")
sink(file_out)
cat("X\tZ\n")
sink()

# ## RT test block ##
# beta.x = rep(NA, NSIM)
# beta.z = rep(NA, NSIM)
# ## RT test block ##

nsim = 1
while (nsim <= NSIM) {
    # print(nsim)
    #### generate data ########################################################################################################
    simZ = rbinom(N, 1, prob_Z)
    
    indZ0 = which(simZ == 0)
    nZ0 = length(indZ0)
    indZ1 = which(simZ == 1)
    nZ1 = length(indZ1)
    simX = rep(NA, N)
    simX[indZ0] = rbinom(nZ0, 1, pX0)
    simX[indZ1] = rbinom(nZ1, 1, pX1)
    
    pred = true_alpha+true_beta*simX+true_gamma*simZ
    exp_pred = exp(pred)
    simU = runif(N)
    time = (-log(simU)/exp_pred)^(1/weibull_k)
    censor = runif(N, min=0, max=5)
    delta = as.numeric(time <= censor)
    time[which(delta == 0)] = censor[which(delta == 0)]
    # ## RT test block
    # res = coxph(Surv(time, delta)~simX+simZ)
    # beta.x[nsim] = res$coef[1]
    # beta.z[nsim] = res$coef[2]
    # ## RT test block
    
    Bspline_Z = cbind(as.numeric(simZ == 0), as.numeric(simZ == 1))
    colnames(Bspline_Z) = paste("bs", 1:ncol(Bspline_Z), sep="")
    dat = data.frame(time=time, delta=delta, X=simX, Z=simZ, Bspline_Z)
    #### generate data ########################################################################################################
    
    #### y-dependent sampling #################################################################################################
    time_id = which(delta == 1)
    order_time = order(time[time_id])
    control_id = which(delta == 0)
    n_control = length(control_id)
    order_control = order(time[control_id])
    phase2_id = c(time_id[order_time[1:200]], control_id[order_control[(n_control-200+1):n_control]])
    dat1 = dat
    dat1[-phase2_id,"X"] = NA
    res = smle(Y="time", Delta="delta", X="X", Z="Z", Bspline_Z=colnames(Bspline_Z), data=dat1, model="coxph", noSE=TRUE)
    #### y-dependent sampling #################################################################################################
    
    if (res$converge) {
        sink(file_out, append=TRUE)
        cat(paste(res$coefficients[1,1], res$coefficients[2,1], sep='\t'))
        cat('\n')
        sink()
    } else {
        nsim = nsim-1
    }
    nsim = nsim+1
}

# ## RT test block
# mean(beta.x)
# mean(beta.z)
# ## RT test block
