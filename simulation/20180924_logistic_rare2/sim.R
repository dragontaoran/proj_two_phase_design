args = commandArgs(TRUE)
true_beta = as.numeric(args[1])
true_gamma = as.numeric(args[2])
wd = args[3]
true_case = as.integer(args[4])
seed = as.integer(args[5])
prob_Z = as.numeric(args[6])

# # RT test block
# true_beta = 0.3
# true_gamma = 2
# true_case = 2
# prob_Z = 0.1
# # RT test block

library(TwoPhaseReg, lib.loc="~/tmp/TwoPhaseReg_Rlibs")

#### constants ################################################################################################################
dp = 0.14
N = 4000
NSIM = 100
if (true_case == 1) {
    pX0 = pX1 = 0.7
} else if (true_case == 2) {
    pX0 = 0.5
    pX1 = 0.9
} else if (true_case == 3) {
    pX0 = 0.1
    pX1 = 0.5
}
k_base = 20
#### constants ################################################################################################################

e_beta = exp(true_beta)
e_gamma = exp(true_gamma)
func_e_alpha = function(e_alpha) {
    y = e_alpha/(1+e_alpha)*(1-prob_Z)*(1-pX0)+e_alpha*e_beta/(1+e_alpha*e_beta)*(1-prob_Z)*pX0+
        e_alpha*e_gamma/(1+e_alpha*e_gamma)*prob_Z*(1-pX1)+e_alpha*e_beta*e_gamma/(1+e_alpha*e_beta*e_gamma)*prob_Z*pX1-dp
    return(y)
}
e_alpha = uniroot(func_e_alpha, interval=c(0, 1))$root
# log(e_alpha)

simY = rep(0, N)

set.seed(seed)
setwd(wd)
file_out = paste("beta", true_beta, "_gamma", true_gamma, "_case", true_case, "_probZ", prob_Z, ".tab", sep="")
sink(file_out)
cat("cc_est\tscc_est\topt_est\n")
sink()

nsim = 1
while (nsim <= NSIM) {
    
    #### generate data ########################################################################################################
    simZ = rbinom(N, 1, prob_Z)
    indZ0 = which(simZ == 0)
    nZ0 = length(indZ0)
    indZ1 = which(simZ == 1)
    nZ1 = length(indZ1)
    simX = rep(NA, N)
    simX[indZ0] = rbinom(nZ0, 1, pX0)
    simX[indZ1] = rbinom(nZ1, 1, pX1)
    pred = true_beta*simX+true_gamma*simZ
    exp_pred = exp(pred)*e_alpha
    prob = exp_pred/(1+exp_pred)
    for (i in 1:N) {
        simY[i] = rbinom(1, size=1, prob=prob[i])
    }
    # print(table(simY, simZ))
    
    if (min(table(simY, simZ)) <= 10 | length(which(simY == 1 & simZ == 1)) > length(which(simY == 0 & simZ == 1))) {
        
        next
        
    } else {
        
        Bspline_Z = cbind(as.numeric(simZ == 0), as.numeric(simZ == 1))
        colnames(Bspline_Z) = paste("bs", 1:ncol(Bspline_Z), sep="")
        dat = data.frame(Y=simY, X=simX, Z=simZ, Bspline_Z)
        #### generate data ########################################################################################################
        
        n10 = length(which(simY == 1 & simZ == 0))
        n11 = length(which(simY == 1 & simZ == 1))
        ncase = n10+n11
        
        #### case-control sampling ################################################################################################
        phase2_id = c(which(simY == 1), sample(which(simY == 0), ncase))
        # table(simY[phase2_id], simZ[phase2_id])
        dat1 = dat
        dat1[-phase2_id,"X"] = NA
        res_cc = smle(Y="Y", X="X", Z="Z", Bspline_Z=colnames(Bspline_Z), data=dat1, model="logistic", noSE=TRUE)
        #### case-control sampling ################################################################################################
        
        #### stratified case-control sampling #####################################################################################
        phase2_id = c(which(simY == 1), sample(which(simY == 0 & simZ == 0), n10), sample(which(simY == 0 & simZ == 1), n11))
        # table(simY[phase2_id], simZ[phase2_id])
        dat1 = dat
        dat1[-phase2_id,"X"] = NA
        res_scc = smle(Y="Y", X="X", Z="Z", Bspline_Z=colnames(Bspline_Z), data=dat1, model="logistic", noSE=TRUE)
        #### stratified case-control sampling #####################################################################################
        
        #### better sampling ######################################################################################################
        phase2_id = c(which(simY == 1), sample(which(simY == 0 & simZ == 0), n10), sample(which(simY == 0 & simZ == 1), n11))
        simZ.opt = simZ[phase2_id]
        simY.opt = simY[phase2_id]
        best_k = 2*n10
        best_var = var(simY.opt[which(simZ.opt == 0)])*pX0*(1-pX0)*sum(simZ.opt==0)+var(simY.opt[which(simZ.opt == 1)])*pX1*(1-pX1)*sum(simZ.opt==1)
        
        k_init = min(k_base, 2*n10)
        start = max(k_init, 2*ncase-nZ1)
        end = min(2*ncase-k_init, nZ0)
        
        for (k in start:end)
        {
            k0 = k
            if (k0 <= 2*n10) {
                tmp_phase2_id = c(sample(which(simY == 1 & simZ == 0), floor(k0/2)), sample(which(simY == 0 & simZ == 0), k0-floor(k0/2)))
            } else {
                tmp_phase2_id = c(which(simY == 1 & simZ == 0), sample(which(simY == 0 & simZ == 0), k0-n10))
            }
            
            k1 = 2*ncase-k
            if (k1 <= 2*n11) {
                tmp_phase2_id = c(tmp_phase2_id, sample(which(simY == 1 & simZ == 1), floor(k1/2)), 
                                  sample(which(simY == 0 & simZ == 1), k1-floor(k1/2)))
            } else {
                tmp_phase2_id = c(tmp_phase2_id, which(simY == 1 & simZ == 1), 
                                  sample(which(simY == 0 & simZ == 1), k1-n11))
            }
            
            simZ.opt = simZ[tmp_phase2_id]
            simY.opt = simY[tmp_phase2_id]
            tmp_var = var(simY.opt[which(simZ.opt == 0)])*pX0*(1-pX0)*sum(simZ.opt==0)+var(simY.opt[which(simZ.opt == 1)])*pX1*(1-pX1)*sum(simZ.opt==1)
            if (tmp_var > best_var) {
                best_k = k
                best_var = tmp_var
                phase2_id = tmp_phase2_id
            }
        }
        # table(simY[phase2_id], simZ[phase2_id])
        
        dat1 = dat
        dat1[-phase2_id,"X"] = NA
        res_opt = smle(Y="Y", X="X", Z="Z", Bspline_Z=colnames(Bspline_Z), data=dat1, model="logistic", noSE=TRUE)
        #### better sampling ######################################################################################################
        
        if (res_cc$converge & res_scc$converge & res_opt$converge) {
            sink(file_out, append=TRUE)
            cat(paste(res_cc$coefficients[2,1], res_scc$coefficients[2,1], res_opt$coefficient[2,1], sep='\t'))
            cat('\n')
            sink()
        } else {
            nsim = nsim-1
        }
        nsim = nsim+1
    } 
}

