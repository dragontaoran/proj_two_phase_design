true_betas = c(0, 0.3, 0.5)
true_gammas = c(0, 0.5, 1)
true_cases = c(1, 2, 3)
seed = 12345
prob_Z = 0.5

#### constants ################################################################################################################
N = 2000
NSIM = 500
true_alpha = log(0.1)
weibull_k = 0.7
#### constants ################################################################################################################

set.seed(seed)

print("case beta gamma censor_rate")
for (true_beta in true_betas) {
    for (true_gamma in true_gammas) {
        for (true_case in true_cases) {
            if (true_case == 1) {
                pX0 = pX1 = 0.7
            } else if (true_case == 2) {
                pX0 = 0.5
                pX1 = 0.9
			} else if (true_case == 3) {
				pX0 = 0.1
				pX1 = 0.5
			}
            
            res_censor = rep(NA, NSIM)
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
                
                res_censor[nsim] = mean(1-delta)
                nsim = nsim+1
            }
            print(paste(true_case, true_beta, true_gamma, mean(res_censor), sep=" ")) 
        }
    }
}


