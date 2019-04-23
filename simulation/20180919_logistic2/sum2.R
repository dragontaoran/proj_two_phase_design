gamma_set = c(0, 1, 2)
beta_set = c(0, 0.3, 0.5)
case_set = c(1, 2, 3)
prob_Z_set = c(0.3, 0.5, 0.7)
FN_out = "summary.tab"
FN_out_bias = "summary_bias.tab"
FN_out_se = "summary_se.tab"
subdir = "results"

sink(FN_out_bias)
cat("\t\t\t\t$\\Exp(Z)=0.3$\t\t\t\t$\\Exp(Z)=0.5$\t\t\t\t$\\Exp(Z)=0.7$\t\t\t\n")
cat("$p_0$\t$p_1$\t$\\beta$\t$\\gamma$\tCC\tSCC\tOPT\t\tCC\tSCC\tOPT\t\tCC\tSCC\tOPT\t\n")
sink()

sink(FN_out_se)
cat("\t\t\t\t$\\Exp(Z)=0.3$\t\t\t\t$\\Exp(Z)=0.5$\t\t\t\t$\\Exp(Z)=0.7$\t\t\t\n")
cat("$p_0$\t$p_1$\t$\\beta$\t$\\gamma$\tCC\tSCC\tOPT\t\tCC\tSCC\tOPT\t\tCC\tSCC\tOPT\t\n")
sink()

sink(FN_out)
cat("\t\t\t\t$\\Exp(Z)=0.3$\t\t\t$\\Exp(Z)=0.5$\t\t\t$\\Exp(Z)=0.7$\t\t\n")
cat("$p_0$\t$p_1$\t$\\beta$\t$\\gamma$\tSCC\tOPT\t\tSCC\tOPT\t\tSCC\tOPT\t\n")
sink()


for (case in case_set) {
    
    if (case == 1) {
        pX0 = pX1 = 0.7
    } else if (case == 2) {
        pX0 = 0.5
        pX1 = 0.9
    } else if (case == 3) {
        pX0 = 0.1
        pX1 = 0.5
    }
    
    for (beta in beta_set) {
        for (gamma in gamma_set) {
            
            sink(FN_out_bias, append=TRUE)
            cat(pX0, pX1, beta, gamma, sep='\t')
            sink()
            
            sink(FN_out_se, append=TRUE)
            cat(pX0, pX1, beta, gamma, sep='\t')
            sink()
            
            sink(FN_out, append=TRUE)
            cat(pX0, pX1, beta, gamma, sep='\t')
            sink()
            
            for (prob_Z in prob_Z_set) {
                
                res = read.table(paste("./", subdir, "/beta", beta, "_gamma", gamma, "_case", case, "_probZ", prob_Z, ".tab", sep=""), sep="\t", header=TRUE)
    
    			sink(FN_out_bias, append=TRUE)
    			cat('\t')
                cat(mean(res$cc_est)-beta, mean(res$scc_est)-beta, mean(res$opt_est)-beta, sep="\t")
                cat('\t')
    			sink()
    			
    			sink(FN_out_se, append=TRUE)
    			cat('\t')
    			cat(sd(res$cc_est), sd(res$scc_est), sd(res$opt_est), sep="\t")
    			cat('\t')
    			sink()
    			
    			sink(FN_out, append=TRUE)
    			cat('\t')
                cat((sd(res$scc_est)/sd(res$cc_est))^(-2), (sd(res$opt_est)/sd(res$cc_est))^(-2), sep="\t")
                cat('\t')
    			sink()
            }
            
            sink(FN_out_bias, append=TRUE)
            cat('\n')
            sink()
            
            sink(FN_out_se, append=TRUE)
            cat('\n')
            sink()
            
            sink(FN_out, append=TRUE)
            cat('\n')
            sink()            
	    }
	}
}
