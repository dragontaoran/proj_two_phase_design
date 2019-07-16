gamma_set = c(0, 0.5, 1)
beta_set = c(0, 0.3, 0.5)
case_set = c(1, 2, 3)
FN_out = "summary_analytical.tab"

sink(FN_out)
cat("\t\t\t\tAnalytical standard error of $\\widehat{\\beta}$\t\t\t\t\tEfficiency relative to SRS\t\t\n")
cat("$p_0$\t$p_1$\t$\\beta$\t$\\gamma$\tSRS\tODS\tRDS\tOPT\t\tODS\tRDS\tOPT\n")
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
            
            sink(FN_out, append=TRUE)
            cat(pX0, pX1, beta, gamma, sep='\t')
            cat('\t')
            sink()
            
            res = read.table(paste("results/se_beta", beta, "_gamma", gamma, "_case", case, ".tab", sep=""), sep="\t", header=TRUE)
            print(dim(res))
            
            sink(FN_out, append=TRUE)
            cat(mean(res$SRS), mean(res$ODS), mean(res$RDS), mean(res$OPT), '', 
                (mean(res$SRS)/mean(res$ODS))^2, (mean(res$SRS)/mean(res$RDS))^2, (mean(res$SRS)/mean(res$OPT))^2, sep="\t")
			cat("\n")
			sink()
        }
    }
}
