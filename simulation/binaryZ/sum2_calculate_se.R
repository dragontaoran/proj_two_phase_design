gamma_set = c(0, 0.5, 1)
beta_set = c(0, 0.3, 0.5)
tau_set = c(0, -0.7)
rho = 0.2
FN_out_se = "summary_analytical_se.tab"

sink(FN_out_se)
cat("tau\tbeta\tgamma\tSRS\tODS\tRDS\tOPT\tODS\tRDS\tPDS\tOPT\n")

for (tau in tau_set) {
    for (beta in beta_set) {
        for (gamma in gamma_set) {
            cat(tau, '\t', beta, '\t', gamma, '\t', sep='')
            res = read.table(paste("results/se_tau", tau, "_beta", beta, "_gamma", gamma, "_rho", rho, ".tab", sep=""), sep="\t", header=TRUE)
			id.exclude = c()
			for (i in 1:ncol(res)) {
				id.exclude = c(id.exclude, which(res[,i] == -999.))	
			}
			if (length(id.exclude) > 0) {
				res = res[-id.exclude,]
			}
            cat(mean(res$SRS), mean(res$ODS), mean(res$RDS), mean(res$OPT), mean(res$ODS_SRS), mean(res$RDS_SRS), mean(res$PDS), mean(res$OPT_SRS), sep="\t")
            cat("\n")
        }
    }
}

sink()

