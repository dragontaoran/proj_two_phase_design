gamma_set = c(0, 0.5, 1)
beta_set = c(0, 0.3, 0.5)
tau_set = c(0, -0.7)
rho = 0.2
FN_out = "summary.tab"
FN_out_bias = "summary_bias.tab"
FN_out_se = "summary_se.tab"

sink(FN_out_bias)
cat("tau\tbeta\tgamma\tSRS\tODS\tRDS\tOPS\tODS\tRDS\tPDS\tOPS\n")
sink()

sink(FN_out_se)
cat("tau\tbeta\tgamma\tSRS\tODS\tRDS\tOPS\tODS\tRDS\tPDS\tOPS\n")
sink()

sink(FN_out)
cat("tau\tbeta\tgamma\tODS\tRDS\tOPS\tODS\tRDS\tPDS\tOPS\n")
sink()

for (tau in tau_set) {
    for (beta in beta_set) {
        for (gamma in gamma_set) {
            sink(FN_out_bias, append=TRUE)
            cat(tau, '\t', beta, '\t', gamma, '\t', sep='')
            sink()
            
            sink(FN_out_se, append=TRUE)
            cat(tau, '\t', beta, '\t', gamma, '\t', sep='')
            sink()
            
            sink(FN_out, append=TRUE)
            cat(tau, '\t', beta, '\t', gamma, '\t', sep='')
            sink()
            
            res = read.table(paste("results/tau", tau, "_beta", beta, "_gamma", gamma, "_rho", rho, ".tab", sep=""),
                sep="\t", header=TRUE)
            id.exclude = c()
            for (i in 1:8) {
                id.exclude = c(id.exclude, which(res[,i] == -999.))
            }
            res = res[-id.exclude,]
            print(dim(res))
            
            sink(FN_out_bias, append=TRUE)
            cat(mean(res$SRS)-beta, mean(res$ODS)-beta, mean(res$RDS)-beta, mean(res$OPS)-beta,
                mean(res$ODS_SRS)-beta, mean(res$RDS_SRS)-beta, mean(res$PDS)-beta, mean(res$OPS_SRS)-beta, sep="\t")
			cat("\n")
			sink()
			
			sink(FN_out_se, append=TRUE)
			cat(sd(res$SRS), sd(res$ODS), sd(res$RDS), sd(res$OPS), sd(res$ODS_SRS), sd(res$RDS_SRS), sd(res$PDS), sd(res$OPS_SRS), sep="\t")
			cat("\n")
			sink()
			
			sink(FN_out, append=TRUE)
			cat((sd(res$ODS)/sd(res$SRS))^(-2),
			    (sd(res$RDS)/sd(res$SRS))^(-2),
			    (sd(res$OPS)/sd(res$SRS))^(-2),
			    (sd(res$ODS_SRS)/sd(res$SRS))^(-2),
			    (sd(res$RDS_SRS)/sd(res$SRS))^(-2),
			    (sd(res$PDS)/sd(res$SRS))^(-2),
			    (sd(res$OPS_SRS)/sd(res$SRS))^(-2), sep="\t")
			cat("\n")
			sink()
        }
    }
}
