gamma_set = c(0, 0.5, 1)
beta_set = c(0, 0.3, 0.5)
case_set = c(1, 2, 3)
prob_Z_set = c(0.5)
FN_out = "summary.tab"
FN_out_bias = "summary_bias.tab"
FN_out_se = "summary_se.tab"
subdir = "results"

sink(FN_out_bias)
cat("prob_Z\tcase\tbeta\tgamma\tCC\tSCC\tNCC\tCM\tYDS\tOPT_FIX_VAR\tOPT\n")
sink()

sink(FN_out_se)
cat("prob_Z\tcase\tbeta\tgamma\tCC\tSCC\tNCC\tCM\tYDS\tOPT_FIX_VAR\tOPT\n")
sink()

sink(FN_out)
cat("prob_Z\tcase\tbeta\tgamma\tSCC\tNCC\tCM\tYDS\tOPT_FIX_VAR\tOPT\n")
sink()

for (prob_Z in prob_Z_set) {
	for (case in case_set) {
	    for (beta in beta_set) {
	        for (gamma in gamma_set) {
	            res = read.table(paste("./", subdir, "/beta", beta, "_gamma", gamma, "_case", case, "_probZ", prob_Z, "_gcc.tab", sep=""),sep="\t", header=TRUE)
	            mean_gcc = mean(res$X)
	            sd_gcc = sd(res$X)
	            
	            res = read.table(paste("./", subdir, "/beta", beta, "_gamma", gamma, "_case", case, "_probZ", prob_Z, "_scc.tab", sep=""),sep="\t", header=TRUE)
	            mean_scc = mean(res$X)
	            sd_scc = sd(res$X)
	            
	            res = read.table(paste("./", subdir, "/beta", beta, "_gamma", gamma, "_case", case, "_probZ", prob_Z, "_ncc.tab", sep=""),sep="\t", header=TRUE)
	            mean_ncc = mean(res$X)
	            sd_ncc = sd(res$X)
	            
	            res = read.table(paste("./", subdir, "/beta", beta, "_gamma", gamma, "_case", case, "_probZ", prob_Z, "_cm.tab", sep=""),sep="\t", header=TRUE)
	            mean_cm = mean(res$X)
	            sd_cm = sd(res$X)
	            
	            res = read.table(paste("./", subdir, "/beta", beta, "_gamma", gamma, "_case", case, "_probZ", prob_Z, "_yds.tab", sep=""),sep="\t", header=TRUE)
	            mean_yds = mean(res$X)
	            sd_yds = sd(res$X)
	            
	            res = read.table(paste("./", subdir, "/beta", beta, "_gamma", gamma, "_case", case, "_probZ", prob_Z, "_opt_fix_var.tab", sep=""),sep="\t", header=TRUE)
	            mean_opt_fix_var = mean(res$X)
	            sd_opt_fix_var = sd(res$X)

	            res = read.table(paste("./", subdir, "/beta", beta, "_gamma", gamma, "_case", case, "_probZ", prob_Z, "_opt.tab", sep=""),sep="\t", header=TRUE)
	            mean_opt = mean(res$X)
	            sd_opt = sd(res$X)
	            
				sink(FN_out_bias, append=TRUE)
	            cat(prob_Z, case, beta, gamma, sep='\t')
				cat('\t')
	            cat(mean_gcc-beta, mean_scc-beta, mean_ncc-beta, mean_cm-beta, mean_yds-beta, mean_opt_fix_var-beta, mean_opt-beta, sep="\t")
				cat("\n")
				sink()
				
				sink(FN_out_se, append=TRUE)
				cat(prob_Z, case, beta, gamma, sep='\t')
				cat('\t')
				cat(sd_gcc, sd_scc, sd_ncc, sd_cm, sd_yds, sd_opt_fix_var, sd_opt, sep="\t")
				cat("\n")
				sink()
				
				sink(FN_out, append=TRUE)
	            cat(prob_Z, case, beta, gamma, sep='\t')
				cat('\t')
	            cat((sd_scc/sd_gcc)^(-2), (sd_ncc/sd_gcc)^(-2), (sd_cm/sd_gcc)^(-2), (sd_yds/sd_gcc)^(-2), (sd_opt_fix_var/sd_gcc)^(-2),  (sd_opt/sd_gcc)^(-2), sep="\t")
				cat("\n")
				sink()
	        }
	    }
	}
}
