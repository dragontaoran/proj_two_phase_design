gamma_set = c(0, 0.5, 1)
beta_set = c(0, 0.3, 0.5)
case_set = c(1, 2, 3)

NJOBS = 1

dir.create("results", showWarnings=FALSE)

for (case in case_set) {
    for (beta in beta_set) {
        for (gamma in gamma_set) {
            res = read.table(paste("res/", 1, "/var_beta", beta, "_gamma",
                gamma, "_case", case, ".tab", sep=""), sep="\t", header=TRUE, as.is=TRUE)
            if (NJOBS > 1) {
                for (njob in 2:NJOBS) {
                    tmp = read.table(paste("res/", njob, "/var_beta", beta, "_gamma",
                                           gamma, "_case", case, ".tab", sep=""), sep="\t", header=TRUE, as.is=TRUE)
                    res = rbind(res, tmp)
                }               
            }
            write.table(res, file=paste("results/se_beta", beta, "_gamma",
                gamma, "_case", case, ".tab", sep=""), quote=FALSE, sep="\t", row.names=FALSE)
        }
    }
}
