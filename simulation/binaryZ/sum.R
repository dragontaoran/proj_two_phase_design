gamma_set = c(0, 0.5, 1)
beta_set = c(0, 0.3, 0.5)
tau_set = c(0, -0.7)
rho = 0.2

NJOBS = 500

dir.create("results", showWarnings=FALSE)

for (tau in tau_set) {
    for (beta in beta_set) {
        for (gamma in gamma_set) {
            res = read.table(paste("res/", 1, "/tau", tau, "_beta", beta, "_gamma",
                gamma, "_rho", rho, ".tab", sep=""), sep="\t", header=T, as.is=TRUE)
            for (njob in 2:NJOBS) {
                tmp = read.table(paste("res/", njob, "/tau", tau, "_beta", beta, "_gamma",
                    gamma, "_rho", rho, ".tab", sep=""), sep="\t", header=T, as.is=TRUE)
                res = rbind(res, tmp)
            }
            write.table(res, file=paste("results/tau", tau, "_beta", beta, "_gamma",
                gamma, "_rho", rho, ".tab", sep=""), quote=F, sep="\t", row.names=FALSE)
        }
    }
}
