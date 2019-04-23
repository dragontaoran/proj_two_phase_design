beta_set = c(0, 0.3, 0.5)
gamma_set = c(0, 0.5, 1)
case_set = c(1, 2, 3)
prob_Z_set = c(0.5)
design_set = c("opt_fix_var")

NJOBS = 100

subdir_o = "results"
dir.create(subdir_o, showWarnings=FALSE)
subdir = "res"

for (design in design_set) {
    for (case in case_set) {
        for (beta in beta_set) {
            for (gamma in gamma_set) {
                for (prob_Z in prob_Z_set) {
                    res = read.table(paste(subdir, "/", 1, "/beta", beta, "_gamma", gamma, "_case", case, "_probZ", prob_Z, "_", design, ".tab", sep=""), sep="\t", header=T, as.is=TRUE)
                    for (njob in 2:NJOBS) {
                        tmp = read.table(paste(subdir, "/", njob, "/beta", beta, "_gamma", gamma, "_case", case, "_probZ", prob_Z, "_", design, ".tab", sep=""), sep="\t", header=T, as.is=TRUE)
                        res = rbind(res, tmp)
                    }
                    write.table(res, file=paste(subdir_o, "/beta", beta, "_gamma",gamma, "_case", case, "_probZ", prob_Z, "_", design, ".tab", sep=""), quote=F, sep="\t", row.names=FALSE)
                }
            }
        }
    }    
}

