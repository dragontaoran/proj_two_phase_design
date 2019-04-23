gamma_set = c(2)
beta_set = c(0, 0.3)
case_set = c(1, 2, 3)
prob_Z_set = c(0.1)

NJOBS = 100

subdir_o = "results"
dir.create(subdir_o, showWarnings=FALSE)
subdir = "res"

for (case in case_set) {
    for (beta in beta_set) {
        for (gamma in gamma_set) {
			for (prob_Z in prob_Z_set) {
            	res = read.table(paste(subdir, "/", 1, "/beta", beta, "_gamma", gamma, "_case", case, "_probZ", prob_Z, ".tab", sep=""), sep="\t", header=T, as.is=TRUE)
            	for (njob in 2:NJOBS) {
                	tmp = read.table(paste(subdir, "/", njob, "/beta", beta, "_gamma", gamma, "_case", case, "_probZ", prob_Z, ".tab", sep=""), sep="\t", header=T, as.is=TRUE)
                	res = rbind(res, tmp)
            	}
            	write.table(res, file=paste(subdir_o, "/beta", beta, "_gamma",gamma, "_case", case, "_probZ", prob_Z, ".tab", sep=""), quote=F, sep="\t", row.names=FALSE)
			}
        }
    }
}
