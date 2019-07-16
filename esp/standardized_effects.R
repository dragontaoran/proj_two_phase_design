FN.pheno = "/home/users/taor/Research/two_phase_GLM/pheno.p1.dpr.tab"
FN.results = "/home/users/taor/Research/two_phase_GLM/design/esp/full_cohort.out"

#### read phenotype data ####
pheno = read.table(FN.pheno, sep="\t", header=T, as.is=T)
id_missing = which(is.na(pheno$esp_bmi_baseline))
pheno = pheno[-id_missing,]

pheno$logbmi = log(pheno$esp_bmi_baseline)
sd.logbmi = sd(pheno$logbmi)

FI = read.table(FN.results, header=TRUE, sep="\t", as.is=TRUE)
FI$std.beta = FI$beta/sd.logbmi
range(FI$std.beta)

png("std_beta.png", width=8, height=5, units="in", res=500)
hist(FI$std.beta, breaks=200, main="Standardized Genetic Effect", xlab="")
dev.off()



