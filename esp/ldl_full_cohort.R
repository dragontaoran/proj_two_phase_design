library(twophase, lib.loc="/home/users/taor/Research/two_phase_GLM/R_package/v8_Rlibs")

FN.pheno = "/home/users/taor/Research/two_phase_GLM/pheno.p1.dpr.tab"
FN.geno = "/home/users/taor/Research/two_phase_GLM/ESP_DPR/geno_maf0.05.tab"
FN.out = "/home/users/taor/Research/two_phase_GLM/design/esp/ldl_full_cohort.out"

#### read phenotype data ####
pheno = read.table(FN.pheno, sep="\t", header=T, as.is=T)
id_missing = which(is.na(pheno$esp_ldl_baseline) | is.na(pheno$esp_lipid_med_baseline) | is.na(pheno$esp_bmi_baseline))
pheno = pheno[-id_missing,]

pheno$aric = as.numeric(pheno$esp_cohort == "ARIC")
pheno$cardia = as.numeric(pheno$esp_cohort == "CARDIA")
pheno$chs = as.numeric(pheno$esp_cohort == "CHS")
pheno$fhs = as.numeric(pheno$esp_cohort == "FHS")
pheno$jhs = as.numeric(pheno$esp_cohort == "JHS")
pheno$mesa = as.numeric(pheno$esp_cohort == "MESA")
pheno$whi = as.numeric(pheno$esp_cohort == "WHI")
table(pheno$esp_cohort, useNA="ifany")
pheno$gender = as.numeric(pheno$esp_sex_selfreport == "Male")
table(pheno$esp_sex_selfreport, useNA="ifany")
pheno$logbmi = log(pheno$esp_bmi_baseline)
pheno$logldl = pheno$esp_ldl_baseline
pheno$logldl[which(pheno$esp_lipid_med_baseline == 1)] = pheno$logldl[which(pheno$esp_lipid_med_baseline == 1)]/0.75
pheno$logldl = log(pheno$logldl)
pheno$race = as.numeric(pheno$esp_race_selfreport == "European American")

covariates = c("intercept", "snp", "race", "age", "aric", "cardia", "chs", "fhs", "jhs", "mesa", "bmi") 

#### read genotype data ####
geno = read.table(FN.geno, sep="\t", as.is=T)

#### run analysis ####
sink(FN.out)
cat("snp\tbeta\tse\tpvalue\n")
sink()

#### analysis ####
for (i in 1:nrow(geno)) {

    snp = geno[i,1]

    x = as.numeric(geno[i,-1])
	x = x[-id_missing]

	res = summary(lm(pheno$logldl~x+pheno$race+pheno$esp_age_baseline+pheno$aric+pheno$cardia+pheno$chs+pheno$fhs+pheno$jhs+pheno$mesa+pheno$logbmi))
	sink(FN.out, append=TRUE)
	cat(paste(snp, res$coef[2,1], res$coef[2,2], res$coef[2,4], sep="\t"))
	cat("\n")
	sink()
}

