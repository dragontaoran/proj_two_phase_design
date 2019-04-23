rm(list=ls())
gc()

# library(devtools)
# install_github("dragontaoran/TwoPhaseReg")
# library(TwoPhaseReg)
# library(TwoPhaseReg, lib.loc="/nas02/home/t/a/taor/Rcode/v11_Rlibs")
library(survival)

dat = nwtco
dat$stage2 = as.numeric(dat$stage == 2)
dat$stage3 = as.numeric(dat$stage == 3)
dat$stage4 = as.numeric(dat$stage == 4)
dat$uh = as.numeric(dat$histol == "2")
dat$luh = as.numeric(dat$instit == "2")
dat$bs1 = dat$luh
dat$bs2 = 1-dat$luh
dat$age = dat$age/12

#### full cohort
table(dat$instit, dat$rel)
res_fc = summary(coxph(Surv(edrel, rel)~uh+stage2+stage3+stage4+age, data=dat, robust=TRUE))
res_fc
