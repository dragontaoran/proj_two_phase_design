rm(list=ls())
gc()

# library(devtools)
# install_github("dragontaoran/TwoPhaseReg")
# library(TwoPhaseReg)
library(TwoPhaseReg, lib.loc="/nas02/home/t/a/taor/Rcode/v11_Rlibs")
library(survival)

dat = nwtco
dat$stage2 = as.numeric(dat$stage == 2)
dat$stage3 = as.numeric(dat$stage == 3)
dat$stage4 = as.numeric(dat$stage == 4)
dat$uh = as.numeric(dat$histol == "2")
dat$luh = as.numeric(dat$instit == "2")
# dat$bs1 = as.numeric(dat$luh == 1 & dat$stage %in% c(1,2))
# dat$bs2 = as.numeric(dat$luh == 1 & dat$stage == 3)
# dat$bs3 = as.numeric(dat$luh == 1 & dat$stage == 4)
# dat$bs4 = as.numeric(dat$luh == 0 & dat$stage %in% c(1,2))
# dat$bs5 = as.numeric(dat$luh == 0 & dat$stage == 3)
# dat$bs6 = as.numeric(dat$luh == 0 & dat$stage == 4)
dat$bs1 = dat$luh
dat$bs2 = 1-dat$luh
dat$age = dat$age/12

#### yds
case_id = which(dat$rel == 1)
control_id = which(dat$rel == 0)
ncontrol = length(control_id)
o_control = order(dat$edrel[control_id])

phase2 = c(case_id, control_id[o_control[(ncontrol-571+1):ncontrol]])
table(dat$instit[phase2], dat$rel[phase2])
dat1 = dat
dat1$uh[-phase2] = NA
res = smle(Y="edrel", Delta="rel", X="uh", Z=c("luh", "stage2","stage3","stage4","age"), 
               Bspline_Z=c("bs1","bs2"), data=dat1, model="coxph", hn_scale=2)
res

