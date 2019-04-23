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
dat$bs1 = dat$luh
dat$bs2 = 1-dat$luh
dat$age = dat$age/12

#### opt
martingale_residual = coxph(Surv(edrel, rel)~luh+stage2+stage3+stage4+age, data=dat)$residuals

id0 = which(dat$luh == 0)
ns0 = length(id0)
id1 = which(dat$luh == 1)
ns1 = length(id1)
martingale_residual[id0] = martingale_residual[id0]*sd(dat$uh[id0])
martingale_residual[id1] = martingale_residual[id1]*sd(dat$uh[id1])
order_resi0 = order(martingale_residual[id0])
order_resi1 = order(martingale_residual[id1])

n2 = 200
best_k0 = 30
best_k1 = n2-best_k0
phase2_tmp = c(id0[order_resi0[1:best_k0]], id0[order_resi0[(ns0-best_k0+1):ns0]],
               id1[order_resi1[1:best_k1]], id1[order_resi1[(ns1-best_k1+1):ns1]])
mart.opt = martingale_residual[phase2_tmp]
strata.opt = dat$luh[phase2_tmp]
best_var = var(mart.opt[which(strata.opt == 0)])*sum(strata.opt == 0)+var(mart.opt[which(strata.opt == 1)])*sum(strata.opt == 1)
for (k0 in 31:170) {
    k1= n2-k0
    phase2_tmp = c(id0[order_resi0[1:k0]], id0[order_resi0[(ns0-k0+1):ns0]],
                   id1[order_resi1[1:k1]], id1[order_resi1[(ns1-k1+1):ns1]])
    mart.opt = martingale_residual[phase2_tmp]
    strata.opt = dat$luh[phase2_tmp]
    tmp_var = var(mart.opt[which(strata.opt == 0)])*sum(strata.opt == 0)+var(mart.opt[which(strata.opt == 1)])*sum(strata.opt == 1)
    if (tmp_var > best_var) {
        best_k0 = k0
        best_k1 = k1
        best_var = tmp_var
    }
}
phase2 = c(id0[order_resi0[1:best_k0]], id0[order_resi0[(ns0-best_k0+1):ns0]],
           id1[order_resi1[1:best_k1]], id1[order_resi1[(ns1-best_k1+1):ns1]])
table(dat$instit[phase2], dat$rel[phase2])
dat1 = dat
dat1$uh[-phase2] = NA
res_opt = smle(Y="edrel", Delta="rel", X="uh", Z=c("luh", "stage2","stage3","stage4","age"), 
               Bspline_Z=c("bs1","bs2"), data=dat1, model="coxph")
res_opt

