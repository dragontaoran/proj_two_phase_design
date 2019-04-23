rm(list=ls())
gc()

# library(devtools)
# install_github("dragontaoran/TwoPhaseReg", force=TRUE)
# library(TwoPhaseReg)
library(TwoPhaseReg, lib.loc="/nas02/home/t/a/taor/Rcode/v11_Rlibs")
library(survival)

fn_full = "tb.cohort.txt"
fn_cc = "tb.cascoh.txt"
fn_ncc = "tb.ncc.txt"
fn_scc = "tb.saRxcc.txt"
N_SIEVE = 3

fi_full = read.table(fn_full, header=TRUE, as.is=TRUE)
head(fi_full)
table(fi_full$bc)

fi_full$age.rx_10 = fi_full$age.rx/10
fi_full$logdose = log2(fi_full$dose+1)
# fi_full$age.rx_cut = cut(fi_full$age.rx, quantile(fi_full$age.rx, probs=seq(0, 1, 1/N_SIEVE)), include.lowest = TRUE)
# table(fi_full$age.rx_cut, useNA="ifany")
# for (i in 1:N_SIEVE) {
#     colid = paste("bs", i, sep="")
#     fi_full[,colid] = as.numeric(fi_full$age.rx_cut == names(table(fi_full$age.rx_cut))[i])
# }
fi_full$age.rx.s = NA
fi_full$age.rx.s[which(fi_full$age.rx < 19)] = 1
fi_full$age.rx.s[which(fi_full$age.rx >= 19 & fi_full$age.rx < 29)] = 2
fi_full$age.rx.s[which(fi_full$age.rx >= 29)] = 3
table(fi_full$age.rx.s, fi_full$bc)
for (i in 1:3) {
    colid = paste("bs", i, sep="")
    fi_full[,colid] = as.numeric(fi_full$age.rx.s == i)
}

#### full cohort
res_full = summary(coxph(Surv(age.in, age.out, bc)~logdose+age.rx_10, data=fi_full, robust=TRUE))


#### case-cohort
fi_cc = read.table(fn_cc, header=TRUE, as.is=TRUE)
head(fi_cc)
table(fi_cc$bc)
dat1 = fi_full
dat1$logdose[which(!(dat1$id %in% fi_cc$id))] = NA
sum(!is.na(dat1$logdose))
table(dat1$age.rx.s[which(!is.na(dat1$logdose))], dat1$bc[which(!is.na(dat1$logdose))])
res_cc = smle(Y="age.out", L="age.in", Delta="bc", X="logdose", Z="age.rx_10", Bspline_Z=paste("bs", 1:3, sep=""), data=dat1, model="coxph")

#### stratified case-cohort
fi_scc = read.table(fn_scc, header=TRUE, as.is=TRUE)
head(fi_scc)
table(fi_scc$bc)
dat1 = fi_full
dat1$logdose[which(!(dat1$id %in% fi_scc$id))] = NA
sum(!is.na(dat1$logdose))
table(dat1$age.rx.s[which(!is.na(dat1$logdose))], dat1$bc[which(!is.na(dat1$logdose))])
res_scc = smle(Y="age.out", L="age.in", Delta="bc", X="logdose", Z="age.rx_10", Bspline_Z=paste("bs", 1:3, sep=""), data=dat1, model="coxph")

#### optimal sampling
martingale_residual = coxph(Surv(age.in, age.out, bc)~age.rx_10, data=fi_full)$residuals
for (stratum in 1:3)
{
    id = which(fi_full$age.rx.s == stratum)
    martingale_residual[id] = martingale_residual[id]*sd(fi_full$logdose[id])
}

id1 = which(fi_full$age.rx.s == 1)
ns1 = length(id1)
order_resi1 = order(martingale_residual[id1])
id2 = which(fi_full$age.rx.s == 2)
ns2 = length(id2)
order_resi2 = order(martingale_residual[id2])
id3 = which(fi_full$age.rx.s == 3)
ns3 = length(id3)
order_resi3 = order(martingale_residual[id3])

n2 = 109
best_k1 = 10
best_k2 = 10
best_k3 = n2-best_k1-best_k2
phase2_tmp = c(id1[order_resi1[1:best_k1]], id1[order_resi1[(ns1-best_k1+1):ns1]],
               id2[order_resi2[1:best_k2]], id2[order_resi2[(ns2-best_k2+1):ns2]],
               id3[order_resi3[1:best_k3]], id3[order_resi3[(ns3-best_k3):ns3]])
mart.opt = martingale_residual[phase2_tmp]
strata.opt = fi_full$age.rx.s[phase2_tmp]
best_var = var(mart.opt[which(strata.opt == 1)])*sum(strata.opt == 1)+var(mart.opt[which(strata.opt == 2)])*sum(strata.opt == 2)+var(mart.opt[which(strata.opt == 3)])*sum(strata.opt == 3)
for (k1 in 11:89) {
    for (k2 in 11:(n2-k1-10)) {
        k3= n2-k1-k2
        phase2_tmp = c(id1[order_resi1[1:k1]], id1[order_resi1[(ns1-k1+1):ns1]],
                       id2[order_resi2[1:k2]], id2[order_resi2[(ns2-k2+1):ns2]],
                       id3[order_resi3[1:k3]], id3[order_resi3[(ns3-k3):ns3]])
        mart.opt = martingale_residual[phase2_tmp]
        strata.opt = fi_full$age.rx.s[phase2_tmp]
        tmp_var = var(mart.opt[which(strata.opt == 1)])*sum(strata.opt == 1)+var(mart.opt[which(strata.opt == 2)])*sum(strata.opt == 2)+var(mart.opt[which(strata.opt == 3)])*sum(strata.opt == 3)
        if (tmp_var > best_var) {
            best_k1 = k1
            best_k2 = k2
            best_k3 = k3
            best_var = tmp_var
        }
    }
}
phase2_opt = c(id1[order_resi1[1:best_k1]], id1[order_resi1[(ns1-best_k1+1):ns1]],
               id2[order_resi2[1:best_k2]], id2[order_resi2[(ns2-best_k2+1):ns2]],
               id3[order_resi3[1:best_k3]], id3[order_resi3[(ns3-best_k3):ns3]])
dat1 = fi_full
dat1$logdose[-phase2_opt] = NA
sum(!is.na(dat1$logdose))
table(dat1$age.rx.s[which(!is.na(dat1$logdose))], dat1$bc[which(!is.na(dat1$logdose))])
res_opt = smle(Y="age.out", L="age.in", Delta="bc", X="logdose", Z="age.rx_10", Bspline_Z=paste("bs", 1:3, sep=""), data=dat1, model="coxph")

res_full$coefficients
res_cc$coefficients
res_scc$coefficients
res_opt$coefficients
