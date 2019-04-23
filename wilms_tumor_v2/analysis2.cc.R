args = commandArgs(TRUE)
index = as.integer(args[1])
wd = args[2]
NSIM = args[3]

library(TwoPhaseReg, lib.loc="/nas02/home/t/a/taor/Rcode/v11_Rlibs")
library(survival)

seed = 12345+5000*index
set.seed(seed)
setwd(wd)
file_out = paste("cc.tab", sep="")
sink(file_out)
cat("uh_est\tuh_se\tstage2_est\tstage2_se\tstage3_est\tstage3_se\tstage4_est\tstage4_se\tage_est\tage_se\n")

dat = nwtco
dat$stage2 = as.numeric(dat$stage == 2)
dat$stage3 = as.numeric(dat$stage == 3)
dat$stage4 = as.numeric(dat$stage == 4)
dat$uh = as.numeric(dat$histol == "2")
dat$luh = as.numeric(dat$instit == "2")
dat$bs1 = dat$luh
dat$bs2 = 1-dat$luh
dat$age = dat$age/12

#### cc
for (i in 1:NSIM)
{
    dat1 = dat
    phase2 = c(sample(which(dat1$rel == 1), 200), sample(which(dat1$rel == 0), 200))
    table(dat$instit[phase2], dat$rel[phase2])
    dat1$uh[-phase2] = NA
    res_cc = smle(Y="edrel", Delta="rel", X="uh", Z=c("stage2","stage3","stage4","age"), 
                  Bspline_Z=c("bs1","bs2"), data=dat1, model="coxph")$coefficients
    cat(paste(res_cc[1,1], res_cc[1,2], 
              res_cc[2,1], res_cc[2,2],
              res_cc[3,1], res_cc[3,2],
              res_cc[4,1], res_cc[4,2],
              res_cc[5,1], res_cc[5,2], sep="\t"))
    cat("\n")
}

sink()



