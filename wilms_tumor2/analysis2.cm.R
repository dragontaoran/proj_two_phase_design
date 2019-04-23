args = commandArgs(TRUE)
index = as.integer(args[1])
wd = args[2]
NSIM = as.integer(args[3])

library(TwoPhaseReg, lib.loc="/nas02/home/t/a/taor/Rcode/v11_Rlibs")
library(survival)

seed = 12345+5000*index
set.seed(seed)
setwd(wd)
file_out = paste("cm.tab", sep="")
sink(file_out)
cat("uh_est\tuh_se\tluh_est\tluh_se\tstage2_est\tstage2_se\tstage3_est\tstage3_se\tstage4_est\tstage4_se\tage_est\tage_se\n")

dat = nwtco
dat$stage2 = as.numeric(dat$stage == 2)
dat$stage3 = as.numeric(dat$stage == 3)
dat$stage4 = as.numeric(dat$stage == 4)
dat$uh = as.numeric(dat$histol == "2")
dat$luh = as.numeric(dat$instit == "2")
dat$bs1 = dat$luh
dat$bs2 = 1-dat$luh
dat$age = dat$age/12

#### cm
i = 1
while (i <= NSIM)
{
    dat1 = dat
    time_id = which(dat1$rel == 1)
    order_time = order(dat1$edrel[time_id])
    case_id = time_id[order_time]
    candidate_control_set = which(dat1$rel == 0)
    control_id = c()
    for (j in 1:length(case_id))
    {
        pool = which(dat1$edrel[candidate_control_set] >= dat1$edrel[case_id[j]] & dat1$luh[candidate_control_set] != dat1$luh[case_id[j]])
        if (length(pool) > 0) {
            id = sample(pool, 1)
        } else {
            id = sample(which(dat1$edrel[candidate_control_set] >= dat1$edrel[case_id[j]]), 1)
        }
        control_id = c(control_id, candidate_control_set[id])
        candidate_control_set = candidate_control_set[-id]
    }
    phase2_id = c(case_id, control_id)
    dat1$uh[-phase2_id] = NA    

    res = smle(Y="edrel", Delta="rel", X="uh", Z=c("luh","stage2","stage3","stage4","age"), 
                  Bspline_Z=c("bs1","bs2"), data=dat1, model="coxph", hn_scale=1)$coefficients
    if (sum(is.na(res[,1]))+sum(is.na(res[,2])) == 0) {
        cat(paste(res[1,1], res[1,2], 
                  res[2,1], res[2,2],
                  res[3,1], res[3,2],
                  res[4,1], res[4,2],
                  res[5,1], res[5,2],
                  res[6,1], res[6,2], sep="\t"))
        cat("\n")
        i = i+1
    }

}

sink()



