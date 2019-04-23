args = commandArgs(TRUE)
index = as.integer(args[1])
wd = args[2]
NSIM = args[3]

# library(devtools)
# install_github("dragontaoran/TwoPhaseReg")
# library(TwoPhaseReg)
library(TwoPhaseReg, lib.loc="/nas02/home/t/a/taor/Rcode/v11_Rlibs")
library(survival)

seed = 12345+5000*index
set.seed(seed)
setwd(wd)
file_out = paste("ods.tab", sep="")
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

#### ods
for (i in 1:NSIM)
{
    subcohort = sample(1:nrow(dat), 200)
    other = (1:nrow(dat))[-subcohort]
    time_id = other[which(dat$rel[other] == 1)]
    order_time = order(dat$edrel[time_id])
    n_event = length(time_id)
    phase2_id = c(subcohort, time_id[order_time[1:100]], time_id[order_time[(n_event-100+1):n_event]])
    dat1 = dat
    dat1[-phase2_id,"uh"] = NA
    
    res = smle(Y="edrel", Delta="rel", X="uh", Z=c("stage2","stage3","stage4","age"), 
               Bspline_Z=c("bs1","bs2"), data=dat1, model="coxph")$coefficients
    cat(paste(res[1,1], res[1,2], 
              res[2,1], res[2,2],
              res[3,1], res[3,2],
              res[4,1], res[4,2],
              res[5,1], res[5,2], sep="\t"))
    cat("\n")
}

sink()
