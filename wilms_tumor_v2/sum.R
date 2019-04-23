NJOBS = 100
subdir = "res"

res = read.table(paste(subdir, "/", 1, "/cc.tab", sep=""), sep="\t", header=T, as.is=TRUE)
for (njob in 2:NJOBS) {
    tmp = read.table(paste(subdir, "/", njob, "/cc.tab", sep=""), sep="\t", header=T, as.is=TRUE)
    res = rbind(res, tmp)
}
dim(res)
colMeans(res, na.rm=TRUE)

res = read.table(paste(subdir, "/", 1, "/scc.tab", sep=""), sep="\t", header=T, as.is=TRUE)
for (njob in 2:NJOBS) {
    tmp = read.table(paste(subdir, "/", njob, "/scc.tab", sep=""), sep="\t", header=T, as.is=TRUE)
    res = rbind(res, tmp)
}
dim(res)
colMeans(res, na.rm=TRUE)

res = read.table(paste(subdir, "/", 1, "/ods.tab", sep=""), sep="\t", header=T, as.is=TRUE)
for (njob in 2:NJOBS) {
    tmp = read.table(paste(subdir, "/", njob, "/ods.tab", sep=""), sep="\t", header=T, as.is=TRUE)
    res = rbind(res, tmp)
}
dim(res)
colMeans(res, na.rm=TRUE)