#!/bin/bash

#### software options
wd="/nas02/home/t/a/taor/wilms_tumor2_v2"
subdir="res"
NSIM=10

if [ ! -d "${wd}/${subdir}" ]; then
	mkdir ${wd}/${subdir}
fi

for njob in {1..100}
do
	if [ ! -d "${wd}/${subdir}/${njob}" ]; then
		mkdir ${wd}/${subdir}/${njob}
	fi
	cd ${wd}/${subdir}/${njob}
	bsub -oo ${wd}/${subdir}/${njob}/cc.lsflog R CMD BATCH --no-save --no-restore --slave "--args ${njob} ${wd}/${subdir}/${njob} ${NSIM}" ${wd}/analysis2.cc.R
	bsub -oo ${wd}/${subdir}/${njob}/scc.lsflog R CMD BATCH --no-save --no-restore --slave "--args ${njob} ${wd}/${subdir}/${njob} ${NSIM}" ${wd}/analysis2.scc.R
	bsub -oo ${wd}/${subdir}/${njob}/ncc.lsflog R CMD BATCH --no-save --no-restore --slave "--args ${njob} ${wd}/${subdir}/${njob} ${NSIM}" ${wd}/analysis2.ncc.R
	bsub -oo ${wd}/${subdir}/${njob}/cm.lsflog R CMD BATCH --no-save --no-restore --slave "--args ${njob} ${wd}/${subdir}/${njob} ${NSIM}" ${wd}/analysis2.cm.R
done