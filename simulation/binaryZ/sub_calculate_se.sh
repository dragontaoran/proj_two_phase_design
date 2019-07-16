#!/bin/bash

#### common SBATCH options
SBATCHOPT="
#SBATCH --job-name=binZ
#SBATCH --mail-type=NONE  # Alerts sent when job begins, ends, or aborts
#SBATCH --nodes=1   # Number of nodes required
#SBATCH --ntasks=1   # Number of nodes required
#SBATCH --time=01-00:00:00  # Wall Clock time (dd-hh:mm:ss) [max of 14 days]
#SBATCH --partition=lin_bat
"

#### software options
wd="/home/users/taor/Research/two_phase_GLM/design/simulation/binaryZ" 
NJOB="500"
tau_set=("0" "-0.7")
true_beta=("0" "0.3" "0.5")
true_gamma=("0" "0.5" "1")
rho="0.2"
NSIM="20"

if [ ! -d "${wd}/res" ]; then
	mkdir ${wd}/res
fi

for njob in `seq $NJOB`;
do
	if [ ! -d "${wd}/res/${njob}" ]; then
		mkdir ${wd}/res/${njob}
	fi
	cd ${wd}/res/${njob}
	
	for tau in ${tau_set[@]};
	do
		for beta in ${true_beta[@]};
		do
			for gamma in ${true_gamma[@]};
			do
				FN_slog="se_tau${tau}_beta${beta}_gamma${gamma}_rho${rho}.slog"
				FN_error="se_tau${tau}_beta${beta}_gamma${gamma}_rho${rho}.error"
				seed=$((12345+5000*$njob))
				echo "#!/bin/bash" >> tmp.out
				echo "$SBATCHOPT" >> tmp.out
				echo "#SBATCH --output=$FN_slog" >> tmp.out
				echo "#SBATCH --error=$FN_error" >> tmp.out
				echo "R CMD BATCH --vanilla --slave \"--args ${NSIM} ${tau} ${gamma} ${njob} ${wd}/res/${njob} ${seed} ${rho} ${beta}\" ${wd}/calculate_se.R se_tau${tau}_beta${beta}_gamma${gamma}_rho${rho}.Rout" >> tmp.out
				chmod 700 tmp.out
				# cat tmp.out
				sbatch tmp.out
				rm -rf tmp.out
			done
		done
	done
done

