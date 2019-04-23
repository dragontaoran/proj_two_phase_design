#!/bin/bash

#### common SBATCH options
SBATCHOPT="
#SBATCH --job-name=survival_v3
#SBATCH --mail-type=NONE  # Alerts sent when job begins, ends, or aborts
#SBATCH --nodes=1   # Number of nodes required
#SBATCH --ntasks=1   # Number of nodes required
#SBATCH --time=01-00:00:00  # Wall Clock time (dd-hh:mm:ss) [max of 14 days]
#SBATCH --partition=general
"

#### software options
wd="/nas/longleaf/home/taor/tmp/20171211_survival_v3"
true_beta=("0" "0.3" "0.5")
true_gamma=("0" "0.5" "1")
true_case=("1" "2" "3")
prob_Z_set=("0.5")
subdir="res"
designs=("opt_fix_var")

if [ ! -d "${wd}/${subdir}" ]; then
	mkdir ${wd}/${subdir}
fi

for njob in {1..100};
do
	if [ ! -d "${wd}/${subdir}/${njob}" ]; then
		mkdir ${wd}/${subdir}/${njob}
	fi

	cd ${wd}/${subdir}/${njob}
	
	for prob_Z in ${prob_Z_set[@]};
	do
		for case in ${true_case[@]};
		do
			for beta in ${true_beta[@]};
			do
				for gamma in ${true_gamma[@]};
				do
					for design in ${designs[@]};
					do
						FN_slog="beta${beta}_gamma${gamma}_case${case}_probZ${prob_Z}_${design}.slog"
						FN_error="beta${beta}_gamma${gamma}_case${case}_probZ${prob_Z}_${design}.error"
						seed=$((12345+5000*$njob))
						
						echo "#!/bin/bash" >> tmp.out
						echo "$SBATCHOPT" >> tmp.out
						echo "#SBATCH --output=$FN_slog" >> tmp.out
						echo "#SBATCH --error=$FN_error" >> tmp.out
						
						echo "R CMD BATCH --no-save --no-restore --slave \"--args ${beta} ${gamma} ${wd}/${subdir}/${njob} ${case} ${seed} ${prob_Z}\" ${wd}/sim_${design}.R ${wd}/${subdir}/${njob}/beta${beta}_gamma${gamma}_case${case}_probZ${prob_Z}_${design}.Rout" >> tmp.out
						
						chmod 700 tmp.out
						# cat tmp.out
						sbatch tmp.out
						rm -rf tmp.out
					done
				done
			done
		done
	done
done
