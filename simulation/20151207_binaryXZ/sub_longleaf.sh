#!/bin/bash

#### common SBATCH options
SBATCHOPT="
#SBATCH --job-name=binXZ
#SBATCH --mail-type=NONE  # Alerts sent when job begins, ends, or aborts
#SBATCH --nodes=1   # Number of nodes required
#SBATCH --ntasks=1   # Number of nodes required
#SBATCH --time=01-00:00:00  # Wall Clock time (dd-hh:mm:ss) [max of 14 days]
#SBATCH --partition=general
"

#### software options
wd="/nas/longleaf/home/taor/tmp/20151207_binaryXZ" 
NJOB="1"
true_gamma="0 0.5 1"
true_beta="0.5 1"
true_case="1 2 3"
NSIM="10000"

mkdir -p $wd/res

for njob in `seq $NJOB`;
do
	mkdir $wd/res/$njob
	cd $wd/res/$njob
	
	for beta in $true_beta;
	do
		for gamma in $true_gamma;
		do
			for case in $true_case;
			do
				FN_slog="beta${beta}_gamma${gamma}_case${case}.slog"
				FN_error="beta${beta}_gamma${gamma}_case${case}.error"
				seed=$((12345+5000*$njob))
				echo "#!/bin/bash" >> tmp.out
				echo "$SBATCHOPT" >> tmp.out
				echo "#SBATCH --output=$FN_slog" >> tmp.out
				echo "#SBATCH --error=$FN_error" >> tmp.out
				echo "R CMD BATCH --no-save --no-restore --slave \"--args $NSIM $case $gamma $njob $wd/res/$njob $seed $beta\" $wd/sim.R beta${beta}_gamma${gamma}_case${case}.Rout" >> tmp.out
				chmod 700 tmp.out
				# cat tmp.out
				sbatch tmp.out
				rm -rf tmp.out
			done
		done
	done
done

