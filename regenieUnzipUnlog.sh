DINPUT=${1}
if ["$2"]; then DOUTPUT=${2}; else DOUTPUT=${1}; fi
FILES=$(ls ${DINPUT}/*.regenie.gz)
for file in ${FILES}
do
NAME1=$(basename ${file:0:-3})
NAME2=${DOUTPUT}'/'${NAME1}'.unloged'
echo ${NAME1}
echo ${NAME2}
gzip -d ${regenie}
awk 'BEGIN{FS=" "} {if (NR>1) {$12=(10**((-1)*$12)); print $0} else {$12="PVAL"; print $0}}' ${NAME1} > ${NAME2}
done
