###
### script for parsing with ace/art
###

suffix=`date -Iminutes` 
jacy=/home/bond/svn/jacy
outdir=/home/bond/jacy


profs="mrs tc-006"
# parse a new profile
for prof in ${profs}
do
    echo Parsing ${prof}
    mkdir -p ${outdir}/${prof}
    mkprof -s ${jacy}/tsdb/skeletons/tanaka/tc-006 ${outdir}/${prof}/${suffix}
    art -a "ace -g ${jacy}/jacy.dat -n 5"  ${outdir}/${prof}/${suffix}

    python3 check.py ${outdir}/${prof}/${suffix} > ${outdir}/${prof}/${suffix}/check
done
