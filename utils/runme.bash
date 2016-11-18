suffix=`date -Iminutes` 
jacy=/home/bond/svn/jacy-2016-11-08
outdir=/home/bond/jacy

# parse a new profile
prof="tc-006"
mkdir -p ${outdir}/${prof}
mkprof -s ${jacy}/tsdb/skeletons/tanaka/tc-006 ${outdir}/${prof}/${suffix}
art -a "ace -g ${jacy}/jacy.dat -n 5"  ${outdir}/${prof}/${suffix}

python3 check.py ${outdir}/${prof}/${suffix} > ${outdir}/${prof}/${suffix}/check
