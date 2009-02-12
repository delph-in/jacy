

#lkbdir=/home/bond/delphin/lkb


lkbdir=${LOGONROOT}/lingo/lkb
#
# JACY
#
grammardir=${LOGONROOT}/dfki/jacy; 
grammarurl=http://wiki.delph-in.net/moin/JacyTop
ltypes=("${grammardir}/lex-types.tdl $grammardir/v-lex-types.tdl")
#
# GG
#
#grammardir=${LOGONROOT}/dfki/gg; 
#grammarurl=http://gg.opendfki.de/
#ltypes=("${grammardir}/le-types.tdl")
#
# SRG
#
#grammardir=${LOGONROOT}/upf/srg; 
#ltypes=("${grammardir}/letypes.tdl")
#
# ERG
#
#grammardir=${LOGONROOT}/lingo/terg; 
#ltypes=("$grammardir/letypes.tdl" "$grammardir/lextypes.tdl")
#
# KRG
#
#grammardir=${LOGONROOT}/khu/krg; 
#ltypes=("$grammardir/letypes.tdl")
#
# Norsource
#
#grammardir=${LOGONROOT}/ntnu/norsource; 
#ltypes=("$grammardir/letypes.tdl")

#ltypes=`ls -d ${grammardir}/*.tdl` ### check them all!
treebanks=`ls -d ${grammardir}/gold/*`
now=`date --rfc-3339=date`

### Constants
LTDB_FILE="lt.db"
LINGUISTICS_FILE="linguistics.xml"
TYPES_FILE="types.xml"
LEXICON_FILE="lex.tab"
TB_FILE="result"



### I really don't want to do this!
if [ -f  $grammardir/Version.lsp ]; then
    versionfile=$grammardir/Version.lsp
else
    versionfile=$grammardir/Version.lisp
fi

version=`perl -ne 'if (/^\(defparameter\s+\*grammar-version\*\s+\"(.*)\s+\((.*)\)\"/) {print "$1_$2"}' $versionfile`
if [ -z "$version" ]; then
    echo "Don't know the version, will use 'something'"
    version=something
fi
outdir=$PWD/$version

HTML_DIR=$HOME/public_html/ltdb/$version
CGI_DIR=$HOME/public_html/cgi-bin/$version