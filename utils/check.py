import sys
sys.path.insert(0,'/home/bond/svn/pydelphin-fcb/')


from delphin import itsdb
from delphin.codecs import simplemrs
from delphin.exceptions import (
    XmrsDeserializationError as XDE,
    XmrsError,
    XmrsWarning
)
import warnings

#print(sys.argv)
if len(sys.argv) == 3:
    current = sys.argv[1]
    gold = sys.argv[2]
elif len(sys.argv) == 2:
    current = sys.argv[1]
    gold = ''
else:
    print("Too many or two few arguments")

def show_cov(profile):    
    newprof = itsdb.ItsdbProfile(profile, index=False)
    newtotal = 0
    newparsed = 0
    for row in newprof.read_table('parse'):
        readings = int(row.get('readings'))
        newtotal +=1
        if readings > 0:
            newparsed +=1
            
    print("Coverage for {}:\n\t {}/{} ({}%)".format(profile,
                                                    newparsed, newtotal,
                                                    100.0 * newparsed / newtotal))

def check_mrs(profile):
    prof = itsdb.ItsdbProfile(profile, index=False)
    warnings.simplefilter('error', XmrsWarning)
    #warnings.filterwarnings('XmrsWarning')
    inp = dict()
    warned=set()
    for row in prof.read_table('item'):
        inp[row.get('i-id')] = row.get('i-input')
    for row in prof.read_table('result'):
        pid, mrs = row.get('parse-id'), row.get('mrs')
        try:
            simplemrs.loads(mrs, single=True)
        except XmrsWarning as w:
            if pid not in warned: 
                print('\nItem: {}\t{}'.format(pid,inp[pid]))
                warned.add(pid)
            print('Warning: {}!'.format(w))
    print("Bad MRS for {}:\n\t {}/{} ({}%)".format(profile,
                                                    len(warned), len(inp),
                                                    100.0 * len(warned) /  len(inp)))
        
show_cov(current)
check_mrs(current)
if gold:
    show_cov(gold)
