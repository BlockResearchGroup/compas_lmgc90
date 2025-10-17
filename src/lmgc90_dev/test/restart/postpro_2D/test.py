import os, shutil, filecmp
import computation


# generating DATBOX
import gen_sample
gen_sample.generate()

# cleaning first
for old_dir in ['POSTPRO', 'BACKUP_POSTPRO', 'OUTBOX', 'DISPLAY']:
    if os.path.isdir(old_dir):
        shutil.rmtree(old_dir)

#
# defining some variables
#

# first computation 100 steps
dt       = 1e-3
nb_steps1= 100
freq_write   = 10
freq_display = 10

hfile = 'lmgc90.h5'

computation.init(dt, hfile)
computation.compute(nb_steps1, freq_write, freq_display)
computation.finalize()


# backup of POSTPRO directory
post = './POSTPRO'
bkpp = './BACKUP_POSTPRO'
shutil.copytree( post, bkpp )


# second computation from 80 to 130 steps
nb_steps2= 50
restart_file = 8

computation.init(dt, hfile, restart_file, restart_file*freq_write+1)
computation.compute(nb_steps2, freq_write, freq_display)
computation.finalize()


# list all files in each test directory
post_list    = [ f for f in os.listdir( post ) if not f.startswith('CONTACT_FORCE_DISTRIBUTION') ]
bkpp_list    = [ f for f in os.listdir( bkpp ) if not f.startswith('CONTACT_FORCE_DISTRIBUTION') ]
post_list.sort()
bkpp_list.sort()

# regular files
for bf, pf in zip( bkpp_list, post_list ):
    # check the begining of the files:
    is_ok = True
    with open(os.path.join(bkpp,bf), 'rt') as b, open(os.path.join(post,pf), 'rt') as f:
        f_it = f.readlines()
        for lb, lf in zip( b.readlines(), f_it ):
            # checking only time column...
            is_ok = lb.split()[0] == lf.split()[0]
        assert is_ok, f"ERROR when comparing common part of files {bf}/{pf}"

        for it, lf in enumerate(f_it):
            val = float(lf.split()[0].replace('D','E'))
            cal = (it+1)*dt
            is_ok = val == cal

        assert is_ok, f"ERROR when reading new times of file {bf}: {val}!={cal}"


## treating time index files
post_list    = [ f for f in os.listdir( post ) if f.startswith('CONTACT_FORCE_DISTRIBUTION') ]
bkpp_list    = [ f for f in os.listdir( bkpp ) if f.startswith('CONTACT_FORCE_DISTRIBUTION') ]
post_list.sort()
bkpp_list.sort()

new_files = [ f"CONTACT_FORCE_DISTRIBUTION_{i:07d}.DAT" for i in range(nb_steps1+1,freq_write*restart_file+nb_steps2+1) ]
assert post_list[:nb_steps1]==bkpp_list and post_list[nb_steps1:] == new_files, f"ERROR wrong list of generated files CONTACT_FORCE_DISTRIBUTIONS.*"
