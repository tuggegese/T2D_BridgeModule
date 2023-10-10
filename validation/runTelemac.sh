cd /home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia
cp /home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia/dicos/modified/telemac2d.dico /home/sebastian/telemac-mascaret/v8p4r0/sources/telemac2d/telemac2d.dico
export HOMETEL=/home/sebastian/telemac-mascaret/v8p4r0
export PATH=$HOMETEL/scripts/python3:.:$PATH
export SYSTELCFG=$HOMETEL/configs/systel.cfg
export USETELCFG=ubuntumpiAPI
export SOURCEFILE=$HOMETEL/configs/pysource.sh
export PYTHONUNBUFFERED='true'
export PYTHONPATH=$HOMETEL/scripts/python3:$PYTHONPATH
export LD_LIBRARY_PATH=$HOMETEL/builds/$USETELCFG/wrap_api/lib:$LD_LIBRARY_PATH
export PYTHONPATH=$HOMETEL/builds/$USETELCFG/wrap_api/lib:$PYTHONPATH
LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/sebastian/telemac-mascaret/v8p4r0/builds/ubuntumpiAPI/lib
python3 /home/sebastian/telemac-mascaret/v8p4r0/scripts/python3/runcode.py -f /home/sebastian/telemac-mascaret/v8p4r0/configs/systel.cfg -c ubuntumpiAPI -s telemac2d T2D_steering.cas --ncsize 24
cp /home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia/dicos/original/telemac2d.dico /home/sebastian/telemac-mascaret/v8p4r0/sources/telemac2d/telemac2d.dico
