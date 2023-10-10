#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: sebastian gegenleithner
"""

import sys
telemac_root = '/home/sebastian/telemac-mascaret/v8p4r0'
sys.path.append(telemac_root + "/scripts/python3")
sys.path.append(telemac_root + "/scripts/python3/data_manip/formats")
from selafin import Selafin

import numpy as np
import os
import shutil
import subprocess
from matplotlib import tri

def get_water_depth(slf,frame):
    value = []
    # get all stores variables stored in Selafin objct
    var_names = [item.rstrip() for item in slf.varnames]
    # extract water depth
    value.append(slf.get_variables_at(frame,[var_names.index('WATER DEPTH')]))
    value = np.squeeze(value,axis=None)
    return value
    
def Interpolator(X,Y,triang, value):
    
    interpolator = tri.LinearTriInterpolator(triang, value)
    z_int = interpolator(X,Y)
    
    return z_int

def get_time_series_waterdepth(filepath, x, y):
    # init the selafin file
    slf = Selafin(filepath)
    # extract x and y coordinates as well as nodal connectivity
    x_mesh = slf.meshx
    y_mesh = slf.meshy
    conn = slf.ikle3
    # get timestamps and convert to frames
    times = slf.tags["times"]
    frames = range(0,len(times),1)
    
    # triangulate
    triang = tri.Triangulation(x_mesh,y_mesh,conn)

    values_time = np.zeros(len(frames))
    
    i = 0
    for frame in frames:
        value = get_water_depth(slf,frame)
        z = Interpolator(x, y,triang, value)
        values_time[i] = z
        i = i+1
        
    return values_time

class Simulation:
    # init Simulation
    def __init__(self,measurements,type,BC, basepath,name):
        # read the measurements
        self.read_meas(measurements,type,BC)
        # build outlet file
        self.build_outlet('outlet.txt')
        # build inlet file
        self.simtime = self.build_inlet('inlet.txt')
        # retrieve result filename
        self.res_file = '%s.slf' % name
        # adjust steering file
        self.adjust_steering()
        # copy files
        self.copy_static(basepath)
        
    # read the measurements
    def read_meas(self,measurements,type,BC):
        self.discharge = []
        self.us_waterlevel = []
        self.ds_waterlevel = []
        self.regime = []
        with open(measurements) as f:
            for line in f:
                if ',' + type + ',' in line and BC + '\n' in line:
                    line = line.split(',')
                    self.discharge.append(float(line[2]))
                    self.us_waterlevel.append(float(line[3]) / 1000)
                    self.ds_waterlevel.append(float(line[4]) / 1000)
                    self.regime.append(line[5])
                    
        self.discharge = np.asarray(self.discharge)
        self.us_waterlevel = np.asarray(self.us_waterlevel)
        self.ds_waterlevel = np.asarray(self.ds_waterlevel)
    # build telemac outlet file
    def build_outlet(self, filepath):
        f2 = open(filepath,'w')
        f2.write('#\n# STAGE DISCHARGE CURVE\n#\nQ(1) SL(1)\nm3/2 m\n')
        for i in range(0,len(self.ds_waterlevel)):
            f2.write(str(self.discharge[i]) + ' ' + str(self.ds_waterlevel[i]) + '\n')
        f2.write('#')
        f2.close()
    # build inlet
    def build_inlet(self, filepath):
        f2 = open(filepath,'w')
        f2.write('T Q(2)\ns m3/s\n')
        time = 0.0
        for i in range(0,len(self.discharge)):
            if i == 0:
                f2.write(str(time) + ' ' + str(self.discharge[i]) + '\n')
                f2.write(str(time + 1000.0) + ' ' + str(self.discharge[i]) + '\n')
            else:
                f2.write(str(time+1) + ' ' + str(self.discharge[i]) + '\n')
                f2.write(str(time + 1000.0) + ' ' + str(self.discharge[i]) + '\n')
            time = time + 1000
        f2.close()
        return time
    # adjust steering file
    def adjust_steering(self):
        f = open('T2D_steering_bp.cas', 'r')
        data = f.read()
        data_split = data.split('\n')
        mask = ['TIME STEP ' in x for x in data_split]
        index = [i for i, x in enumerate(mask) if x][0]
        time_step = float(data_split[index].split('=')[-1].rstrip().lstrip())
        # replace data
        data = data.replace('NNAME', self.res_file)
        self.timesteps = int(self.simtime / time_step)
        data = data.replace('NTIME', str(self.timesteps))
        f.close()
        f = open('T2D_steering.cas', 'w')
        f.write(data)
        f.close()
    # copy static files
    def copy_static(self, basepath):
        shutil.copyfile(basepath + '/bridges.i2s', 'bridges.i2s')
        shutil.copyfile(basepath + '/bridges_cut.i2s', 'bridges_cut.i2s')
    # run the simulation
    def run(self):
        process = subprocess.Popen('sh runTelemac.sh',shell=True)
        process.wait()

################ User settings ##############

measurements = r'Literature/SteadyFlowBridgeExperiments.csv'
result_file = r'results.txt'
meas_point = [8.0,0.12]

val_cases = {
        '1':{
                'name':'arc2_free',
                'basepath':'/home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia/IO/arc2_free',
                'type':'ARC2',
                'BC': 'Free'
                },
        '2':{
                'name':'arc2_weir',
                'basepath':'/home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia/IO/arc2_weir',
                'type':'ARC2',
                'BC': 'Weir'
                },
        '3':{
                'name':'deck1_free',
                'basepath':'/home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia/IO/deck1_free',
                'type':'DECK1',
                'BC': 'Free'
                },
        '4':{
                'name':'deck2_weir',
                'basepath':'/home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia/IO/deck2_weir',
                'type':'DECK2',
                'BC': 'Weir'
                },
        '5':{
                'name':'deck3_weir',
                'basepath':'/home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia/IO/deck3_weir',
                'type':'DECK3',
                'BC': 'Weir'
                },
        '6':{
                'name':'I2_free',
                'basepath':'/home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia/IO/I2_free',
                'type':'I2',
                'BC': 'Free'
                },
        '7':{
                'name':'I2_weir',
                'basepath':'/home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia/IO/I2_weir',
                'type':'I2',
                'BC': 'Weir'
                },
        '8':{
                'name':'rect1_free',
                'basepath':'/home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia/IO/rect1_free',
                'type':'RECT1',
                'BC': 'Free'
                },
        '9':{
                'name':'rect1_hor_free',
                'basepath':'/home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia/IO/rect1_hor_free',
                'type':'RECT1 HOR.',
                'BC': 'Free'
                },
        '10':{
                'name':'rect1_obl_free',
                'basepath':'/home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia/IO/rect1_obl_free',
                'type':'RECT1 OBLIQUE',
                'BC': 'Free'
                },
        '11':{
                'name':'rect1_ver_free',
                'basepath':'/home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia/IO/rect1_ver_free',
                'type':'RECT1 VER.',
                'BC': 'Free'
                },
        '12':{
                'name':'rect1_weir',
                'basepath':'/home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia/IO/rect1_weir',
                'type':'RECT1',
                'BC': 'Weir'
                },
        '13':{
                'name':'rect2_free',
                'basepath':'/home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia/IO/rect2_free',
                'type':'RECT2',
                'BC': 'Free'
                },
        '14':{
                'name':'rect2_weir',
                'basepath':'/home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia/IO/rect2_weir',
                'type':'RECT2',
                'BC': 'Weir'
                },
        '15':{
                'name':'T1_free',
                'basepath':'/home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia/IO/T1_free',
                'type':'T1',
                'BC': 'Free'
                },
        '16':{
                'name':'T1_obl_free',
                'basepath':'/home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia/IO/T1_obl_free',
                'type':'T1 OBLIQUE',
                'BC': 'Free'
                },
        '17':{
                'name':'T1_weir',
                'basepath':'/home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia/IO/T1_weir',
                'type':'T1',
                'BC': 'Weir'
                },
        '18':{
                'name':'T2_free',
                'basepath':'/home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia/IO/T2_free',
                'type':'T2',
                'BC': 'Free'
                },
        '19':{
                'name':'T2_weir',
                'basepath':'/home/sebastian/telemac-mascaret/v8p4r0/examples/ValidRatia/IO/T2_weir',
                'type':'T2',
                'BC': 'Weir'
                },
        }

run_cases = [1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19]
#run_cases = [3,4]

#############################################

# open resultfile
f = open(result_file,'w')

f.write('Num,Q,US meas,US sim,regime,type,BC\n')

sim_number = 1
for case in run_cases:
    
    print('Starting case: %s' % case)
    
    name = val_cases[str(case)]['name']
    basepath = val_cases[str(case)]['basepath']
    type = val_cases[str(case)]['type']
    bc = val_cases[str(case)]['BC']
    
    # init simulation
    sim = Simulation(measurements,type,bc,basepath,name)
    # run simulation
    sim.run()

    # # move file to basefolder
    shutil.copyfile('%s.slf' % name, basepath + '/' + '%s.slf' % name)
    os.remove('%s.slf' % name)

    # extract results
    values_time = get_time_series_waterdepth(basepath + '/%s.slf' % name,meas_point[0],meas_point[1])
    values_time = values_time[20::20]
    
    # write stuff to line in file
    for i in range(0,len(sim.discharge)):
        f.write('%s,%s,%s,%s,%s,%s,%s\n' % (sim_number, sim.discharge[i],sim.us_waterlevel[i],values_time[i],sim.regime[i],type,bc))
    
    sim_number += 1
    
    print('Finished case: %s' % case)
    
f.close()




