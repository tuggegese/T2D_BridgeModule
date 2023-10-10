#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu May 11 06:23:47 2023

@author: iwbworkstation
"""

import matplotlib.pyplot as plt
import numpy as np

def read_results(measurements):
    discharge = []
    us_meas = []
    us_sim = []
    state = []
    type = []
    bc = []
    
    with open(measurements) as f:
        for line in f:
            try:
                line=line.strip()
                line = line.split(',')
                discharge.append(float(line[1]))
                us_meas.append(float(line[2]))
                us_sim.append(float(line[3]))
                state.append(line[4])
                type.append(line[5])
                bc.append(line[6])
            except:
                continue
            
    return discharge, us_meas, us_sim, state, type, bc

def create_subset(state,bc,us_meas,us_sim,des_state,des_bc):
    
    us_meas_list = []
    us_sim_list = []
    
    for i in range(0,len(state)):
        if state[i] == des_state:
            if bc[i] == des_bc:
                 us_meas_list.append(us_meas[i])
                 us_sim_list.append(us_sim[i])
                
    return us_meas_list, us_sim_list
    
# extract results
res_file = r'results.txt'
discharge, us_meas, us_sim, state, type, bc = read_results(res_file)

# total cases plot
all_measured = np.asarray(us_meas)
all_simulated = np.asanyarray(us_sim)
correlation_matrix = np.corrcoef(all_measured, all_simulated)
correlation = correlation_matrix[0, 1]
# Compute R-squared
r_squared = correlation**2
print('Validated bridge module with a total of %s cases' % (len(all_measured)))
print('R2 = ' + str(r_squared))
# Plotting
fig, ax = plt.subplots(figsize = (10,10))
ax.plot([0,0.13],[0,0.13], color = 'k')
ax.scatter(all_measured,all_simulated,color = 'b', marker = 'x')
ax.grid()
ax.set_aspect('equal')
ax.tick_params(axis='both', which='major', labelsize=14)
ax.tick_params(axis='both', which='minor', labelsize=14)
ax.text(0,0.121, 'R² = ' + "%.3f" % r_squared, fontsize=16)
ax.set_xlabel(u'$h_{measured}$ in m', fontsize = 16)
ax.set_ylabel(u'$h_{simulated}$ in m', fontsize = 16)
plt.tight_layout()
fig.savefig('validationplot.png', dpi = 400)

# color coded by class
fig, ax = plt.subplots(figsize = (10,10))
ax.plot([0,0.1],[0,0.1], color = 'k')

us_meas1, us_sim1 = create_subset(state, bc, us_meas, us_sim, 'Free water surface', 'Free')
ax.scatter(us_meas1,us_sim1,color = 'b', marker = 'x', label = 'Free water surface')

us_meas1, us_sim1 = create_subset(state, bc, us_meas, us_sim, 'Partially submerged', 'Free')
ax.scatter(us_meas1,us_sim1,color = 'g', marker = 's',facecolor='none', label = 'Partially submerged')

us_meas1, us_sim1 = create_subset(state, bc, us_meas, us_sim, 'Fully submerged', 'Free')
ax.scatter(us_meas1,us_sim1,color = 'r', marker = 'D',facecolor='none', label = 'Fully submerged')

ax.grid()
ax.legend(loc="lower right", prop={'size': 16})
ax.set_aspect('equal')
ax.tick_params(axis='both', which='major', labelsize=14)
ax.tick_params(axis='both', which='minor', labelsize=14)
ax.set_xlabel(u'$h_{measured}$ in m', fontsize = 16)
ax.set_ylabel(u'$h_{simulated}$ in m', fontsize = 16)
plt.tight_layout()
fig.savefig('free_BC.png', dpi = 400)

# color coded by class
fig, ax = plt.subplots(figsize = (10,10))
ax.plot([0.045,0.13],[0.045,0.13], color = 'k')

us_meas1, us_sim1 = create_subset(state, bc, us_meas, us_sim, 'Free water surface', 'Weir')
ax.scatter(us_meas1,us_sim1,color = 'b', marker = 'x', label = 'Free water surface')

us_meas1, us_sim1 = create_subset(state, bc, us_meas, us_sim, 'Partially submerged', 'Weir')
ax.scatter(us_meas1,us_sim1,color = 'g', marker = 's',facecolor='none', label = 'Partially submerged')

us_meas1, us_sim1 = create_subset(state, bc, us_meas, us_sim, 'Fully submerged', 'Weir')
ax.scatter(us_meas1,us_sim1,color = 'r', marker = 'D',facecolor='none', label = 'Fully submerged')

ax.grid()
ax.legend(loc="lower right", prop={'size': 16})
ax.set_aspect('equal')
ax.tick_params(axis='both', which='major', labelsize=14)
ax.tick_params(axis='both', which='minor', labelsize=14)
ax.set_xlabel(u'$h_{measured}$ in m', fontsize = 16)
ax.set_ylabel(u'$h_{simulated}$ in m', fontsize = 16)
plt.tight_layout()
fig.savefig('weir_BC.png', dpi = 400)

"""


class bridge:
    # init the bridge
    def __init__(self,measurements,type,BC):
        self.read_meas(measurements,type,BC)
    # read the measurements
    def read_meas(self,measurements,type,BC):
        self.discharge = []
        self.us_waterlevel = []
        self.ds_waterlevel = []
        with open(measurements) as f:
            for line in f:
                if ',' + type + ',' in line and BC + '\n' in line:
                    line = line.split(',')
                    self.discharge.append(float(line[2]))
                    self.us_waterlevel.append(float(line[3]) / 1000)
                    self.ds_waterlevel.append(float(line[4]) / 1000)

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

def read_numerics(filename):

    endheader = False
    all_vals = []
    with open (filename) as f:
        for line in f:
            line = line.split()[0]
            if endheader:
                value = float(line)
                all_vals.append(value)
            if line == ':EndHeader':
                endheader = True
    
    stable_vals = []
    for i in range(0,len(all_vals)-2):
        if all_vals[i] == all_vals[i+1]:
            if all_vals[i] == all_vals[i+2]:
                if all_vals[i] not in stable_vals:
                    stable_vals.append(all_vals[i])
                    
    return np.asarray(stable_vals)


measurements = r'Literature/SteadyFlowBridgeExperiments.csv'

all_measured = []
all_simulated = []
r_squred_list = []
#####
# Bridge rect 1 free
#####
rect1 = bridge(measurements, 'RECT1','Free')
numerics = 'IO/rect1_free/rect1_free.ts1'
stable_vals = read_numerics(numerics)

# Calculate correlation coefficient
correlation_matrix = np.corrcoef(rect1.us_waterlevel, stable_vals)
correlation = correlation_matrix[0, 1]
# Compute R-squared
r_squared = correlation**2
r_squred_list.append(r_squared)
[all_measured.append(x) for x in rect1.us_waterlevel]
[all_simulated.append(x) for x in stable_vals]


#####
# Bridge rect 2 free
#####
rect1 = bridge(measurements, 'RECT2','Free')
numerics = 'IO/rect2_free/rect2_free.ts1'
stable_vals = read_numerics(numerics)

# Calculate correlation coefficient
correlation_matrix = np.corrcoef(rect1.us_waterlevel, stable_vals)
correlation = correlation_matrix[0, 1]
# Compute R-squared
r_squared = correlation**2
r_squred_list.append(r_squared)
[all_measured.append(x) for x in rect1.us_waterlevel]
[all_simulated.append(x) for x in stable_vals]

#####
# Bridge rect 2 weir
#####
rect1 = bridge(measurements, 'RECT2','Weir')
numerics = 'IO/rect2_weir/rect2_weir.ts1'
stable_vals = read_numerics(numerics)

# Calculate correlation coefficient
correlation_matrix = np.corrcoef(rect1.us_waterlevel, stable_vals)
correlation = correlation_matrix[0, 1]
# Compute R-squared
r_squared = correlation**2
r_squred_list.append(r_squared)
[all_measured.append(x) for x in rect1.us_waterlevel]
[all_simulated.append(x) for x in stable_vals]

#####
# Bridge rect 1 weir
#####
rect1 = bridge(measurements, 'RECT1','Weir')
numerics = 'IO/rect1_weir/rect1_weir.ts1'
stable_vals = read_numerics(numerics)

# Calculate correlation coefficient
correlation_matrix = np.corrcoef(rect1.us_waterlevel, stable_vals)
correlation = correlation_matrix[0, 1]
# Compute R-squared
r_squared = correlation**2
r_squred_list.append(r_squared)
[all_measured.append(x) for x in rect1.us_waterlevel]
[all_simulated.append(x) for x in stable_vals]

#####
# Bridge t1 free
#####
rect1 = bridge(measurements, 'T1','Free')
numerics = 'IO/T1_free/t1_free.ts1'
stable_vals = read_numerics(numerics)

# Calculate correlation coefficient
correlation_matrix = np.corrcoef(rect1.us_waterlevel, stable_vals)
correlation = correlation_matrix[0, 1]
# Compute R-squared
r_squared = correlation**2
r_squred_list.append(r_squared)
[all_measured.append(x) for x in rect1.us_waterlevel]
[all_simulated.append(x) for x in stable_vals]

#####
# Bridge t1 free
#####
rect1 = bridge(measurements, 'T1','Weir')
numerics = 'IO/T1_weir/t1_weir.ts1'
stable_vals = read_numerics(numerics)

# Calculate correlation coefficient
correlation_matrix = np.corrcoef(rect1.us_waterlevel, stable_vals)
correlation = correlation_matrix[0, 1]
# Compute R-squared
r_squared = correlation**2
r_squred_list.append(r_squared)
[all_measured.append(x) for x in rect1.us_waterlevel]
[all_simulated.append(x) for x in stable_vals]

#####
# Bridge t2 free
#####
rect1 = bridge(measurements, 'T2','Free')
numerics = 'IO/T2_free/t2_free.ts1'
stable_vals = read_numerics(numerics)

# Calculate correlation coefficient
correlation_matrix = np.corrcoef(rect1.us_waterlevel, stable_vals)
correlation = correlation_matrix[0, 1]
# Compute R-squared
r_squared = correlation**2
r_squred_list.append(r_squared)
[all_measured.append(x) for x in rect1.us_waterlevel]
[all_simulated.append(x) for x in stable_vals]

#####
# Bridge t2 weir
#####
rect1 = bridge(measurements, 'T2','Weir')
numerics = 'IO/T2_weir/t2_weir.ts1'
stable_vals = read_numerics(numerics)

# Calculate correlation coefficient
correlation_matrix = np.corrcoef(rect1.us_waterlevel, stable_vals)
correlation = correlation_matrix[0, 1]
# Compute R-squared
r_squared = correlation**2
r_squred_list.append(r_squared)
[all_measured.append(x) for x in rect1.us_waterlevel]
[all_simulated.append(x) for x in stable_vals]

#####
# Arc2 free
#####
rect1 = bridge(measurements, 'ARC2','Free')
numerics = 'IO/arc2_free/arc2_free.ts1'
stable_vals = read_numerics(numerics)

# Calculate correlation coefficient
correlation_matrix = np.corrcoef(rect1.us_waterlevel, stable_vals)
correlation = correlation_matrix[0, 1]
# Compute R-squared
r_squared = correlation**2
r_squred_list.append(r_squared)
[all_measured.append(x) for x in rect1.us_waterlevel]
[all_simulated.append(x) for x in stable_vals]

#####
# I2 free
#####
rect1 = bridge(measurements, 'I2','Free')
numerics = 'IO/I2_free/I2_free.ts1'
stable_vals = read_numerics(numerics)

# Calculate correlation coefficient
correlation_matrix = np.corrcoef(rect1.us_waterlevel, stable_vals)
correlation = correlation_matrix[0, 1]
# Compute R-squared
r_squared = correlation**2
r_squred_list.append(r_squared)
[all_measured.append(x) for x in rect1.us_waterlevel]
[all_simulated.append(x) for x in stable_vals]

#####
# Deck1 free
#####
rect1 = bridge(measurements, 'DECK1','Free')
numerics = 'IO/deck1_free/deck1_free.ts1'
stable_vals = read_numerics(numerics)

# Calculate correlation coefficient
correlation_matrix = np.corrcoef(rect1.us_waterlevel, stable_vals)
correlation = correlation_matrix[0, 1]
# Compute R-squared
r_squared = correlation**2
r_squred_list.append(r_squared)
[all_measured.append(x) for x in rect1.us_waterlevel]
[all_simulated.append(x) for x in stable_vals]

#####
# Deck2 weir
#####
rect1 = bridge(measurements, 'DECK2','Weir')
numerics = 'IO/deck2_weir/deck2_weir.ts1'
stable_vals = read_numerics(numerics)

# Calculate correlation coefficient
correlation_matrix = np.corrcoef(rect1.us_waterlevel, stable_vals)
correlation = correlation_matrix[0, 1]
# Compute R-squared
r_squared = correlation**2
r_squred_list.append(r_squared)
[all_measured.append(x) for x in rect1.us_waterlevel]
[all_simulated.append(x) for x in stable_vals]

#####
# Deck3 weir
#####
rect1 = bridge(measurements, 'DECK3','Weir')
numerics = 'IO/deck3_weir/deck3_weir.ts1'
stable_vals = read_numerics(numerics)

# Calculate correlation coefficient
correlation_matrix = np.corrcoef(rect1.us_waterlevel, stable_vals)
correlation = correlation_matrix[0, 1]
# Compute R-squared
r_squared = correlation**2
r_squred_list.append(r_squared)
[all_measured.append(x) for x in rect1.us_waterlevel]
[all_simulated.append(x) for x in stable_vals]

#####
# Rect1 hor free
#####
rect1 = bridge(measurements, 'RECT1 HOR.','Free')
numerics = 'IO/rect1_hor_free/rect1_hor_free.ts1'
stable_vals = read_numerics(numerics)

# Calculate correlation coefficient
correlation_matrix = np.corrcoef(rect1.us_waterlevel, stable_vals)
correlation = correlation_matrix[0, 1]
# Compute R-squared
r_squared = correlation**2
r_squred_list.append(r_squared)
[all_measured.append(x) for x in rect1.us_waterlevel]
[all_simulated.append(x) for x in stable_vals]

#####
# Rect1 ver free
#####
rect1 = bridge(measurements, 'RECT1 VER.','Free')
numerics = 'IO/rect1_ver_free/rect1_ver_free.ts1'
stable_vals = read_numerics(numerics)

# Calculate correlation coefficient
correlation_matrix = np.corrcoef(rect1.us_waterlevel, stable_vals)
correlation = correlation_matrix[0, 1]
# Compute R-squared
r_squared = correlation**2
r_squred_list.append(r_squared)
[all_measured.append(x) for x in rect1.us_waterlevel]
[all_simulated.append(x) for x in stable_vals]

#####
# T1 obl free
#####
rect1 = bridge(measurements, 'T1 OBLIQUE','Free')
numerics = 'IO/T1_obl_free/t1_obl_free.ts1'
stable_vals = read_numerics(numerics)

# Calculate correlation coefficient
correlation_matrix = np.corrcoef(rect1.us_waterlevel, stable_vals)
correlation = correlation_matrix[0, 1]
# Compute R-squared
r_squared = correlation**2
r_squred_list.append(r_squared)
[all_measured.append(x) for x in rect1.us_waterlevel]
[all_simulated.append(x) for x in stable_vals]

#####
# ARC2 weir
#####
rect1 = bridge(measurements, 'ARC2','Weir')
numerics = 'IO/arc2_weir/arc2_weir.ts1'
stable_vals = read_numerics(numerics)

# Calculate correlation coefficient
correlation_matrix = np.corrcoef(rect1.us_waterlevel, stable_vals)
correlation = correlation_matrix[0, 1]
# Compute R-squared
r_squared = correlation**2
r_squred_list.append(r_squared)
[all_measured.append(x) for x in rect1.us_waterlevel]
[all_simulated.append(x) for x in stable_vals]

#####
# I2 weir
#####
rect1 = bridge(measurements, 'I2','Weir')
numerics = 'IO/I2_weir/I2_weir.ts1'
stable_vals = read_numerics(numerics)

# Calculate correlation coefficient
correlation_matrix = np.corrcoef(rect1.us_waterlevel, stable_vals)
correlation = correlation_matrix[0, 1]
# Compute R-squared
r_squared = correlation**2
r_squred_list.append(r_squared)
[all_measured.append(x) for x in rect1.us_waterlevel]
[all_simulated.append(x) for x in stable_vals]

#####
# Rect1 obl free
#####
rect1 = bridge(measurements, 'RECT1 OBLIQUE','Free')
numerics = 'IO/rect1_obl_free/rect1_obl_free.ts1'
stable_vals = read_numerics(numerics)

# Calculate correlation coefficient
correlation_matrix = np.corrcoef(rect1.us_waterlevel, stable_vals)
correlation = correlation_matrix[0, 1]
# Compute R-squared
r_squared = correlation**2
r_squred_list.append(r_squared)
[all_measured.append(x) for x in rect1.us_waterlevel]
[all_simulated.append(x) for x in stable_vals]

##################
#    general
##################

# all r2
all_measured = np.asarray(all_measured)
all_simulated = np.asanyarray(all_simulated)

correlation_matrix = np.corrcoef(all_measured, all_simulated)
correlation = correlation_matrix[0, 1]
# Compute R-squared
r_squared = correlation**2

print('Validated bridge module with a total of %s cases' % (len(all_measured)))
print('R2 = ' + str(r_squared))

# Plotting
fig, ax = plt.subplots(figsize = (10,10))

ax.plot([0,0.13],[0,0.13], color = 'k')
ax.scatter(all_measured,all_simulated,color = 'b', marker = 'x')
ax.grid()
ax.set_aspect('equal')
ax.tick_params(axis='both', which='major', labelsize=14)
ax.tick_params(axis='both', which='minor', labelsize=14)
ax.text(0,0.121, 'R² = ' + "%.3f" % r_squared, fontsize=16)
ax.set_xlabel(u'$h_{measured}$ in m', fontsize = 16)
ax.set_ylabel(u'$h_{simulated}$ in m', fontsize = 16)
plt.tight_layout()
fig.savefig('validationplot.png', dpi = 400)

"""