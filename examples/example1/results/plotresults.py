import matplotlib.pyplot as plt
import numpy as np
from matplotlib.patches import Polygon

def read_file(filename):
    data = np.genfromtxt(filename, skip_header = 17)

    return data

def read_polygon(filename):
    
    data = np.genfromtxt(filename, skip_header = 16)
    
    p = Polygon(data, facecolor = 'lightgrey',edgecolor = 'purple', linewidth = 2.0)
    
    return p

def read_geo(filename):
    
    data = np.genfromtxt(filename, skip_header = 15)
    
    return data
    

#############################
#          Input
#############################

# section 1
bottom_sec1 = 'bottom_sec1.xy'
free_no_bridge_sec1 = 'free_nB_sec1.xy'
free_bridge_sec1 = 'free_wB_sec1.xy' 

# section 2
bottom_sec2 = 'bottom_sec2.xy'
free_no_bridge_sec2 = 'free_nB_sec2.xy'
free_bridge_sec2 = 'free_wB_sec2.xy'

# bridges
bridge1 = '/home/sebastian/telemac-mascaret/v8p4r0/examples/Example1/bridgesBK/bridge1_new.i2s'
bridge2 = '/home/sebastian/telemac-mascaret/v8p4r0/examples/Example1/bridgesBK/bridge2_new.i2s'
bridge3 = '/home/sebastian/telemac-mascaret/v8p4r0/examples/Example1/bridgesBK/bridge3_new.i2s'

# geo
geo1 = '/home/sebastian/telemac-mascaret/v8p4r0/examples/Example1/bridgesBK/geo1.i2s'
geo2 = '/home/sebastian/telemac-mascaret/v8p4r0/examples/Example1/bridgesBK/geo2.i2s'
geo3 = '/home/sebastian/telemac-mascaret/v8p4r0/examples/Example1/bridgesBK/geo3.i2s'

# other input
b1_x = 100.0
b2_x = 342.208
b3_x = 344.933
diff_x = 180.0

us_wl_b1_wb = 1.783
us_wl_b1_nb = 1.67
us_wl_b2_wb = 1.278
us_wl_b2_nb = 1.261
us_wl_b3_wb = 1.420
us_wl_b3_nb = 1.265

#############################

# read sections
b1 = read_file(bottom_sec1)
s_nB1 = read_file(free_no_bridge_sec1)
s_wB1 = read_file(free_bridge_sec1)

b2 = read_file(bottom_sec2)
s_nB2 = read_file(free_no_bridge_sec2)
s_wB2 = read_file(free_bridge_sec2)

# read cross sections
poly1 = read_polygon(bridge1)
poly2 = read_polygon(bridge2)
poly3 = read_polygon(bridge3)

# read geo
geo_1 = read_geo(geo1)
geo_2 = read_geo(geo2)
geo_3 = read_geo(geo3)

mosaic = [['1','1'],['2','2'],['3','3'],['.','.'],['4','4'],['5','5']]

plt.rcParams.update({
    'axes.titlesize' : 26,
    'axes.labelsize' : 26,
    'xtick.labelsize' : 24,
    'ytick.labelsize' : 24})

fig, axes = plt.subplot_mosaic(mosaic, figsize=(14,16), gridspec_kw={'height_ratios' : [1.2,1.2,1.2,0.1,1.2,1.2]})

# settings for all axis
for i in range(1,6):
    axes[str(i)].grid()

# plot CS 1
axes['1'].set_xlim([-1, 34])
axes['1'].set_ylim([0.70, 2.2])
axes['1'].add_patch(poly1)
axes['1'].plot(geo_1[:,0], geo_1[:,1], color = 'k', linewidth = 2.0, label = 'Bottom')
axes['1'].hlines(us_wl_b1_wb, 0, 20.0, colors='b', label = 'WL inc. Bridges')
axes['1'].hlines(us_wl_b1_nb, 0, 20.0, colors='r', label = 'WL no Bridges')
axes['1'].set_title('Bridge 1 (B1)')
axes['1'].set_ylabel('Elev. in m')

# plot CS 2
axes['2'].set_xlim([-1, 58])
axes['2'].set_ylim([0.10, 4.8])
axes['2'].add_patch(poly2)
axes['2'].plot(geo_2[:,0], geo_2[:,1], color = 'k', linewidth = 2.0, label = 'Bottom')
axes['2'].hlines(us_wl_b2_wb, 4.793, 25.8, colors='b', label = 'WL inc. Bridges')
axes['2'].hlines(us_wl_b2_nb, 4.793, 25.8, colors='r', label = 'WL no Bridges')
axes['2'].set_title('Bridge 2 (B2)')
axes['2'].set_ylabel('Elev. in m')

# plot CS 3
axes['3'].set_xlim([-1, 45])
axes['3'].set_ylim([0.1, 2.2])
axes['3'].add_patch(poly3)
axes['3'].plot(geo_3[:,0], geo_3[:,1], color = 'k', linewidth = 2.0, label = 'Bottom')
axes['3'].hlines(us_wl_b3_wb, 2.208, 25.653, colors='b', label = 'WL inc. Bridges')
axes['3'].hlines(us_wl_b3_nb, 2.835, 25.07, colors='r', label = 'WL no Bridges')
axes['3'].set_title('Bridge 3 (B3)')
axes['3'].set_xlabel('Distance in m')
axes['3'].set_ylabel('Elev. in m')


# Plot section 1
#axes['4'].plot(b1[:,0],b1[:,1])
axes['4'].set_xlim([-10, 550])
axes['4'].set_ylim([1.15, 2.0])
axes['4'].plot(s_nB1[:,0],s_nB1[:,1], color='r', label = 'WL no Bridges')
axes['4'].plot(s_wB1[:,0],s_wB1[:,1], color='b', label = 'WL inc. Bridges')
axes['4'].vlines(b1_x, 0, 10.0, colors='k')
axes['4'].vlines(b2_x, 0, 10.0, colors='k')
axes['4'].vlines(diff_x, 0, 10.0, colors='k')
axes['4'].set_title('Section 1 (S1)')
axes['4'].set_ylabel('Elev. in m')
axes['4'].text(84,1.2,'B1', rotation=90, fontsize = 24)
axes['4'].text(165,1.71,'Sep.', rotation=90, fontsize = 24)
axes['4'].text(326,1.83,'B2', rotation=90, fontsize = 24)

# Plot section 2
axes['5'].set_xlim([-10, 550])
axes['5'].set_ylim([1.15, 2.0])
axes['5'].plot(s_nB2[:,0],s_nB2[:,1], color='r', label = 'WL no Bridges')
axes['5'].plot(s_wB2[:,0],s_wB2[:,1], color='b', label = 'WL inc. Bridges')
axes['5'].vlines(b1_x, 0, 10.0, colors='k')
axes['5'].vlines(b3_x, 0, 10.0, colors='k')
axes['5'].vlines(diff_x, 0, 10.0, colors='k')
axes['5'].set_title('Section 2 (S2)')
axes['5'].set_ylabel('Elev. in m')
axes['5'].set_xlabel('Distance in m')
axes['5'].text(84,1.2,'B1', rotation=90, fontsize = 24)
axes['5'].text(163,1.71,'Sep.', rotation=90, fontsize = 24)
axes['5'].text(329,1.83,'B3', rotation=90, fontsize = 24)

# add legend
for i in range(1,6):
    axes[str(i)].legend(loc = 'upper right', prop = {'size' : 24})


# save
plt.tight_layout()
fig.savefig('example1_plot.png', dpi = 600)
