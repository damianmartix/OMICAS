import numpy as np
import matplotlib.pyplot as plt  
from matplotlib.colors import BoundaryNorm  #Organizes the color scale
import pandas as pd

import os
os.environ['PROJ_LIB'] = r'C:\Users\User\Anaconda3\pkgs\proj4-5.2.0-h6538335_1006\Library\share'

from mpl_toolkits.basemap import Basemap    #plots the maps in geographical coordinates

#Open a graphic window
plt.figure(figsize=(7.3, 4))
plt.subplots_adjust(left=.06,right=.99,top=.99,bottom=.05)

longlat=pd.read_excel(r'C:/Users/User/All data.xlsx')

lat=longlat.iloc[:,1].values
lon=longlat.iloc[:,2].values
div=1

#Prepare the lon and lat with the area to be ploted (acording to a lon and lat array)
mny=np.floor(min(lat)/div)*div; mxy=np.ceil(max(lat)/div)*div
sty=10 if (mxy-mny < 60) else 15
if(mxy-mny < 20): sty=4
if(mxy-mny > 100): sty=30

circles=np.arange(mny,mxy+sty,sty).tolist()
mnx=np.floor(min(lon)/div)*div; mxx=np.ceil(max(lon)/div)*div
stx=15 if (mxx-mnx < 100) else 30
if(mxx-mnx > 300): stx=45
if(mxx-mnx < 20): stx=4
meridians=np.arange(mnx,mxx+stx,stx).tolist()

mm = Basemap(llcrnrlon=mnx,llcrnrlat=mny,urcrnrlon=mxx,\
            urcrnrlat=mxy,resolution='l',area_thresh=10000.,\
            projection='cyl')     #This sets the coordinates in space, but won't plot any line
mm.drawcountries(zorder=1);mm.drawcoastlines(zorder=1,linewidth=.5)  #Continents will be ploted here
paral=mm.drawparallels(circles,linewidth='0.1',labels=[1,0,0,0],fontsize=8)  #Draw paralels

for m in paral: 
    try: 
        paral[m][1][0].set_rotation(90)
    except:
        dummy=""
mm.drawmeridians(meridians[1:],linewidth='0.1',labels=[0,0,0,1],fontsize=8)  #Draw meridians
plt.box(on=None)  #No frame around the map

inshp='C:/Users/User/Desktop/BR_States/estados_br_pol.shp'  #Input shp 

from shapefileBR_PR import shapefile_BR
states=shapefile_BR(inshp)

for i in states.keys(): plt.plot(states[i][0,:],states[i][1,:],'k',linewidth=0.5) #Plots states

lvl=(0,1200,100) #min, max and step of the shade
#Defining your color scheme
cmap=plt.get_cmap('RdBu')  #RdBu is a scale varing from red to blue. The opposite (blue to red) use 'RdBu_r'
levels=np.arange(lvl[0],lvl[1]+lvl[2],lvl[2])  #Define the levels (or range) you want to plot
norm=BoundaryNorm(levels,ncolors=cmap.N,clip=True)

shade = longlat.iloc[:,3].values

#Plotting values in irregular grid as filled countours
ct=plt.tricontourf(lon,lat,shade,levels=levels,cmap=cmap,norm=norm,zorder=0)  #shade should be an array [nlat,nlon]. Here lon and lat are arrays with the longitude and latitude of each station 
plt.scatter(lon,lat,marker='o',color='k',s=20,zorder=10) #this will plot the location of your stations as circles.