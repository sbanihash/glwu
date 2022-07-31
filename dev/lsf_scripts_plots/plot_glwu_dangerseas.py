# Script for plotting unstructured WW3 output files in netCDF format, created by ww3_ounf
# Andre van der Westhuysen, 09/08/20
#
import matplotlib
#matplotlib.use('Agg',warn=False)

import sys
import os
import datetime
from matplotlib.tri import Triangulation, TriAnalyzer, LinearTriInterpolator
import matplotlib.pyplot as plt
import netCDF4
import numpy as np
#import pandas as pd
import cartopy	
import cartopy.crs as ccrs	
import cartopy.feature as cfeature
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER

print('*** In plot_ww3_unstr.py ***')

NWPSdir = os.environ['NWPSdir']
cartopy.config['pre_existing_data_dir'] = NWPSdir+'/lib/cartopy'
print('Reading cartopy shapefiles from:')
print(cartopy.config['pre_existing_data_dir'])

datafile = sys.argv[1]
print('Plotting unstructured file '+str(datafile))

# Parameters
monthstr = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']

def plot_wnd(storm, datafile):
   print('Plotting: Wind')

   #Single file netCDF reading
   ncf=datafile
   nco=netCDF4.Dataset(ncf)

   #Get fields to plot
   lon=nco.variables['longitude'][:]
   lat=nco.variables['latitude'][:]
   timeindays=nco.variables['time'][:]
   uwnd=nco.variables['uwnd'][:]
   vwnd=nco.variables['vwnd'][:]
   triangles=nco.variables['tri'][:,:]

   reflon=np.linspace(lon.min(),lon.max(),1000)
   reflat=np.linspace(lat.min(),lat.max(),1000)
   #reflon=np.linspace(-80.40, -74.75, 1000)
   #reflat=np.linspace(32.50, 36.60, 1000)
   #reflon=np.linspace(-75.70, -71.05, 1000)
   #reflat=np.linspace(38.50, 41.40, 1000)
   #reflon=np.linspace(-80.40, -73.35, 1000)
   #reflat=np.linspace(32.50, 39.50, 1000)
   #reflon=np.linspace(-85.30, -78.50, 1000)
   #reflat=np.linspace(23.00, 29.70, 1000)
   reflon,reflat=np.meshgrid(reflon,reflat)

   plt.figure(figsize = [6.4, 3.8])

   flatness=0.10  # flatness is from 0-.5 .5 is equilateral triangle
   triangles=triangles-1  # Correct indices for Python's zero-base
   tri=Triangulation(lon,lat,triangles=triangles)
   mask = TriAnalyzer(tri).get_flat_tri_mask(flatness)
   tri.set_mask(mask)

   # Convert units to knots
   unit = 'm s-1'
   if unit == 'm s-1':
      unitconvert = 1/0.514444
      uwnd = unitconvert*uwnd
      vwnd = unitconvert*vwnd

   culim = int(np.max( np.sqrt( np.square(np.double(uwnd)) + np.square(np.double(vwnd)) ) ))+1

   # Loop through each time step and plot results
   for ind in range(0, plot_hours):
      plt.clf()
      ax = plt.axes(projection=ccrs.Mercator())
      #ax.set_extent([-100.00, -50.00, 4.00, 48.00], crs=ccrs.PlateCarree())
      #ax.set_extent([-80.40, -74.75, 32.50, 36.60], crs=ccrs.PlateCarree())  
      #ax.set_extent([-85.30, -78.50, 23.00, 29.70], crs=ccrs.PlateCarree())   

      dt = datetime.datetime.combine(datetime.date(1990, 1, 1), datetime.time(0, 0)) + datetime.timedelta(days=timeindays[ind])
      dstr = datetime.date.strftime(dt,'%Y%m%d%H:%M:%S')
      dstr = dstr[0:8]+' '+dstr[8:17]
      print('Plotting '+dstr)

      par=np.sqrt( np.square(np.double(uwnd[ind,:])) + np.square(np.double(vwnd[ind,:])) )
      tli=LinearTriInterpolator(tri,par)
      par_interp=tli(reflon,reflat)
      tli=LinearTriInterpolator(tri,uwnd[ind,:])
      u_interp=tli(reflon,reflat)
      tli=LinearTriInterpolator(tri,vwnd[ind,:])
      v_interp=tli(reflon,reflat)

      plt.pcolormesh(reflon,reflat,par_interp,vmax=culim,shading='flat',cmap=plt.cm.jet, transform=ccrs.PlateCarree())
      cb = plt.colorbar()
      cb.ax.tick_params(labelsize=8)

      rowskip=int(np.floor(par_interp.shape[0]/20))
      colskip=int(np.floor(par_interp.shape[1]/35))
      plt.barbs(reflon[0::rowskip,0::colskip],reflat[0::rowskip,0::colskip],\
         u_interp[0::rowskip,0::colskip],v_interp[0::rowskip,0::colskip],\
         length=3.5,linewidth=0.5,transform=ccrs.PlateCarree())

      coast = cfeature.GSHHSFeature(scale='high',edgecolor='black',facecolor='none',linewidth=0.25)	
      ax.add_feature(coast)

      gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
                  linewidth=2, color='gray', alpha=0.5, linestyle='--')
      gl.xlabels_top = False
      gl.ylabels_right = False
      gl.xlines = False
      gl.ylines = False
      gl.xformatter = LONGITUDE_FORMATTER
      gl.yformatter = LATITUDE_FORMATTER
      gl.xlabel_style = {'size': 6, 'color': 'black'}
      gl.ylabel_style = {'size': 6, 'color': 'black'}
      figtitle = 'GLWU Wind (knots) \n Hour '\
              +str(ind)+' ('+str(dt.hour).zfill(2)+'Z '+str(dt.day).zfill(2)\
              +monthstr[int(dt.month)-1]+str(dt.year)+')'	
      plt.title(figtitle,fontsize=10)

      dtlabel = datetime.date.strftime(dt,'%Y%m%d%H%M%S')
      dtlabel = dtlabel[0:8]+'_'+dtlabel[8:14]
      #filenm = storm+'_hs_'+dtlabel+'.png'
      filenm = 'swan_wind_hr'+str(ind).zfill(3)+'.png'
      plt.savefig(filenm,dpi=300,bbox_inches='tight',pad_inches=0.1)

      del(par)
      del(par_interp)
   plt.clf()

def plot_hs(storm, datafile):
   print('Plotting: Significant Wave Height')

   #Single file netCDF reading
   ncf=datafile
   nco=netCDF4.Dataset(ncf)

   #Get fields to plot
   lon=nco.variables['longitude'][:]
   lat=nco.variables['latitude'][:]
   timeindays=nco.variables['time'][:]
   hs=nco.variables['hs'][:]
   dp=nco.variables['dp'][:]
   triangles=nco.variables['tri'][:,:]

   reflon=np.linspace(lon.min(),lon.max(),1000)
   reflat=np.linspace(lat.min(),lat.max(),1000)
   #>reflon=np.linspace(-92.720, -75.545, 1000)
   #>reflat=np.linspace(41.167500, 49.347000, 1000)
   #reflon=np.linspace(-85.30, -78.50, 1000)
   #reflat=np.linspace(23.00, 29.70, 1000)
   reflon,reflat=np.meshgrid(reflon,reflat)

   plt.figure(figsize = [6.4, 3.8])

   flatness=0.10  # flatness is from 0-.5 .5 is equilateral triangle
   triangles=triangles-1  # Correct indices for Python's zero-base
   tri=Triangulation(lon,lat,triangles=triangles)
   mask = TriAnalyzer(tri).get_flat_tri_mask(flatness)
   tri.set_mask(mask)

   # Convert units to feet	
   unit = 'm'	
   if unit == 'm':	
      unitconvert = 1/0.3048	
      hs = unitconvert*hs

   culim = int(np.max(hs))+1

   # Loop through each time step and plot results
   for ind in range(0, plot_hours): 
      #besttrack = pd.read_csv(PARMnsem+'/storms/'+STORM+'/best_track.txt', header=None, skiprows=4, delim_whitespace=True)
      plt.clf()
      ax = plt.axes(projection=ccrs.Mercator())
      #ax.set_extent([-100.00, -50.00, 4.00, 48.00], crs=ccrs.PlateCarree())
      #>ax.set_extent([-92.720, -75.545, 41.167500, 49.347000], crs=ccrs.PlateCarree()) 
      #ax.set_extent([-85.30, -78.50, 23.00, 29.70], crs=ccrs.PlateCarree())     

      dt = datetime.datetime.combine(datetime.date(1990, 1, 1), datetime.time(0, 0)) + datetime.timedelta(days=timeindays[ind])
      dstr = datetime.date.strftime(dt,'%Y%m%d%H:%M:%S')
      dstr = dstr[0:8]+' '+dstr[8:17]
      print('Plotting '+dstr)

      par=np.double(hs[ind,:])
      tli=LinearTriInterpolator(tri,par)
      par_interp=tli(reflon,reflat)

      par2=np.double(dp[ind,:])
      tli=LinearTriInterpolator(tri,par2)
      par2_interp=tli(reflon,reflat)

      u=np.cos(3.1416/180*(270-par2_interp))
      v=np.sin(3.1416/180*(270-par2_interp))

      #plt.pcolormesh(reflon,reflat,par_interp,vmax=culim,shading='flat',cmap=plt.cm.jet, transform=ccrs.PlateCarree())  #Breathing color scale
      plt.pcolormesh(reflon,reflat,par_interp,vmax=20.,shading='flat',cmap=plt.cm.jet, transform=ccrs.PlateCarree())  #Fixed color scale
      cb = plt.colorbar()
      cb.ax.tick_params(labelsize=8)

      rowskip=int(np.floor(par2_interp.shape[0]/30))
      colskip=int(np.floor(par2_interp.shape[1]/50))
      plt.quiver(reflon[0::rowskip,0::colskip],reflat[0::rowskip,0::colskip],\
      u[0::rowskip,0::colskip],v[0::rowskip,0::colskip],
      color='black',pivot='middle',alpha=0.7,scale=16.,width=0.007,units='inches',transform=ccrs.PlateCarree())

      coast = cfeature.GSHHSFeature(scale='high',edgecolor='black',facecolor='none',linewidth=0.25)	
      ax.add_feature(coast)
      #plt.plot(besttrack.iloc[:,3].values, besttrack.iloc[:,2].values, 'k--', linewidth=1.0, transform=ccrs.PlateCarree())

      gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
                  linewidth=2, color='gray', alpha=0.5, linestyle='--')
      gl.xlabels_top = False
      gl.ylabels_right = False
      gl.xlines = False
      gl.ylines = False
      gl.xformatter = LONGITUDE_FORMATTER
      gl.yformatter = LATITUDE_FORMATTER
      gl.xlabel_style = {'size': 6, 'color': 'black'}
      gl.ylabel_style = {'size': 6, 'color': 'black'}
      figtitle = 'GLWU Significant Wave Height (ft) and Peak Wave Direction \n Hour '\
              +str(ind)+' ('+str(dt.hour).zfill(2)+'Z '+str(dt.day).zfill(2)\
              +monthstr[int(dt.month)-1]+str(dt.year)+')'	
      plt.title(figtitle,fontsize=10)

      dtlabel = datetime.date.strftime(dt,'%Y%m%d%H%M%S')
      dtlabel = dtlabel[0:8]+'_'+dtlabel[8:14]
      #filenm = storm+'_hs_'+dtlabel+'.png'
      filenm = 'swan_sigwaveheight_hr'+str(ind).zfill(3)+'.png'
      plt.savefig(filenm,dpi=300,bbox_inches='tight',pad_inches=0.1)

      del(par)
      del(par_interp)
      del(par2)
      del(par2_interp)
   plt.clf()

def plot_fp(storm, datafile):
   print('Plotting: Peak Wave Period')

   #Single file netCDF reading
   ncf=datafile
   nco=netCDF4.Dataset(ncf)

   #Get fields to plot
   lon=nco.variables['longitude'][:]
   lat=nco.variables['latitude'][:]
   timeindays=nco.variables['time'][:]
   fp=nco.variables['fp'][:]
   dp=nco.variables['dp'][:]
   triangles=nco.variables['tri'][:,:]

   reflon=np.linspace(lon.min(),lon.max(),1000)
   reflat=np.linspace(lat.min(),lat.max(),1000)
   #reflon=np.linspace(-80.40, -73.35, 1000)
   #reflat=np.linspace(32.50, 39.50, 1000)
   reflon,reflat=np.meshgrid(reflon,reflat)

   plt.figure(figsize = [6.4, 3.8])

   flatness=0.10  # flatness is from 0-.5 .5 is equilateral triangle
   triangles=triangles-1  # Correct indices for Python's zero-base
   tri=Triangulation(lon,lat,triangles=triangles)
   mask = TriAnalyzer(tri).get_flat_tri_mask(flatness)
   tri.set_mask(mask)

   culim = int(1/np.min(fp))+1

   # Loop through each time step and plot results
   for ind in range(0, plot_hours): 
      plt.clf()
      ax = plt.axes(projection=ccrs.Mercator())
      #ax.set_extent([-100.00, -50.00, 4.00, 48.00], crs=ccrs.PlateCarree())
      #ax.set_extent([-80.40, -73.35, 32.50, 39.50], crs=ccrs.PlateCarree())     

      dt = datetime.datetime.combine(datetime.date(1990, 1, 1), datetime.time(0, 0)) + datetime.timedelta(days=timeindays[ind])
      dstr = datetime.date.strftime(dt,'%Y%m%d%H:%M:%S')
      dstr = dstr[0:8]+' '+dstr[8:17]
      print('Plotting '+dstr)

      par=np.double(1/fp[ind,:])
      tli=LinearTriInterpolator(tri,par)
      par_interp=tli(reflon,reflat)

      par2=np.double(dp[ind,:])
      tli=LinearTriInterpolator(tri,par2)
      par2_interp=tli(reflon,reflat)

      u=np.cos(3.1416/180*(270-par2_interp))
      v=np.sin(3.1416/180*(270-par2_interp))

      plt.pcolormesh(reflon,reflat,par_interp,vmax=culim,shading='flat',cmap=plt.cm.jet, transform=ccrs.PlateCarree())
      cb = plt.colorbar()
      cb.ax.tick_params(labelsize=8)

      rowskip=int(np.floor(par2_interp.shape[0]/30))
      colskip=int(np.floor(par2_interp.shape[1]/50))
      plt.quiver(reflon[0::rowskip,0::colskip],reflat[0::rowskip,0::colskip],\
      u[0::rowskip,0::colskip],v[0::rowskip,0::colskip],
      color='black',pivot='middle',alpha=0.7,scale=16.,width=0.007,units='inches',transform=ccrs.PlateCarree())

      coast = cfeature.GSHHSFeature(scale='high',edgecolor='black',facecolor='none',linewidth=0.25)	
      ax.add_feature(coast)

      gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
                  linewidth=2, color='gray', alpha=0.5, linestyle='--')
      gl.xlabels_top = False
      gl.ylabels_right = False
      gl.xlines = False
      gl.ylines = False
      gl.xformatter = LONGITUDE_FORMATTER
      gl.yformatter = LATITUDE_FORMATTER
      gl.xlabel_style = {'size': 6, 'color': 'black'}
      gl.ylabel_style = {'size': 6, 'color': 'black'}
      figtitle = 'GLWU Peak Wave Period (s) and Direction \n Hour '\
              +str(ind)+' ('+str(dt.hour).zfill(2)+'Z '+str(dt.day).zfill(2)\
              +monthstr[int(dt.month)-1]+str(dt.year)+')'	
      plt.title(figtitle,fontsize=10)

      dtlabel = datetime.date.strftime(dt,'%Y%m%d%H%M%S')
      dtlabel = dtlabel[0:8]+'_'+dtlabel[8:14]
      #filenm = storm+'_hs_'+dtlabel+'.png'
      filenm = 'swan_waveperiod_hr'+str(ind).zfill(3)+'.png'
      plt.savefig(filenm,dpi=300,bbox_inches='tight',pad_inches=0.1)

      del(par)
      del(par_interp)
      del(par2)
      del(par2_interp)
   plt.clf()

def plot_spr(storm, datafile):
   print('Plotting: Directional Spread')

   #Single file netCDF reading
   ncf=datafile
   nco=netCDF4.Dataset(ncf)

   #Get fields to plot
   lon=nco.variables['longitude'][:]
   lat=nco.variables['latitude'][:]
   timeindays=nco.variables['time'][:]
   spr=nco.variables['spr'][:]
   triangles=nco.variables['tri'][:,:]

   reflon=np.linspace(lon.min(),lon.max(),1000)
   reflat=np.linspace(lat.min(),lat.max(),1000)
   #>reflon=np.linspace(-92.720, -75.545, 1000)
   #>reflat=np.linspace(41.167500, 49.347000, 1000)
   #reflon=np.linspace(-85.30, -78.50, 1000)
   #reflat=np.linspace(23.00, 29.70, 1000)
   reflon,reflat=np.meshgrid(reflon,reflat)

   plt.figure(figsize = [6.4, 3.8])

   flatness=0.10  # flatness is from 0-.5 .5 is equilateral triangle
   triangles=triangles-1  # Correct indices for Python's zero-base
   tri=Triangulation(lon,lat,triangles=triangles)
   mask = TriAnalyzer(tri).get_flat_tri_mask(flatness)
   tri.set_mask(mask)

   # Filter out exception values
   #wcc = np.where(wcc > 1., 0., wcc)

   # Loop through each time step and plot results
   for ind in range(0, plot_hours): 
      plt.clf()
      ax = plt.axes(projection=ccrs.Mercator())
      #ax.set_extent([-100.00, -50.00, 4.00, 48.00], crs=ccrs.PlateCarree())
      #>ax.set_extent([-92.720, -75.545, 41.167500, 49.347000], crs=ccrs.PlateCarree()) 
      #ax.set_extent([-85.30, -78.50, 23.00, 29.70], crs=ccrs.PlateCarree())     

      dt = datetime.datetime.combine(datetime.date(1990, 1, 1), datetime.time(0, 0)) + datetime.timedelta(days=timeindays[ind])
      dstr = datetime.date.strftime(dt,'%Y%m%d%H:%M:%S')
      dstr = dstr[0:8]+' '+dstr[8:17]
      print('Plotting '+dstr)

      par=np.double(spr[ind,:])
      tli=LinearTriInterpolator(tri,par)
      par_interp=tli(reflon,reflat)

      plt.pcolormesh(reflon,reflat,par_interp,vmax=np.max(spr),shading='flat',cmap=plt.cm.jet, transform=ccrs.PlateCarree())
      cb = plt.colorbar()
      cb.ax.tick_params(labelsize=8)
      coast = cfeature.GSHHSFeature(scale='high',edgecolor='black',facecolor='none',linewidth=0.25)	
      ax.add_feature(coast)

      gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
                  linewidth=2, color='gray', alpha=0.5, linestyle='--')
      gl.xlabels_top = False
      gl.ylabels_right = False
      gl.xlines = False
      gl.ylines = False
      gl.xformatter = LONGITUDE_FORMATTER
      gl.yformatter = LATITUDE_FORMATTER
      gl.xlabel_style = {'size': 6, 'color': 'black'}
      gl.ylabel_style = {'size': 6, 'color': 'black'}
      figtitle = 'GLWU Directional Spread (deg) \n Hour '\
              +str(ind)+' ('+str(dt.hour).zfill(2)+'Z '+str(dt.day).zfill(2)\
              +monthstr[int(dt.month)-1]+str(dt.year)+')'	
      plt.title(figtitle,fontsize=10)

      dtlabel = datetime.date.strftime(dt,'%Y%m%d%H%M%S')
      dtlabel = dtlabel[0:8]+'_'+dtlabel[8:14]
      #filenm = upper(storm)+'_wcc_'+dtlabel+'.png'
      filenm = 'swan_spr_hr'+str(ind).zfill(3)+'.png'
      plt.savefig(filenm,dpi=300,bbox_inches='tight',pad_inches=0.1)

      del(par)
      del(par_interp)
   plt.clf()

def plot_stp(storm, datafile):
   print('Plotting: Mean Wave Steepness')

   #Single file netCDF reading
   ncf=datafile
   nco=netCDF4.Dataset(ncf)

   #Get fields to plot
   lon=nco.variables['longitude'][:]
   lat=nco.variables['latitude'][:]
   timeindays=nco.variables['time'][:]
   hs=nco.variables['hs'][:]
   lm=nco.variables['lm'][:]
   triangles=nco.variables['tri'][:,:]

   reflon=np.linspace(lon.min(),lon.max(),1000)
   reflat=np.linspace(lat.min(),lat.max(),1000)
   #>reflon=np.linspace(-92.720, -75.545, 1000)
   #>reflat=np.linspace(41.167500, 49.347000, 1000)
   #reflon=np.linspace(-85.30, -78.50, 1000)
   #reflat=np.linspace(23.00, 29.70, 1000)
   reflon,reflat=np.meshgrid(reflon,reflat)

   plt.figure(figsize = [6.4, 3.8])

   flatness=0.10  # flatness is from 0-.5 .5 is equilateral triangle
   triangles=triangles-1  # Correct indices for Python's zero-base
   tri=Triangulation(lon,lat,triangles=triangles)
   mask = TriAnalyzer(tri).get_flat_tri_mask(flatness)
   tri.set_mask(mask)

   stp = hs/np.maximum(lm,0.1)
   culim = np.max(stp)   # Default scale max of 0.10, but can be overriden by actual max

   # Loop through each time step and plot results
   for ind in range(0, plot_hours): 
      #besttrack = pd.read_csv(PARMnsem+'/storms/'+STORM+'/best_track.txt', header=None, skiprows=4, delim_whitespace=True)
      plt.clf()
      ax = plt.axes(projection=ccrs.Mercator())
      #ax.set_extent([-100.00, -50.00, 4.00, 48.00], crs=ccrs.PlateCarree())
      #>ax.set_extent([-92.720, -75.545, 41.167500, 49.347000], crs=ccrs.PlateCarree()) 
      #ax.set_extent([-85.30, -78.50, 23.00, 29.70], crs=ccrs.PlateCarree())     

      dt = datetime.datetime.combine(datetime.date(1990, 1, 1), datetime.time(0, 0)) + datetime.timedelta(days=timeindays[ind])
      dstr = datetime.date.strftime(dt,'%Y%m%d%H:%M:%S')
      dstr = dstr[0:8]+' '+dstr[8:17]
      print('Plotting '+dstr)

      par=np.double(stp[ind,:])
      tli=LinearTriInterpolator(tri,par)
      par_interp=tli(reflon,reflat)

      plt.pcolormesh(reflon,reflat,par_interp,vmin=0.,vmax=culim,shading='flat',cmap=plt.cm.jet, transform=ccrs.PlateCarree())
      cb = plt.colorbar()
      cb.ax.tick_params(labelsize=8)

      coast = cfeature.GSHHSFeature(scale='high',edgecolor='black',facecolor='none',linewidth=0.25)	
      ax.add_feature(coast)
      #plt.plot(besttrack.iloc[:,3].values, besttrack.iloc[:,2].values, 'k--', linewidth=1.0, transform=ccrs.PlateCarree())

      gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
                  linewidth=2, color='gray', alpha=0.5, linestyle='--')
      gl.xlabels_top = False
      gl.ylabels_right = False
      gl.xlines = False
      gl.ylines = False
      gl.xformatter = LONGITUDE_FORMATTER
      gl.yformatter = LATITUDE_FORMATTER
      gl.xlabel_style = {'size': 6, 'color': 'black'}
      gl.ylabel_style = {'size': 6, 'color': 'black'}
      figtitle = 'GLWU Mean Wave Steepness (-) \n Hour '\
              +str(ind)+' ('+str(dt.hour).zfill(2)+'Z '+str(dt.day).zfill(2)\
              +monthstr[int(dt.month)-1]+str(dt.year)+')'	
      plt.title(figtitle,fontsize=10)

      dtlabel = datetime.date.strftime(dt,'%Y%m%d%H%M%S')
      dtlabel = dtlabel[0:8]+'_'+dtlabel[8:14]
      filenm = 'swan_steepness_hr'+str(ind).zfill(3)+'.png'
      plt.savefig(filenm,dpi=300,bbox_inches='tight',pad_inches=0.1)

      del(par)
      del(par_interp)
   plt.clf()

def plot_hmaxe(storm, datafile):
   print('Plotting: Expected Maximum Wave Height')

   #Single file netCDF reading
   ncf=datafile
   nco=netCDF4.Dataset(ncf)

   #Get fields to plot
   lon=nco.variables['longitude'][:]
   lat=nco.variables['latitude'][:]
   timeindays=nco.variables['time'][:]
   hmaxe=nco.variables['hmaxe'][:]
   dp=nco.variables['dp'][:]
   triangles=nco.variables['tri'][:,:]

   reflon=np.linspace(lon.min(),lon.max(),1000)
   reflat=np.linspace(lat.min(),lat.max(),1000)
   #>reflon=np.linspace(-92.720, -75.545, 1000)
   #>reflat=np.linspace(41.167500, 49.347000, 1000)
   #reflon=np.linspace(-85.30, -78.50, 1000)
   #reflat=np.linspace(23.00, 29.70, 1000)
   reflon,reflat=np.meshgrid(reflon,reflat)

   plt.figure(figsize = [6.4, 3.8])

   flatness=0.10  # flatness is from 0-.5 .5 is equilateral triangle
   triangles=triangles-1  # Correct indices for Python's zero-base
   tri=Triangulation(lon,lat,triangles=triangles)
   mask = TriAnalyzer(tri).get_flat_tri_mask(flatness)
   tri.set_mask(mask)

   # Convert units to feet	
   unit = 'm'	
   if unit == 'm':	
      unitconvert = 1/0.3048	
      hmaxe = unitconvert*hmaxe

   culim = np.max(hmaxe)

   # Loop through each time step and plot results
   for ind in range(0, plot_hours): 
      #besttrack = pd.read_csv(PARMnsem+'/storms/'+STORM+'/best_track.txt', header=None, skiprows=4, delim_whitespace=True)
      plt.clf()
      ax = plt.axes(projection=ccrs.Mercator())
      #ax.set_extent([-100.00, -50.00, 4.00, 48.00], crs=ccrs.PlateCarree())
      #>ax.set_extent([-92.720, -75.545, 41.167500, 49.347000], crs=ccrs.PlateCarree()) 
      #ax.set_extent([-85.30, -78.50, 23.00, 29.70], crs=ccrs.PlateCarree())     

      dt = datetime.datetime.combine(datetime.date(1990, 1, 1), datetime.time(0, 0)) + datetime.timedelta(days=timeindays[ind])
      dstr = datetime.date.strftime(dt,'%Y%m%d%H:%M:%S')
      dstr = dstr[0:8]+' '+dstr[8:17]
      print('Plotting '+dstr)

      par=np.double(hmaxe[ind,:])
      tli=LinearTriInterpolator(tri,par)
      par_interp=tli(reflon,reflat)

      par2=np.double(dp[ind,:])
      tli=LinearTriInterpolator(tri,par2)
      par2_interp=tli(reflon,reflat)

      u=np.cos(3.1416/180*(270-par2_interp))
      v=np.sin(3.1416/180*(270-par2_interp))

      plt.pcolormesh(reflon,reflat,par_interp,shading='flat',cmap=plt.cm.jet, transform=ccrs.PlateCarree())
      cb = plt.colorbar()
      cb.ax.tick_params(labelsize=8)

      rowskip=int(np.floor(par2_interp.shape[0]/30))
      colskip=int(np.floor(par2_interp.shape[1]/50))
      plt.quiver(reflon[0::rowskip,0::colskip],reflat[0::rowskip,0::colskip],\
      u[0::rowskip,0::colskip],v[0::rowskip,0::colskip],
      color='black',pivot='middle',alpha=0.7,scale=16.,width=0.007,units='inches',transform=ccrs.PlateCarree())

      coast = cfeature.GSHHSFeature(scale='high',edgecolor='black',facecolor='none',linewidth=0.25)	
      ax.add_feature(coast)
      #plt.plot(besttrack.iloc[:,3].values, besttrack.iloc[:,2].values, 'k--', linewidth=1.0, transform=ccrs.PlateCarree())

      gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
                  linewidth=2, color='gray', alpha=0.5, linestyle='--')
      gl.xlabels_top = False
      gl.ylabels_right = False
      gl.xlines = False
      gl.ylines = False
      gl.xformatter = LONGITUDE_FORMATTER
      gl.yformatter = LATITUDE_FORMATTER
      gl.xlabel_style = {'size': 6, 'color': 'black'}
      gl.ylabel_style = {'size': 6, 'color': 'black'}
      figtitle = 'GLWU Expected Maximum Wave Height (ft) and Peak Wave Direction \n Hour '\
              +str(ind)+' ('+str(dt.hour).zfill(2)+'Z '+str(dt.day).zfill(2)\
              +monthstr[int(dt.month)-1]+str(dt.year)+')'	
      plt.title(figtitle,fontsize=10)

      dtlabel = datetime.date.strftime(dt,'%Y%m%d%H%M%S')
      dtlabel = dtlabel[0:8]+'_'+dtlabel[8:14]
      filenm = 'swan_maxwaveheight_hr'+str(ind).zfill(3)+'.png'
      plt.savefig(filenm,dpi=300,bbox_inches='tight',pad_inches=0.1)

      del(par)
      del(par_interp)
      del(par2)
      del(par2_interp)
   plt.clf()

def plot_wch(storm, datafile):
   print('Plotting: Significant Breaking Wave Height')

   #Single file netCDF reading
   ncf=datafile
   nco=netCDF4.Dataset(ncf)

   #Get fields to plot
   lon=nco.variables['longitude'][:]
   lat=nco.variables['latitude'][:]
   timeindays=nco.variables['time'][:]
   wch=nco.variables['wch'][:]
   dp=nco.variables['dp'][:]
   triangles=nco.variables['tri'][:,:]

   reflon=np.linspace(lon.min(),lon.max(),1000)
   reflat=np.linspace(lat.min(),lat.max(),1000)
   #>reflon=np.linspace(-92.720, -75.545, 1000)
   #>reflat=np.linspace(41.167500, 49.347000, 1000)
   #reflon=np.linspace(-85.30, -78.50, 1000)
   #reflat=np.linspace(23.00, 29.70, 1000)
   reflon,reflat=np.meshgrid(reflon,reflat)

   plt.figure(figsize = [6.4, 3.8])

   flatness=0.10  # flatness is from 0-.5 .5 is equilateral triangle
   triangles=triangles-1  # Correct indices for Python's zero-base
   tri=Triangulation(lon,lat,triangles=triangles)
   mask = TriAnalyzer(tri).get_flat_tri_mask(flatness)
   tri.set_mask(mask)

   # Convert units to feet	
   unit = 'm'	
   if unit == 'm':	
      unitconvert = 1/0.3048	
      wch = unitconvert*wch

   culim = np.max(wch)

   # Loop through each time step and plot results
   for ind in range(0, plot_hours): 
      plt.clf()
      ax = plt.axes(projection=ccrs.Mercator())
      #ax.set_extent([-100.00, -50.00, 4.00, 48.00], crs=ccrs.PlateCarree())
      #>ax.set_extent([-92.720, -75.545, 41.167500, 49.347000], crs=ccrs.PlateCarree()) 
      #ax.set_extent([-85.30, -78.50, 23.00, 29.70], crs=ccrs.PlateCarree())     

      dt = datetime.datetime.combine(datetime.date(1990, 1, 1), datetime.time(0, 0)) + datetime.timedelta(days=timeindays[ind])
      dstr = datetime.date.strftime(dt,'%Y%m%d%H:%M:%S')
      dstr = dstr[0:8]+' '+dstr[8:17]
      print('Plotting '+dstr)

      par=np.double(wch[ind,:])
      tli=LinearTriInterpolator(tri,par)
      par_interp=tli(reflon,reflat)

      par2=np.double(dp[ind,:])
      tli=LinearTriInterpolator(tri,par2)
      par2_interp=tli(reflon,reflat)

      if ind == 0:
         par_interp = 0.*par_interp    # Variable not defined for first time step -> set to zero

      u=np.cos(3.1416/180*(270-par2_interp))
      v=np.sin(3.1416/180*(270-par2_interp))

      plt.pcolormesh(reflon,reflat,par_interp,vmin=0,vmax=culim,shading='flat',cmap=plt.cm.jet, transform=ccrs.PlateCarree())
      cb = plt.colorbar()
      cb.ax.tick_params(labelsize=8)

      if ind > 0:
         rowskip=int(np.floor(par2_interp.shape[0]/30))
         colskip=int(np.floor(par2_interp.shape[1]/50))
         plt.quiver(reflon[0::rowskip,0::colskip],reflat[0::rowskip,0::colskip],\
         u[0::rowskip,0::colskip],v[0::rowskip,0::colskip],
         color='black',pivot='middle',alpha=0.7,scale=16.,width=0.007,units='inches',transform=ccrs.PlateCarree())

      coast = cfeature.GSHHSFeature(scale='high',edgecolor='black',facecolor='none',linewidth=0.25)	
      ax.add_feature(coast)
      #plt.plot(besttrack.iloc[:,3].values, besttrack.iloc[:,2].values, 'k--', linewidth=1.0, transform=ccrs.PlateCarree())

      gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
                  linewidth=2, color='gray', alpha=0.5, linestyle='--')
      gl.xlabels_top = False
      gl.ylabels_right = False
      gl.xlines = False
      gl.ylines = False
      gl.xformatter = LONGITUDE_FORMATTER
      gl.yformatter = LATITUDE_FORMATTER
      gl.xlabel_style = {'size': 6, 'color': 'black'}
      gl.ylabel_style = {'size': 6, 'color': 'black'}
      figtitle = 'GLWU Significant Breaking Wave Height (ft) and Peak Direction \n Hour '\
              +str(ind)+' ('+str(dt.hour).zfill(2)+'Z '+str(dt.day).zfill(2)\
              +monthstr[int(dt.month)-1]+str(dt.year)+')'	
      plt.title(figtitle,fontsize=10)

      dtlabel = datetime.date.strftime(dt,'%Y%m%d%H%M%S')
      dtlabel = dtlabel[0:8]+'_'+dtlabel[8:14]
      #filenm = storm+'_hs_'+dtlabel+'.png'
      filenm = 'swan_wch_hr'+str(ind).zfill(3)+'.png'
      plt.savefig(filenm,dpi=300,bbox_inches='tight',pad_inches=0.1)

      del(par)
      del(par_interp)
      del(par2)
      del(par2_interp)
   plt.clf()

def plot_wcc(storm, datafile):
   print('Plotting: Whitecap coverage')

   #Single file netCDF reading
   ncf=datafile
   nco=netCDF4.Dataset(ncf)

   #Get fields to plot
   lon=nco.variables['longitude'][:]
   lat=nco.variables['latitude'][:]
   timeindays=nco.variables['time'][:]
   wcc=nco.variables['wcc'][:]
   triangles=nco.variables['tri'][:,:]

   reflon=np.linspace(lon.min(),lon.max(),1000)
   reflat=np.linspace(lat.min(),lat.max(),1000)
   #>reflon=np.linspace(-92.720, -75.545, 1000)
   #>reflat=np.linspace(41.167500, 49.347000, 1000)
   #reflon=np.linspace(-85.30, -78.50, 1000)
   #reflat=np.linspace(23.00, 29.70, 1000)
   reflon,reflat=np.meshgrid(reflon,reflat)

   plt.figure(figsize = [6.4, 3.8])

   flatness=0.10  # flatness is from 0-.5 .5 is equilateral triangle
   triangles=triangles-1  # Correct indices for Python's zero-base
   tri=Triangulation(lon,lat,triangles=triangles)
   mask = TriAnalyzer(tri).get_flat_tri_mask(flatness)
   tri.set_mask(mask)

   # Filter out exception values
   wcc = np.where(wcc > 1., 0., wcc)

   # Loop through each time step and plot results
   for ind in range(0, plot_hours): 
      plt.clf()
      ax = plt.axes(projection=ccrs.Mercator())
      #ax.set_extent([-100.00, -50.00, 4.00, 48.00], crs=ccrs.PlateCarree())
      #>ax.set_extent([-92.720, -75.545, 41.167500, 49.347000], crs=ccrs.PlateCarree()) 
      #ax.set_extent([-85.30, -78.50, 23.00, 29.70], crs=ccrs.PlateCarree())     

      dt = datetime.datetime.combine(datetime.date(1990, 1, 1), datetime.time(0, 0)) + datetime.timedelta(days=timeindays[ind])
      dstr = datetime.date.strftime(dt,'%Y%m%d%H:%M:%S')
      dstr = dstr[0:8]+' '+dstr[8:17]
      print('Plotting '+dstr)

      par=np.double(wcc[ind,:])
      tli=LinearTriInterpolator(tri,par)
      par_interp=tli(reflon,reflat)

      plt.pcolormesh(reflon,reflat,par_interp,vmax=np.max(wcc),shading='flat',cmap=plt.cm.jet, transform=ccrs.PlateCarree())
      cb = plt.colorbar()
      cb.ax.tick_params(labelsize=8)
      coast = cfeature.GSHHSFeature(scale='high',edgecolor='black',facecolor='none',linewidth=0.25)	
      ax.add_feature(coast)

      gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
                  linewidth=2, color='gray', alpha=0.5, linestyle='--')
      gl.xlabels_top = False
      gl.ylabels_right = False
      gl.xlines = False
      gl.ylines = False
      gl.xformatter = LONGITUDE_FORMATTER
      gl.yformatter = LATITUDE_FORMATTER
      gl.xlabel_style = {'size': 6, 'color': 'black'}
      gl.ylabel_style = {'size': 6, 'color': 'black'}
      #figtitle = storm.upper()+': Whitecap coverage (-): '+dstr
      figtitle = 'GLWU Whitecap coverage (-) \n Hour '\
              +str(ind)+' ('+str(dt.hour).zfill(2)+'Z '+str(dt.day).zfill(2)\
              +monthstr[int(dt.month)-1]+str(dt.year)+')'	
      plt.title(figtitle,fontsize=10)

      dtlabel = datetime.date.strftime(dt,'%Y%m%d%H%M%S')
      dtlabel = dtlabel[0:8]+'_'+dtlabel[8:14]
      #filenm = upper(storm)+'_wcc_'+dtlabel+'.png'
      filenm = 'swan_wcc_hr'+str(ind).zfill(3)+'.png'
      plt.savefig(filenm,dpi=300,bbox_inches='tight',pad_inches=0.1)

      del(par)
      del(par_interp)
   plt.clf()

def plot_dsea(storm, datafile, tier, gate_hs_ft, thrsh_hs_ft, limit_hs_ft, thrsh_stp_ft, thrsh_spr_ft, thrsh_wcc_ft, thrsh_wch_ft):
   print('Plotting: Dangerous sea, based on thresholds')

   gate_hs = gate_hs_ft/3.28084 #feet to meter conversion
   thrsh_hs = thrsh_hs_ft/3.28084 #feet to meter conversion
   limit_hs = limit_hs_ft/3.28084 #feet to meter conversion
   thrsh_stp = thrsh_stp_ft
   thrsh_spr = thrsh_spr_ft
   thrsh_wcc = thrsh_wcc_ft
   thrsh_wch = thrsh_wch_ft/3.28084 #feet to meter conversion

   #Single file netCDF reading
   ncf=datafile
   nco=netCDF4.Dataset(ncf)

   #Get fields to plot
   lon=nco.variables['longitude'][:]
   lat=nco.variables['latitude'][:]
   timeindays=nco.variables['time'][:]
   hs=nco.variables['hs'][:]
   spr=nco.variables['spr'][:]
   lm=nco.variables['lm'][:]
   stp = hs/np.maximum(lm,0.1)
   wcc=nco.variables['wcc'][:]
   wch=nco.variables['wch'][:]
   triangles=nco.variables['tri'][:,:]

   # Filter out exception values
   wcc = np.where(wcc > 1., 0., wcc)
   wch[0] = 0.  # Variable not defined for first time step -> set to zero

   # Filter out ice cover areas
   hs = np.where(hs > 10., 0., hs)
   stp = np.where(stp > 0.1, 0., stp)
   spr = np.where(spr > 90., 0., spr)
   wch = np.where(wch > 10., 0., wch)

   mask_hs_gate = hs > gate_hs
   mask_hs_limit = hs > limit_hs
   mask_hs = hs > thrsh_hs
   mask_stp = stp > thrsh_stp
   mask_spr = spr > thrsh_spr
   mask_wcc = wcc > thrsh_wcc
   mask_wch = wch > thrsh_wch

   #mask_dsea = np.asarray(mask_hs) | np.asarray(mask_stp) | np.asarray(mask_spr) | np.asarray(mask_wcc) | np.asarray(mask_wch)
   mask_dsea = np.asarray(mask_hs) | np.asarray(mask_stp) | np.asarray(mask_spr) | np.logical_and(np.asarray(mask_wcc), np.asarray(mask_wch))
   mask_dsea = np.logical_and(np.asarray(mask_hs_gate), mask_dsea)
   mask_dsea = np.logical_or(np.asarray(mask_hs_limit), mask_dsea)

   #AW
   mask_dsea_hs = np.logical_and(np.asarray(mask_hs_gate), np.asarray(mask_hs))
   mask_dsea_stp = np.logical_and(np.asarray(mask_hs_gate), np.asarray(mask_stp))
   mask_dsea_spr = np.logical_and(np.asarray(mask_hs_gate), np.asarray(mask_spr))
   mask_dsea_wcc = np.logical_and(np.asarray(mask_hs_gate), np.asarray(mask_wcc))
   mask_dsea_wch = np.logical_and(np.asarray(mask_hs_gate), np.asarray(mask_wch))
   #AW

   reflon=np.linspace(lon.min(),lon.max(),1000)
   reflat=np.linspace(lat.min(),lat.max(),1000)
   #>reflon=np.linspace(-92.720, -75.545, 1000)
   #>reflat=np.linspace(41.167500, 49.347000, 1000)
   #reflon=np.linspace(-85.30, -78.50, 1000)
   #reflat=np.linspace(23.00, 29.70, 1000)
   reflon,reflat=np.meshgrid(reflon,reflat)

   plt.figure(figsize = [6.4, 3.8])

   flatness=0.10  # flatness is from 0-.5 .5 is equilateral triangle
   triangles=triangles-1  # Correct indices for Python's zero-base
   tri=Triangulation(lon,lat,triangles=triangles)
   mask = TriAnalyzer(tri).get_flat_tri_mask(flatness)
   tri.set_mask(mask)

   # Filter out exception values
   wcc = np.where(wcc > 1., 0., wcc)

   # Loop through each time step and plot results
   for ind in range(0, plot_hours): 
      plt.clf()
      ax = plt.axes(projection=ccrs.Mercator())
      #ax.set_extent([-100.00, -50.00, 4.00, 48.00], crs=ccrs.PlateCarree())
      #>ax.set_extent([-92.720, -75.545, 41.167500, 49.347000], crs=ccrs.PlateCarree()) 
      #ax.set_extent([-85.30, -78.50, 23.00, 29.70], crs=ccrs.PlateCarree())     

      dt = datetime.datetime.combine(datetime.date(1990, 1, 1), datetime.time(0, 0)) + datetime.timedelta(days=timeindays[ind])
      dstr = datetime.date.strftime(dt,'%Y%m%d%H:%M:%S')
      dstr = dstr[0:8]+' '+dstr[8:17]
      cyc = datetime.datetime.combine(datetime.date(1990, 1, 1), datetime.time(0, 0)) + datetime.timedelta(days=timeindays[0])
      cycstr = datetime.date.strftime(cyc,'%Y%m%d%H:%M:%S')
      cycstr = cycstr[0:8]+'_'+cycstr[8:10]
      print('Plotting '+dstr)

      par=np.double(mask_dsea[ind,:])
      tli=LinearTriInterpolator(tri,par)
      par_interp=tli(reflon,reflat)

      par2=np.double(hs[ind,:])
      tli=LinearTriInterpolator(tri,par2)
      par2_interp=tli(reflon,reflat)
      wet_area = np.sum(par2_interp>0.)

      mask_dsea_pct = np.sum(par_interp)/wet_area*100.

      par3=np.double(mask_dsea_hs[ind,:])
      tli=LinearTriInterpolator(tri,par3)
      par3_interp=tli(reflon,reflat)
      mask_hs_pct = np.sum(par3_interp)/wet_area*100.

      par4=np.double(mask_dsea_stp[ind,:])
      tli=LinearTriInterpolator(tri,par4)
      par4_interp=tli(reflon,reflat)
      mask_stp_pct = np.sum(par4_interp)/wet_area*100.

      par5=np.double(mask_dsea_spr[ind,:])
      tli=LinearTriInterpolator(tri,par5)
      par5_interp=tli(reflon,reflat)
      mask_spr_pct = np.sum(par5_interp)/wet_area*100.

      par6=np.double(mask_dsea_wcc[ind,:])
      tli=LinearTriInterpolator(tri,par6)
      par6_interp=tli(reflon,reflat)
      mask_wcc_pct = np.sum(par6_interp)/wet_area*100.

      par7=np.double(mask_dsea_wch[ind,:])
      tli=LinearTriInterpolator(tri,par7)
      par7_interp=tli(reflon,reflat)
      mask_wch_pct = np.sum(par7_interp)/wet_area*100.

      """
      wet_area = np.count_nonzero(hs[ind,:])
      mask_dsea_pct = np.sum(mask_dsea[ind,:])/wet_area*100.
      mask_hs_pct = np.sum( np.logical_and(mask_hs_gate[ind,:],mask_hs[ind,:]) )/wet_area*100.
      mask_stp_pct = np.sum( np.logical_and(mask_hs_gate[ind,:],mask_stp[ind,:]) )/wet_area*100.
      mask_spr_pct = np.sum( np.logical_and(mask_hs_gate[ind,:],mask_spr[ind,:]) )/wet_area*100.
      mask_wcc_pct = np.sum( np.logical_and(mask_hs_gate[ind,:],mask_wcc[ind,:]) )/wet_area*100.
      mask_wch_pct = np.sum( np.logical_and(mask_hs_gate[ind,:],mask_wch[ind,:]) )/wet_area*100.
      #mask_wch_pct = np.sum(mask_wch[ind,:])/wet_area*100.
      print('Old:')
      print(mask_hs_pct)
      print(mask_stp_pct)
      print(mask_spr_pct)
      print(mask_wcc_pct)
      print(mask_wch_pct) 

      #AW
      mask_hs_pct = np.sum(mask_dsea_hs[ind,:])/wet_area*100.
      mask_stp_pct = np.sum(mask_dsea_stp[ind,:])/wet_area*100.
      mask_spr_pct = np.sum(mask_dsea_spr[ind,:])/wet_area*100.
      mask_wcc_pct = np.sum(mask_dsea_wcc[ind,:])/wet_area*100.
      mask_wch_pct = np.sum(mask_dsea_wch[ind,:])/wet_area*100.
      #AW
      print('New:')
      print(mask_hs_pct)
      print(mask_stp_pct)
      print(mask_spr_pct)
      print(mask_wcc_pct)
      print(mask_wch_pct)
      """
 
      mask_dsea_pct_str='{0:.2f}'.format(mask_dsea_pct)
      mask_hs_pct_str='{0:.2f}'.format(mask_hs_pct)
      mask_stp_pct_str='{0:.2f}'.format(mask_stp_pct)
      mask_spr_pct_str='{0:.2f}'.format(mask_spr_pct)
      mask_wcc_pct_str='{0:.2f}'.format(mask_wcc_pct)
      mask_wch_pct_str='{0:.2f}'.format(mask_wch_pct)

      plt.pcolormesh(reflon,reflat,par_interp,vmin=0,vmax=1,shading='flat',cmap=plt.cm.Reds, transform=ccrs.PlateCarree())
      cb = plt.colorbar(ticks=[0, 1])
      cb.ax.tick_params(labelsize=8)
      coast = cfeature.GSHHSFeature(scale='high',edgecolor='black',facecolor='none',linewidth=0.25)	
      ax.add_feature(coast)
      
      ax.text(-80.5,48.75,'DSeas:  '+mask_dsea_pct_str+'%', transform=ccrs.PlateCarree())    
      ax.text(-80.5,48.25,'Hs:        '+mask_hs_pct_str+'%', transform=ccrs.PlateCarree())
      ax.text(-80.5,47.75,'Steepn: '+mask_stp_pct_str+'%', transform=ccrs.PlateCarree())
      ax.text(-80.5,47.25,'Dspr:     '+mask_spr_pct_str+'%', transform=ccrs.PlateCarree())
      ax.text(-80.5,46.75,'WCC:     '+mask_wcc_pct_str+'%', transform=ccrs.PlateCarree())
      ax.text(-80.5,46.25,'WCH:     '+mask_wch_pct_str+'%', transform=ccrs.PlateCarree())

      gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
                  linewidth=2, color='gray', alpha=0.5, linestyle='--')
      gl.xlabels_top = False
      gl.ylabels_right = False
      gl.xlines = False
      gl.ylines = False
      gl.xformatter = LONGITUDE_FORMATTER
      gl.yformatter = LATITUDE_FORMATTER
      gl.xlabel_style = {'size': 6, 'color': 'black'}
      gl.ylabel_style = {'size': 6, 'color': 'black'}
      #figtitle = storm.upper()+': Whitecap coverage (-): '+dstr
      figtitle = 'GLWU Dangerous Seas Tier '+str(tier)+' (-). Gate: Hs='+str(gate_hs_ft)+'ft\n Thresholds: Hs='+str(thrsh_hs_ft)\
              +'ft/Steepn='+str(thrsh_stp_ft)+'/Dspr='+str(thrsh_spr_ft)\
              +'deg/WCC='+str(thrsh_wcc_ft)+'/WCH='+str(thrsh_wch_ft)\
              +'ft \n Hour '+str(ind)+' ('+str(dt.hour).zfill(2)+'Z '+str(dt.day).zfill(2)\
              +monthstr[int(dt.month)-1]+str(dt.year)+')'	
      plt.title(figtitle,fontsize=10)

      dtlabel = datetime.date.strftime(dt,'%Y%m%d%H%M%S')
      dtlabel = dtlabel[0:8]+'_'+dtlabel[8:14]
      #filenm = upper(storm)+'_wcc_'+dtlabel+'.png'
      filenm = 'swan_dsea'+str(tier)+'_hr'+str(ind).zfill(3)+'.png'
      plt.savefig(filenm,dpi=300,bbox_inches='tight',pad_inches=0.1)

      # Write output file with dangerous seas contributions
      ofilenm = 'glwu_dsea'+str(tier)+'_'+cycstr+'.txt'
      text_file = open(ofilenm, "a")
      outstring = dtlabel+' '+mask_hs_pct_str+' '+mask_stp_pct_str\
                         +' '+mask_spr_pct_str+' '+mask_wcc_pct_str+' '+mask_wch_pct_str+' '+mask_dsea_pct_str+'\n'
      text_file.write("%s" % outstring)
      text_file.close()

      del(par)
      del(par_interp)
   plt.clf()

if __name__ == "__main__":

   plot_hours = 25   # Number of forecast hours to plot (out of the available 49/145 h)

   #---- DSea Tier 1 ----
   gate_hs_ft = 2
   thrsh_hs_ft = 4
   limit_hs_ft = 8
   thrsh_stp_ft =0.04
   thrsh_spr_ft = 60
   thrsh_wcc_ft = 0.012
   thrsh_wch_ft = 3
   plot_dsea(storm='GLWU',datafile=datafile,tier=1,
             gate_hs_ft=gate_hs_ft, thrsh_hs_ft=thrsh_hs_ft,limit_hs_ft=limit_hs_ft,
             thrsh_stp_ft=thrsh_stp_ft,thrsh_spr_ft=thrsh_spr_ft,
             thrsh_wcc_ft=thrsh_wcc_ft,thrsh_wch_ft=thrsh_wch_ft)
   #---- DSea Tier 2 ----
   gate_hs_ft = 4
   thrsh_hs_ft = 8
   limit_hs_ft = 12
   thrsh_stp_ft =0.04
   thrsh_spr_ft = 60
   thrsh_wcc_ft = 0.012
   thrsh_wch_ft = 6
   plot_dsea(storm='GLWU',datafile=datafile,tier=2,
             gate_hs_ft=gate_hs_ft, thrsh_hs_ft=thrsh_hs_ft,limit_hs_ft=limit_hs_ft,
             thrsh_stp_ft=thrsh_stp_ft,thrsh_spr_ft=thrsh_spr_ft,
             thrsh_wcc_ft=thrsh_wcc_ft,thrsh_wch_ft=thrsh_wch_ft)
   #---- DSea Tier 3 ----
   gate_hs_ft = 6
   thrsh_hs_ft = 12
   limit_hs_ft = 20
   thrsh_stp_ft =0.04
   thrsh_spr_ft = 60
   thrsh_wcc_ft = 0.012
   thrsh_wch_ft = 9
   plot_dsea(storm='GLWU',datafile=datafile,tier=3,
             gate_hs_ft=gate_hs_ft, thrsh_hs_ft=thrsh_hs_ft,limit_hs_ft=limit_hs_ft,
             thrsh_stp_ft=thrsh_stp_ft,thrsh_spr_ft=thrsh_spr_ft,
             thrsh_wcc_ft=thrsh_wcc_ft,thrsh_wch_ft=thrsh_wch_ft)
   #---- Component fields ----   
   plot_wnd(storm='GLWU', datafile=datafile)
   plot_hs(storm='GLWU', datafile=datafile)
   plot_spr(storm='GLWU', datafile=datafile)
   plot_stp(storm='GLWU', datafile=datafile)
   plot_wch(storm='GLWU', datafile=datafile)
   plot_wcc(storm='GLWU', datafile=datafile)
   plot_fp(storm='GLWU', datafile=datafile)
   #plot_hmaxe(storm='GLWU', datafile=datafile)
