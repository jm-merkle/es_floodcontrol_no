


# import shape
catch<-terra::vect(file.path(dfp,"Norway_files","NVE_vassdragomr","NVEKartdata",
                             "NVEData","Nedborfelt",
                             "Nedborfelt_Vassdragsomr.shp"))
# reproject
catch <- terra::project(catch,crs(lcm))
plot(catch)
# remove svalbard
catch <- crop(catch,ext(4020798.64910517, 5135860.76845464, 3873563.87082421, 5500000))
# use terra::extract. This sums across cells within each polygon. A cell is
# within a polygon if its center is within the polygon.
# first we need a version where NA values are zeros, otherwise it returns NA everywhere
flow_d_econ_adj<-flow_d_econ
flow_d_econ_adj[is.na(flow_d_econ_adj)]<-0
sdam_econ_adj<-sdam_econ
sdam_econ_adj[is.na(sdam_econ_adj)]<-0
metr_d_econ <- flow_d_econ_adj/sdam_econ_adj


metr_d_econ_by_catch <- terra::zonal(metr_d_econ,catch,fun = "mean",na.rm=T,touches=F)

# add data to the shapefile. We can do this because terra::zonal returns values in the same order as the attribute table of the spatial vector file
catch$metdecon <- metr_d_econ_by_catch[,1]

plot(catch,"metdecon",breaks=c(0,0.000000000001,0.2, 0.4, 0.6, 0.8, 1),col=viridis(6),main="Proportion of demand by economic assets met by supply")

# this is good. Try the same for population
flow_d_pop_adj<-flow_d_pop
flow_d_pop_adj[is.na(flow_d_pop_adj)]<-0
sdam_pop_adj<-sdam_pop
sdam_pop_adj[is.na(sdam_pop_adj)]<-0
metr_d_pop <- flow_d_pop_adj/sdam_pop_adj

metr_d_pop_by_catch <- terra::zonal(metr_d_pop,catch,fun = "mean",na.rm=T,touches=F)

# add data to the shapefile. We can do this because terra::zonal returns values in the same order as the attribute table of the spatial vector file
catch$metdpop <- metr_d_pop_by_catch[,1]

plot(catch,"metdpop",breaks=c(0,0.000000000001,0.2, 0.4, 0.6, 0.8, 1),col=viridis(6), main="Proportion of demand by population met by supply")





