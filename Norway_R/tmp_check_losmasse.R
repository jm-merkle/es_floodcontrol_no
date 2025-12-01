# løsmasse data test

# make space
rm(list = ls())
# packages
library(dplyr)
library(tidyr)
library(terra)
library(sf)
library(knitr)
library(viridisLite)
# filepath
dfp<-file.path("C:","Users","muk",
               paste0("OneDrive - Statistisk sentralbyr","\U00E5"),
               "data","INCA")
# file path to results
rfp<-file.path("C:","Users","muk",
               paste0("OneDrive - Statistisk sentralbyr","\U00E5"),
               "Projects","Naturregnskap","Flomkontroll","Norway_R","output_maps")
# A function to extract levels vectors from factor data stored in rasters 
# rst is raster file, lrn is layer name
extr_lvl_from_raster_lyr<-function(rst,lrn){
  if(is.factor(rst[[lrn]])){
    v1 <- as.vector(values(rst[[lrn]]))
    v2 <- as.vector(levels(rst[[lrn]]))[[1]]
    v3 <- v2$value[v1]
    return(v3)
  } else {
    return(as.vector(values(rst[[lrn]])))
  }
}
gc()

gc()
# read Norway Grunnkart (this is our reference LCM) ----------------------------
lcm<-terra::rast(file.path(dfp,"Norway_files","Grunnkart","Grunnkart_OT.tif"))
terra::plot(lcm, main="Norwegian Land Cover Map (Grunnkart)")

gc()
# make a numeric version of the lcm --------------------------------------------
lcm2<-lcm
activeCat(lcm2) <- "Value"
lcm2<-as.numeric(lcm2)
# read in grunnkart lookup -----------------------------------------------------
lcm_idf<-read.csv(file.path(dfp,"Norway_files","Grunnkart",
                            "Lookup_grunnkart.csv"))
# read in curve number table ---------------------------------------------------
cn_idf<-read.csv(file.path(dfp,"version_2_2","inca_input_floodcontrol",
                           "Lookup_tables",
                           "curve_number_per_landcover_soil.csv"))
# adjust lcm to feature official CORINE categories -----------------------------
# (otherwise our CN lookup table does not fit)
# I create this concordance using the following sources
# https://land.copernicus.eu/content/corine-land-cover-nomenclature-guidelines/html/
# https://nibio.brage.unit.no/nibio-xmlui/bitstream/handle/11250/3120510/NIBIO_RAPPORT_2024_10_28.pdf?sequence=1&isAllowed=y
lcm_idf['CN_cover_id']<-NA
lcm_idf$CN_cover_id[lcm_idf$landcover_id %in% 
                      c(100,111,112,121,130,131,132,133,134,135,142,152)] <- 1
lcm_idf$CN_cover_id[lcm_idf$landcover_id == 141] <- 141
lcm_idf$CN_cover_id[lcm_idf$landcover_id == 200] <- 241
lcm_idf$CN_cover_id[lcm_idf$landcover_id == 310] <- 231
lcm_idf$CN_cover_id[lcm_idf$landcover_id == 320] <- 321
lcm_idf$CN_cover_id[lcm_idf$landcover_id == 400] <- 312
lcm_idf$CN_cover_id[lcm_idf$landcover_id == 410] <- 311
lcm_idf$CN_cover_id[lcm_idf$landcover_id == 420] <- 312
lcm_idf$CN_cover_id[lcm_idf$landcover_id == 440] <- 313
lcm_idf$CN_cover_id[lcm_idf$landcover_id == 520] <- 322
lcm_idf$CN_cover_id[lcm_idf$landcover_id == 600] <- 333
lcm_idf$CN_cover_id[lcm_idf$landcover_id == 611] <- 332
lcm_idf$CN_cover_id[lcm_idf$landcover_id == 623] <- 333
lcm_idf$CN_cover_id[lcm_idf$landcover_id == 631] <- 335 # NA in CN lookup
lcm_idf$CN_cover_id[lcm_idf$landcover_id == 720] <- 322
lcm_idf$CN_cover_id[lcm_idf$landcover_id == 810] <- 511 # NA in cn lookup
lcm_idf$CN_cover_id[lcm_idf$landcover_id == 910] <- 512 # NA in cn lookup
lcm_idf$CN_cover_id[lcm_idf$landcover_id == 1100] <- 331 # NA in cn lookup
lcm_idf$CN_cover_id[lcm_idf$landcover_id == 1200] <- NA # NA 523 in cn lookup
# and then we add the last category that is not available (uklassifisert)
lcm_idf <- rbind(lcm_idf,c(NA,9900,"unclassified","uklassifisert",NA))
lcm_idf$landcover_id <- as.numeric(lcm_idf$landcover_id)
# make a new lcm that features the CN_cover_id
lcm3 <- terra::subst(lcm2,lcm_idf$landcover_id,lcm_idf$CN_cover_id)
terra::plot(lcm3, main="Grunnkartet translated to CORINE LCM categories")
# make a numeric version of this
lcm4 <- rast(ext(lcm3),resolution=res(lcm3),crs=crs(lcm3))
values(lcm4)<-as.numeric(extr_lvl_from_raster_lyr(lcm3,"value"))
gc()

# read hydrological soil type map ----------------------------------------------
soils<-terra::rast(file.path(dfp,"version_2_2","inca_input_floodcontrol",
                             "Hydrological_soil_type_map",
                             "hydro_soilgroup_INCA_EPSG3035.tif"))
# mm comment: only four categories seems a bit coarse...
# mm comment: the soils data is masked in a weird way in the north of Norway. 
# I think we should really use something better here.
# first crop and mask soils ----------------------------------------------------
soils<-crop(soils,lcm)
soils<-mask(soils,lcm)
terra::plot(soils, main="Hydrological soil type masked to Norway")

# # now read in the losmasse data ------------------------------------------------
# los1<-terra::vect(file.path(dfp,"Norway_files","Losmasse","Losmasse","LosmasseFlate_20240621.shp"))
# los2<-terra::vect(file.path(dfp,"Norway_files","Losmasse","Losmasse","LosmasseFlate_20240622.shp"))
# gc()
# plot(los1)
# plot(los2)
# View(as.data.frame(los))
# summary(los$infilt)
# # reproject
# los1p <- terra::project(los1,lcm3)
# los2p <- terra::project(los2,lcm3)
# los1r<-terra::rasterize(los1p,lcm3,field = "infilt",fun="mean")
# los2r<-terra::rasterize(los2p,lcm3,field = "infilt",fun="mean")
# 
# plot(los1r)
# plot(los2r)
# 
# # The two maps look like they are complements to each other, so I add them together.
# losr <- sum(los1r,los2r,na.rm=T)
# losr <- terra::mask(losr,lcm3)

losr <- terra::rast(file.path(rfp,"..","tmp_losm.tif"))

# now let me also import the GCN250 data.

gcn250ii<-terra::rast(file.path(dfp,"..","GCN250","GCN250_ARCII.tif"))

gcn250ii <- terra::project(gcn250ii,lcm)
gcn250ii <- terra::mask(gcn250ii,lcm)

plot(gcn250ii)




plot(losr)
plot(los1r)
plot(lcm3)


plot(losr)
plot(soils)

summary(values(losr))


# try correlation
soils<-terra::resample(soils,lcm3,method="near")
ext(lcm3)
ext(losr)
ext(soils)

dim(losr)

losrc<-losr
soilsc<-soils
losrc[is.na(losrc)]<-0
soilsc[is.na(soilsc)]<-0

correl<-cor(values(losrc),values(soilsc),method="pearson")
conttabl <- table(values(losrc),values(soilsc))
conttabl <- as.data.frame(conttabl) %>% pivot_wider(names_from = Var2,values_from = Freq)

# TYhe four classes in the løsmasse data should correspond more or less to the 
# hydrological soil types by VITO, if those hydrological soil types correspond 
# to the USDA description: https://efotg.sc.egov.usda.gov/references/Delete/2017-11-11/hydrogroups.htm
# need to check the description of the hydrological soil type data. If the description
# does match, then I am happy to just use the Norwegian data. We will though need 
# a process to fille the gaps for those areas that are marked as category 5 (missing data)
# maybe just something based on the correlation with lcm?

plot(lcm3)

writeRaster(losr,file.path(rfp,"..","tmp_losm.tif"),overwrite=T)
writeRaster(soils,file.path(rfp,"..","tmp_soilt.tif"),overwrite=T)
