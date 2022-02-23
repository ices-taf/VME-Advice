
# load current NEAFC closures 
# https://www.neafc.org/closures/coordinates --> downloaded 21 Feb 2022 

NEAFC1_dir <- paste(pathdir,"1-Input data/NEAFC closures/mid-atlantic-vme-closures",sep="/")

shape1  <- st_read(paste(NEAFC1_dir,"altair.shp",sep="/"), crs = 4326)
shape2  <- st_read(paste(NEAFC1_dir,"antialtair.shp",sep="/"), crs = 4326)
shape3  <- st_read(paste(NEAFC1_dir,"midmar.shp",sep="/"), crs = 4326)
shape4  <- st_read(paste(NEAFC1_dir,"northenmar.shp",sep="/"), crs = 4326)
shape5  <- st_read(paste(NEAFC1_dir,"southernmar.shp",sep="/"), crs = 4326)

clos1  <- st_union(shape1,shape2)
for (i in 3:5){
  clos1 <- st_union(clos1,get(paste("shape",i,sep="")))
}

clos1  <- st_cast(clos1,"POLYGON")
clos1  <- st_make_valid(clos1)

# 
NEAFC2_dir <- paste(pathdir,"1-Input data/NEAFC closures/hr_shape_files_2018",sep="/")

shape1  <- st_read(paste(NEAFC2_dir,"f_hattonbank1.shp",sep="/"), crs = 4326)  
shape2  <- st_read(paste(NEAFC2_dir,"g_nw_rockall.shp",sep="/"), crs = 4326) ; shape2 <- st_make_valid(shape2)
shape3  <- st_read(paste(NEAFC2_dir,"g_sw_rockall_v2.shp",sep="/"), crs = 4326); shape3 <- st_make_valid(shape3)
shape4  <- st_read(paste(NEAFC2_dir,"g_sw_rockall2_v2.shp",sep="/"), crs = 4326)
shape5  <- st_read(paste(NEAFC2_dir,"g_sw_rockall3_v2.shp",sep="/"), crs = 4326)
shape6  <- st_read(paste(NEAFC2_dir,"h_logachev_mounds.shp",sep="/"), crs = 4326); shape6 <- st_make_valid(shape6)
shape7  <- st_read(paste(NEAFC2_dir,"i_westrockall_mounds_closures.shp",sep="/"), crs = 4326)
shape8  <- st_read(paste(NEAFC2_dir,"j_edorabank2.shp",sep="/"), crs = 4326); shape8 <- st_make_valid(shape8)
shape9  <- st_read(paste(NEAFC2_dir,"k1_southwest_rockall_area1.shp",sep="/"), crs = 4326)
shape10  <- st_read(paste(NEAFC2_dir,"k2_southwest_rockall_area2.shp",sep="/"), crs = 4326)
shape11  <- st_read(paste(NEAFC2_dir,"l1_hr_basin_area1.shp",sep="/"), crs = 4326)
shape12  <- st_read(paste(NEAFC2_dir,"l2_hr_basin_area2.shp",sep="/"), crs = 4326)
shape13  <- st_read(paste(NEAFC2_dir,"m_hattonbank_area_1.shp",sep="/"), crs = 4326)
shape14  <- st_read(paste(NEAFC2_dir,"m_hattonbank_area_2.shp",sep="/"), crs = 4326)

clos2  <- st_union(shape1,shape2)
for (i in 3:14){
  print(i)
  clos2 <- st_union(clos2,get(paste("shape",i,sep="")))
}
clos2  <- st_cast(clos2,"POLYGON")
clos2  <- st_make_valid(clos2)

#
NEAFC3_dir <- paste(pathdir,"1-Input data/NEAFC closures/IrmingerSeaClosure-shapefiles",sep="/")
clos3  <- st_read(paste(NEAFC3_dir,"RedfishClosure.shp",sep="/"), crs = 4326)  
clos3  <- st_make_valid(clos3)

# combine 
clos_neafc <- st_union(clos1,clos2)
clos_neafc <- st_union(clos_neafc,clos3)

rm(list=ls(pattern="shape"))
rm(list = c('clos1','clos2','clos3','NEAFC1_dir','NEAFC2_dir','NEAFC3_dir'))
