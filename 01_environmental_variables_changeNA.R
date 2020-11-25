##### Environmental data
common_path<-"./BD/Current/"
files <- list.files(
  path <- common_path,
  pattern <- "\\.tif$",
  recursive = F,          
  full.names = TRUE          
)

####################################33
## HAY QUE CAMBIAR LOS NA!!
wc_current<- stack(files[8:17])
# plot(wc_current)

for(i in 1:10){
  x <-wc_current[[i]]
  x[x<=-1] <- NA
  writeRaster(x,filename=paste("BD/Current/new/",names(x),".tif",sep = ""))
}

plot(wc_current)


soil_current <- stack(files[c(1:7,18)])
for(i in c(1:7)){
  x <-soil_current[[i]]
  x[x>30000] <- NA
  writeRaster(x,filename=paste("BD/Current/new/",names(x),".tif",sep = ""))
}

x <-soil_current[[8]]
x[x>=255] <- NA
writeRaster(x,filename=paste("BD/Current/new/",names(x),".tif",sep = ""))

common_path<-"./BD/Current/"
files <- list.files(
  path <- common_path,
  pattern <- "\\.tif$",
  recursive = F,          
  full.names = TRUE          
)

####################################33

common_path<-"./BD/Future//"
files <- list.files(
  path <- common_path,
  pattern <- "\\.tif$",
  recursive = F,          
  full.names = TRUE          
)

for(f in 8:36){
  wc_future<- stack(files[f])
  wc_future <- stack(raster::crop(wc_future,extent(-17,15,2,13),))
  
  # plot(wc_current)
  wc_future_corr <- wc_future
  for(i in 1:19){
    x <-wc_future[[i]]
    x[x<=-1] <- NA
    wc_future_corr[[i]] <- x
  }
  writeRaster(wc_future_corr,filename=paste0(substr(files[f],start = 1,stop = nchar(files[f])-4),"_corr.tif"),overwrite=T)
}


