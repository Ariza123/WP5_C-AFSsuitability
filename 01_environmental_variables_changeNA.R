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