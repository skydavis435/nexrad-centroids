nexrad.centroids <- function(){
  #Require
    require(geosphere)
    require(data.table)
    require(bit64)
  #Variables
    working.directory<-file.path(path.expand("~"),"NEXRAD-Centroids")
    
  #################### <MODIFY ME> ##################### 
    Re <- 6371000 #Radius of Earth in m 
    theta <- 0.925 #Normalized Nexrad Beamwidth Degrees
    PHI <- 3.95 #Nexrad Elevation Angle
    IR <- 1.21 #Refractive Index
    az.seq <- seq(0.25,359.75,0.5)
    max.dist <- 230000 +125
    slant.range.seq <- seq(2000+125,max.dist,250)
  #################### </MODIFY ME> #####################
    
  #Read in the NEXRAD info
    nexrads<-read.csv(file = file.path(working.directory,"Data","nexrad-stations.csv"),header = T)
    
  #Loop through every nexrad station
    for(i in 56:56){
      sub.nexrads<-nexrads[i,]
      nexrad.code<-as.character(sub.nexrads$ICAO[1])
      nexrad.lon<-sub.nexrads$LON[1]
      nexrad.lat<-sub.nexrads$LAT[1]
      nexrad.el.m<-sub.nexrads$ELEV * 0.3048
      
      for(j in 1:length(slant.range.seq)){
        data<-data.frame(az.seq)
        data$Slant.Range.m<-slant.range.seq[j]
        data$NEXRAD.Center.m <- (data$Slant.Range.m)*sin(PHI*pi/180) + (data$Slant.Range.m)^2/(2*IR*Re)
        data$NEXRAD.Center.msl <- data$NEXRAD.Center.m + nexrad.el.m
        
        data$ground.range.m <-sqrt(data$Slant.Range.m^2 - data$NEXRAD.Center.m^2)
        data$NEXRAD.Beamwidth.m <- data$ground.range.m * (theta*pi/180)
        data$NEXRAD.Beam.Perpendicular.Height.m <- data$NEXRAD.Beamwidth.m * cos(PHI * pi/180)
        data$NEXRAD.Top.m <- data$NEXRAD.Center.m + (0.5 * data$NEXRAD.Beam.Perpendicular.Height.m)
        data$NEXRAD.Top.msl <- data$NEXRAD.Top + nexrad.el.m
        data$NEXRAD.Bottom.m <- data$NEXRAD.Center.m - (0.5 * data$NEXRAD.Beam.Perpendicular.Height.m)
        data$NEXRAD.Bottom.msl <- data$NEXRAD.Bottom.m + nexrad.el.m
        coords<-destPoint(p = cbind(nexrad.lon,nexrad.lat),b = data$az.seq,d = data$ground.range.m)
        data$lon<-coords[,1]
        data$lat<-coords[,2]
        filename<-paste0(nexrad.code,"_",slant.range.seq[j],"m_slant.range.csv")
        write.csv(x = data,file = file.path(working.directory,"temp",filename),row.names = F)
       }
      setwd(file.path(working.directory,"temp"))
      filelist<-list.files(pattern="\\.csv$")
      datalist<-lapply(filelist,fread)
      complete.coverage<-rbindlist(datalist)
      colnames(complete.coverage)<-c("az_seq","Slant_Range_m","NEXRAD_Center_m","NEXRAD_Center_msl","Ground_Range_m","NEXRAD_Beamwidth_m","NEXRAD_Beamwidth_Perpendicular_Height_m","NEXRAD_Top_m","NEXRAD_Top_msl","NEXRAD_Bottom_m","NEXRAD_Bottom_msl","lon","lat")
      final.filename<-paste0(nexrad.code,"_",PHI,"_degree_centroids.csv")
      write.csv(x = complete.coverage,file = file.path(working.directory,"Results",final.filename),row.names = F)
      file.remove(filelist)
      }
}