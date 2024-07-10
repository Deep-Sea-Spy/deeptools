require(chron)
require(plyr)
require(reshape)
require(ggplot2)
require(lubridate)
require(zoo)
require(padr)
require(dplyr)
library(sp)
library(rgdal)
library(raster)
library(jpeg)

# SEB - A proscrire. Faire un package avec des fonctions
# source('~/Stats/Periodograph folder/Periodograph folder/WRperiodogram.R', encoding = 'UTF-8')

# SEB - A proscrire. Faire un projet Rstudio avec des liens relatifs
# Pour que tout le monde puisse utiliser les scripts
# WD='C:/Users/mmatabos/Documents/Science_participatives/DSS/Analyses/Data'
# setwd(WD)

#load and transform list of images in the mission ----
if (FALSE) { # SEB - I do not have images list
liste_photo = read.table('liste_photo.txt')
liste_photo$DateTime=t(data.frame(strsplit(as.character(liste_photo$V4),'_')))[,2]
liste_photo$DateTime = t(data.frame(strsplit(liste_photo$DateTime,"[.]")))[,1]
splitted <- t(sapply(as.numeric(liste_photo$DateTime), function(x) substring(x, first=c(1,9), last=c(8,14))))
liste_photo$DateTime <- chron(dates=splitted[,1], times=splitted[,2],
                              format = c(dates = "Ymd", times = "hms"),
                              out.format = c(dates = 'd/m/Y', times = "h:m:s"))
liste_photo$TimeUTC=with_tz(as.POSIXct(liste_photo$DateTime),tzone="UTC")

liste_photo_MAR=liste_photo[1:577,5:6]
liste_photo_ONC=liste_photo[578:nrow(liste_photo),5:6]
}

#load data ----
mission2 <- read.csv("export_20180329.csv", header = T, stringsAsFactors = FALSE)
mission2 <- mission2[,-which(names(mission2) %in% "comment")]

#convert to date time
mission2$DateTime = t(data.frame(strsplit(mission2$name,'_')))[,2]
mission2$DateTime = t(data.frame(strsplit(mission2$DateTime,"[.]")))[,1]

splitted <- t(sapply(as.numeric(mission2$DateTime), function(x) substring(x, first=c(1,9), last=c(8,14))))
mission2$DateTime <- chron(dates=splitted[,1], times=splitted[,2],
                  format = c(dates = "Ymd", times = "hms"),
                  out.format = c(dates = 'd/m/Y', times = "h:m:s"))

mission2$TimeUTC=with_tz(as.POSIXct(mission2$DateTime),tzone="UTC")

#Separate observatory dataset
mission2_MAR = mission2[which(mission2$obs_code=='MAR'),]
mission2_ONC = mission2[which(mission2$obs_code=='JDF'),]

# Exploration ====
#percentage participation (part) ----
part=table(mission2$username)
part=data.frame(part)
# 438 participants
part$perc=part$Freq*100/sum(part$Freq)
colnames(part)=c('UserID','nb_annotations','percentage')
sum(part$percentage)

part=part[order(part$nb_annotations,decreasing = TRUE),]
part$Sumcum=cumsum(part$percentage)

#Number of participants per number of image annotated ----
nb_annot_part=as.data.frame(table(part$nb_annotations))
colnames(nb_annot_part)=c('Nb_annot','Nb_participants')
nb_annot_part= nb_annot_part[order(nb_annot_part$Nb_annot,decreasing = TRUE),]
nb_annot_part$sumcum=cumsum(nb_annot_part$Nb_participants)

# Without Chipiok (58552 annotations)
part2=part[-1,1:3]
part2$sumcum=cumsum(part2$percentage)

##Number of images annotated by participant ----
toto=mission2[,c(2,4)]
toto=unique(toto[c('image_id','username')])
nb_image_part=ddply(toto,.(username),each(nrow))
colnames(nb_image_part)=c('username','nb_image')
nb_image_part=nb_image_part[order(nb_image_part$nb_image, decreasing=TRUE),]

##Number of time an image was annotated ----
toto=mission2[,c(2,4)]
toto=unique(toto[c('image_id','username')])
nb_part_image=ddply(toto,.(image_id),each(nrow))
colnames(nb_part_image)=c('image_id','nb_times')
nb_part_image=nb_part_image[order(nb_part_image$nb_times, decreasing=TRUE),]

freq_dis=ddply(nb_part_image,.(nb_times),each(nrow))
colnames(freq_dis)=c('nb_times','nb_citizen')

#histogram
ggplot(nb_part_image,aes(nb_times))+
  geom_bar()+
  theme_bw()+
  xlab("Number of times an image was annotated")+
  ylab("Number of images")

####################
###     ONC      ###
####################
#Files
#ONC2_bucc = TOtal file with only buccind data from ONC
#CountM2_bucc = cleaned file of buccinid without spatial information
#bucc_by_id = number of buccinid by annotation
#stats_buccM2 = average stats on each image (number of replicates, mean, median) with all time info
#bucc_globalM2 = stats_buccM2 with only data one the hour (every four hours - global view)
#bucc_globalM2_red = bucc_globalM2 sans gaps (starts 2014-09-19)
#bucc_globalM2_NA = bucc_globalM2 completed with NAs
#TimeUTC = sequence of dates every four hours from July 5 2014 to January 31 2015

#sequence correspondant ? l'heure des vid?os en UTC sans les secondes
four_hour_seq=seq(as.POSIXct("2014-07-05 02:00",tz='UTC'), as.POSIXct("2015-01-31 22:00",tz='UTC'), by = 14400)
TimeUTC=as.data.frame(four_hour_seq)
names(TimeUTC)=c('TimeUTC')
TimeUTC$DateHour=format(TimeUTC$TimeUTC, format='%Y-%m-%d %H:%M')

## BUCCINIDES ####

ONC2_bucc=mission2_ONC[which(mission2_ONC$name_fr=="Escargot buccinide"),]
#Add processed images with no data
ONC2_bucc=merge(ONC2_bucc[,-17],liste_photo_ONC,by='TimeUTC',all.x=T,all.y=T)
write.csv(ONC2_bucc, file = "buccinids_mission2.csv")
#replace NA to keep the lines in the following transformation as 0 buccinid (need to replace id=NA by another)
ONC2_bucc[is.na(ONC2_bucc)]<-0

#Cleaned file without positions and length information
CountM2_bucc=ONC2_bucc[,-which(names(ONC2_bucc)
                               %in% c("pos1x","pos1y","pos2x","pos2y","length","middle_x","middle_y","polygon_values"))]

#Number of buccinids per annotated image
bucc_by_id <- ddply(ONC2_bucc, .(id),each(nrow))
toto <- merge(bucc_by_id[,], ONC2_bucc[,c("DateTime","id","image_id")],
              by = "id")
nb_buccM2_temp = toto[-which(duplicated(toto)==TRUE),]
colnames(nb_buccM2_temp)=c("annot_id","Buccinids","DateTime","Image_ID")

#Calculate mean and median for each image
stats_buccM2 <- ddply(nb_buccM2_temp, .(DateTime), with, each(length,mean,sd,median)(Buccinids))
#replace NA to keep the lines in the following transformation as 0 buccinid (need to replace id=NA by another)
stats_buccM2$mean[stats_buccM2$mean>150]<-0
stats_buccM2$median[stats_buccM2$median>150]<-0
write.csv(stats_buccM2, file = "buccinid_stats_mission2.csv")

#Ajout de colonne temps utiles au tri
stats_buccM2$TimeUTC=with_tz(as.POSIXct(stats_buccM2$DateTime),tzone="UTC")
stats_buccM2$Date=as.Date(stats_buccM2$DateTime)
stats_buccM2$Hour=format(stats_buccM2$TimeUTC, format='%H:%M')
stats_buccM2$DateHour=format(stats_buccM2$TimeUTC, format='%Y-%m-%d %H:%M')

#S?lection des comptages sur l'heure (zoom large/vue globale)
bucc_globalM2=merge(stats_buccM2[,-which(names(stats_buccM2)=='TimeUTC')],TimeUTC[,], by='DateHour') #none of the images with 0 buccinids were on the hour (global view)
#bucc_globalM2_NA=bucc_globalM2[,-c(1:2)]%>% pad (by='TimeUTC') %>% fill_by_value('0')
#Ajout de NA
bucc_globalM2_NA=bucc_globalM2[,-c(1:2)]%>% pad (by='TimeUTC') %>% fill_by_value('NA')

#Fichier sans gaps
bucc_globalM2_red=bucc_globalM2_NA[-(1:521),]

#Plot median data
par(mfrow=c(2,1))
plot(bucc_globalM2_red$TimeUTC,bucc_globalM2_red$median,type='o',xlab="Date",ylab='Number buccinids',main='Median value of Buccinid numbers')
#Rolling median
#plot(bucc_globalM2_red[,2],rollmean(bucc_globalM2_red$median,k=7,align="center")
#     ,type='l',xlab="Date",ylab='Number buccinids',main='Rolling Median of Buccinid numbers (k=7)')

#Plot median data with NA
par(mfrow=c(2,1))
plot(bucc_globalM2_NA$TimeUTC,bucc_globalM2_NA$median,type='o',xlab="Date",ylab='Number buccinids',main='Median value of Buccinid numbers')
#Rolling median
plot(bucc_globalM2_NA[-c(1:3,875:877),7],rollapply(bucc_globalM2_NA$median,median,width=7,align="center", na.rm=TRUE),
     type='l',xlab="Date",ylab='Number buccinids',main='Rolling Median of Buccinid numbers (k=7)')

#raw median data
ggplot(bucc_globalM2_NA, aes(TimeUTC, median))+
  geom_line()+
  scale_x_datetime(date_breaks="1 month", date_minor_breaks='1 week', date_labels="%Y-%m-%d")+
  xlab("Date") + ylab("Number of buccinids")+
  theme_bw()

ggplot(bucc_globalM2_NA, aes(TimeUTC, median))+
  geom_smooth(aes(TimeUTC, median),method='loess', col='brown',fill="brown")+
  scale_x_datetime(date_breaks="1 month", date_minor_breaks='1 week', date_labels="%Y-%m-%d")+
  xlab("Date") + ylab("Number of buccinids")+
  theme_bw()

#plot mean and error bars
ggplot(bucc_globalM2_red, aes(TimeUTC, mean))+
  geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd),width=0.1,col='grey')+
  geom_line()+
  geom_point(shape=16,size=1)+
  scale_x_datetime(date_breaks="1 month", date_minor_breaks='1 week', date_labels="%Y-%m-%d")+
  xlab("Date") + ylab("Number of buccinids")+
  ggtitle('Evolution of density of buccinids')+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=30,face="bold"))+
  theme_bw()

#histogram with the number of times an image was processed
ggplot(bucc_globalM2,aes(length))+
  geom_bar()+
  theme_bw()+
  xlab("Number of times an image was annotated")+
  ylab("Number of images")+
  ggtitle("Number of replicate per image")

#Periodograms on buccinid data
#Remove linear trend
bucc_lm=lm(median~TimeUTC,data=bucc_globalM2)
#periodogram on residuals
WRbucc=WRperiodogram(residuals(bucc_lm),T2=182,nperm=999)
plot.WRperio(WRbucc)

##Cartographie Buccinid?s ----
#Le 12/07/14 ? 10:00:18

r <- raster("C:/Users/mmatabos/Documents/Science_participatives/DSS/Analyses/Images/CAM-TEMPO-MINI-2_20140712100018.jpg")
# proj4string(r) <- "+proj=lcc +lat_1=44 +lat_2=49 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs"
proj4string(r) <- "+init=epsg:2154"
proj4string(r) <- "+init=epsg:2154"

plot_jpeg = function(path, add=FALSE)
{
  require('jpeg')
  r = readJPEG("C:/Users/mmatabos/Documents/Science_participatives/DSS/Analyses/Images/CAM-TEMPO-MINI-2_20140712100018.jpg", native=T) # read the file
  res = dim(r)[2:1] # get the resolution
  if (!add) # initialize an empty plot area if add==FALSE
    plot(0,0,xlim=c(0, res[1]),ylim=c(0,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  rasterImage(r,0,0,res[1],res[2])
}

image_douzejuldix=(ONC2_bucc[which(ONC2_bucc$name=='CAM-TEMPO-MINI-2_20140712100018.jpg'),])
plot_jpeg("C:/Users/mmatabos/Documents/Science_participatives/DSS/Analyses/Images/CAM-TEMPO-MINI-2_20140712100018.jpg")
par(new=TRUE)
plot(image_douzejuldix$middle_x[image_douzejuldix$username=='Jumartico'],
     image_douzejuldix$middle_y[image_douzejuldix$username=='Jumartico'],
     xlim=c(1,1920),ylim=c(1080,1),col='white',xlab='',ylab='',xaxt='n', yaxt='n')
par(new=TRUE)
plot(image_douzejuldix$middle_x[image_douzejuldix$username=='chipiok'],
     image_douzejuldix$middle_y[image_douzejuldix$username=='chipiok'],
     xlim=c(1,1920),ylim=c(1080,1),col='red',xlab='',ylab='',xaxt='n', yaxt='n')
par(new=TRUE)
plot(image_douzejuldix$middle_x[image_douzejuldix$username=='kuetos'],
     image_douzejuldix$middle_y[image_douzejuldix$username=='kuetos'],
     xlim=c(1,1920),ylim=c(1080,1),col='blue',xlab='',ylab='',xaxt='n', yaxt='n')
par(new=TRUE)
plot(image_douzejuldix$middle_x[image_douzejuldix$username=='Lukas29'],
     image_douzejuldix$middle_y[image_douzejuldix$username=='Lukas29'],
     xlim=c(1,1920),ylim=c(1080,1),col='green',xlab='',ylab='',xaxt='n', yaxt='n')
par(new=TRUE)
plot(image_douzejuldix$middle_x[image_douzejuldix$username=='joco'],
     image_douzejuldix$middle_y[image_douzejuldix$username=='joco'],
     xlim=c(1,1920),ylim=c(1080,1),col='yellow',xlab='',ylab='',xaxt='n', yaxt='n')

## ZOARCIDES ####

ONC2_zoar=mission2_ONC[which(mission2_ONC$name_fr=="Poisson zoarcide"),]
#Add processed images with no data
ONC2_zoar=merge(ONC2_zoar[,-17],liste_photo_ONC,by='TimeUTC',all.x=T,all.y=T)
#replace NA to keep the lines in the following transformation as 0 buccinid (need to replace id=NA by another)
ONC2_zoar[is.na(ONC2_zoar)]<-0
write.csv(ONC2_zoar, file = "zoarcids_mission2.csv")

#Cleaned file without positions and length information
CountM2_zoar=ONC2_zoar[,-which(names(ONC2_zoar)
                               %in% c("pos1x","pos1y","pos2x","pos2y","length","middle_x","middle_y","polygon_values"))]

#Number of buccinids per annotated image
zoar_by_id <- ddply(ONC2_zoar, .(id),each(nrow))
toto <- merge(zoar_by_id[,], ONC2_zoar[,c("DateTime","id","image_id")],
              by = "id")
nb_zoarM2_temp = toto[-which(duplicated(toto)==TRUE),]
colnames(nb_zoarM2_temp)=c("annot_id","Zoarcids","DateTime","Image_ID")

#Calculate mean and median for each image
stats_zoarM2 <- ddply(nb_zoarM2_temp, .(DateTime), with, each(length,mean,sd,median)(Zoarcids))
#replace NA to keep the lines in the following transformation as 0 buccinid (need to replace id=NA by another)
stats_zoarM2$mean[stats_zoarM2$mean>150]<-0
stats_zoarM2$median[stats_zoarM2$median>150]<-0
write.csv(stats_zoarM2, file = "zoarcid_stats_mission2.csv")

#Ajout de colonne temps utiles au tri
stats_zoarM2$TimeUTC=with_tz(as.POSIXct(stats_zoarM2$DateTime),tzone="UTC")
stats_zoarM2$Date=as.Date(stats_zoarM2$DateTime)
stats_zoarM2$Hour=format(stats_zoarM2$TimeUTC, format='%H:%M')
stats_zoarM2$DateHour=format(stats_zoarM2$TimeUTC, format='%Y-%m-%d %H:%M')

#S?lection des comptages sur l'heure (zoom large/vue globale)
zoar_globalM2=merge(stats_zoarM2[,-which(names(stats_zoarM2)=='TimeUTC')],TimeUTC[,], by='DateHour')
#Ajout de NA
zoar_globalM2_NA=zoar_globalM2[,-c(1:2)]%>% pad (by='TimeUTC') %>% fill_by_value('NA')

#Fichier sans gaps -
zoar_globalM2_red=zoar_globalM2[-(XX:XX),]

## PYCNOGONIDS ####

ONC2_pycno=mission2_ONC[which(mission2_ONC$name_fr=="Pycnogonide"),]
#Add processed images with no data
#ONC2_pycno=merge(ONC2_pycno[,-17],liste_photo_ONC,by='TimeUTC',all.x=T,all.y=T)
#replace NA to keep the lines in the following transformation as 0 pycnogonid (need to replace id=NA by another)
#ONC2_pycno[is.na(ONC2_pycno)]<-0
write.csv(ONC2_pycno, file = "Pycnogonids_mission2.csv")

#Cleaned file without positions and length information
CountM2_pycno=ONC2_pycno[,-which(names(ONC2_pycno)
                               %in% c("pos1x","pos1y","pos2x","pos2y","length","middle_x","middle_y","polygon_values"))]

#Number of pycno per annotated image
pycno_by_id <- ddply(ONC2_pycno, .(id),each(nrow))
toto <- merge(pycno_by_id[,], ONC2_pycno[,c("DateTime","id","image_id")],
              by = "id")
nb_pycnoM2_temp = toto[-which(duplicated(toto)==TRUE),]
colnames(nb_pycnoM2_temp)=c("annot_id","Pycnogonids","DateTime","Image_ID")

#Calculate mean and median for each image
stats_pycnoM2 <- ddply(nb_pycnoM2_temp, .(DateTime), with, each(length,mean,sd,median)(Pycnogonids))
#replace NA to keep the lines in the following transformation as 0 pycnogonid (need to replace id=NA by another)
#stats_pycnoM2$mean[stats_pycnoM2$mean>1600]<-0
#stats_pycnoM2$median[stats_pycnoM2$median>1600]<-0
write.csv(stats_pycnoM2, file = "pycnogonid_stats_mission2.csv")

#Ajout de colonne temps utiles au tri
stats_pycnoM2$TimeUTC=with_tz(as.POSIXct(stats_pycnoM2$DateTime),tzone="UTC")
stats_pycnoM2$Date=as.Date(stats_pycnoM2$DateTime)
stats_pycnoM2$Hour=format(stats_pycnoM2$TimeUTC, format='%H:%M')
stats_pycnoM2$DateHour=format(stats_pycnoM2$TimeUTC, format='%Y-%m-%d %H:%M')

#S?lection des comptages sur l'heure (zoom large/vue globale)
pycno_globalM2=merge(stats_pycnoM2[,-which(names(stats_pycnoM2)=='TimeUTC')],TimeUTC[,], by='DateHour')
#Ajout de NA
pycno_globalM2_NA=pycno_globalM2[,-c(1:2)]%>% pad (by='TimeUTC') %>% fill_by_value('NA')

#Fichier sans gaps -
pycno_globalM2_red=pycno_globalM2_NA[-(1:456),]

#raw median data
ggplot(pycno_globalM2_red, aes(TimeUTC, median))+
  geom_line()+
  scale_x_datetime(date_breaks="1 month", date_minor_breaks='1 week', date_labels="%Y-%m-%d")+
  xlab("Date") + ylab("Number of pycnogonids")+
  ggtitle('Number of pycnogonids (median)')
  theme_bw()

#plot mean and error bars
ggplot(pycno_globalM2_red, aes(TimeUTC, mean))+
  geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd),width=0.1,col='blue')+
  geom_line()+
  geom_point(shape=16,size=1)+
  scale_x_datetime(date_breaks="1 month", date_minor_breaks='1 week', date_labels="%Y-%m-%d")+
  xlab("Date") + ylab("Number of pycnogonids")+
  ggtitle('Evolution of density of pycnogonids')+
  theme_bw()

#histogram with the number of times an image was processed
ggplot(stats_pycnoM2,aes(length))+
  geom_bar()+
  theme_bw()+
  xlab("Number of times an image was annotated")+
  ylab("Number of images")+
  ggtitle("Number of replicate per image")

#Periodograms on buccinid data - pas r?gulier
WRpycno=WRperiodogram(pycno_globalM2_red$median,T2=182,nperm=999)
plot.WRperio(WRpycno)

## POLYNOIDS ####

ONC2_poly=mission2_ONC[which(mission2_ONC$name_fr=="Vers polynoides"),]
#Add processed images with no data
#ONC2_poly=merge(ONC2_poly[,-17],liste_photo_ONC,by='TimeUTC',all.x=T,all.y=T)
#replace NA to keep the lines in the following transformation as 0 polynoid (need to replace id=NA by another)
#ONC2_poly[is.na(ONC2_poly)]<-0
write.csv(ONC2_poly, file = "Polynoids_mission2.csv")

#Cleaned file without positions and length information
CountM2_poly=ONC2_poly[,-which(names(ONC2_poly)
                                 %in% c("pos1x","pos1y","pos2x","pos2y","length","middle_x","middle_y","polygon_values"))]

#Number of pycno per annotated image
poly_by_id <- ddply(ONC2_poly, .(id),each(nrow))
toto <- merge(poly_by_id[,], ONC2_poly[,c("DateTime","id","image_id")],
              by = "id")
nb_polyM2_temp = toto[-which(duplicated(toto)==TRUE),]
colnames(nb_polyM2_temp)=c("annot_id","Polynoids","DateTime","Image_ID")

#Calculate mean and median for each image
stats_polyM2 <- ddply(nb_polyM2_temp, .(DateTime), with, each(length,mean,sd,median)(Polynoids))
#replace NA to keep the lines in the following transformation as 0 polynoid (need to replace id=NA by another)
#stats_polyM2$mean[stats_polyM2$mean>1600]<-0
#stats_polyM2$median[stats_polyM2$median>1600]<-0
write.csv(stats_polyM2, file = "polynoid_stats_mission2.csv")

#Ajout de colonne temps utiles au tri
stats_polyM2$TimeUTC=with_tz(as.POSIXct(stats_polyM2$DateTime),tzone="UTC")
stats_polyM2$Date=as.Date(stats_polyM2$DateTime)
stats_polyM2$Hour=format(stats_polyM2$TimeUTC, format='%H:%M')
stats_polyM2$DateHour=format(stats_polyM2$TimeUTC, format='%Y-%m-%d %H:%M')

#S?lection des comptages sur l'heure (zoom large/vue globale)
poly_globalM2=merge(stats_polyM2[,-which(names(stats_polyM2)=='TimeUTC')],TimeUTC[,], by='DateHour')
#Ajout de NA
poly_globalM2_NA=poly_globalM2[,-c(1:2)]%>% pad (by='TimeUTC') %>% fill_by_value('NA')

#Fichier sans gaps -
poly_globalM2_red=poly_globalM2_NA[-(1:456),]

#raw median data
ggplot(poly_globalM2_red, aes(TimeUTC, median))+
  geom_line()+
  scale_x_datetime(date_breaks="1 month", date_minor_breaks='1 week', date_labels="%Y-%m-%d")+
  xlab("Date") + ylab("Number of polynoids")+
  ggtitle('Number of polynoids (median)')+
  theme_bw()

#plot mean and error bars
ggplot(poly_globalM2_red, aes(TimeUTC, mean))+
  geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd),width=0.1,col='blue')+
  geom_line()+
  geom_point(shape=16,size=1)+
  scale_x_datetime(date_breaks="1 month", date_minor_breaks='1 week', date_labels="%Y-%m-%d")+
  xlab("Date") + ylab("Number of polynoids")+
  ggtitle('Evolution of density of polynoids')+
  theme_bw()

#histogram with the number of times an image was processed
ggplot(stats_polyM2,aes(length))+
  geom_bar()+
  theme_bw()+
  xlab("Number of times an image was annotated")+
  ylab("Number of images")+
  ggtitle("Number of replicate per image")

#Periodograms on buccinid data - pas r?gulier
WRpoly=WRperiodogram(poly_globalM2_red$median,T2=182,nperm=999)
plot.WRperio(WRpoly)

####################
###     EMSO      ###
####################
#Files
#MAR2_crab = TOtal file with only buccind data from ONC
#CountM2_crab = cleaned file of buccinid without psatial information
#crab_by_id = number of buccinid by annotation
#stats_crabM2 = average stats on each image (number of replicates, mean, median) with all time info
#crab_globalM2 = stats_crabM2 with only data one the hour (every four hours - global view)
#crab_globalM2_red = crab_globalM2 sans gaps (starts 2014-09-19)
#crab_globalM2_NA = crab_globalM2 completed with NAs
#MARTimeUTC = sequence of dates every six hours from July 27 2014 to January 31 2015

#sequence correspondant ? l'heure des vid?os en UTC sans les secondes
six_hour_seq=seq(as.POSIXct("2014-07-27 12:00",tz='UTC'), as.POSIXct("2015-02-02 00:00",tz='UTC'), by = 21600)
MARTimeUTC=as.data.frame(six_hour_seq)
names(MARTimeUTC)=c('TimeUTC')
MARTimeUTC$DateHour=format(MARTimeUTC$TimeUTC, format='%Y-%m-%d %H:%M')

## CRABS ####

MAR2_crab=mission2_MAR[which(mission2_MAR$name_fr=="Crabe bythograeide"),]
#Add processed images with no data
MAR2_crab=merge(MAR2_crab[,-17],liste_photo_MAR,by='TimeUTC',all.x=T,all.y=T)
write.csv(MAR2_crab, file = "crabs_mission2.csv")
#replace NA to keep the lines in the following transformation as 0 buccinid (need to replace id=NA by another)
MAR2_crab[is.na(MAR2_crab)]<-0

#Cleaned file without positions and length information
CountM2_crab=MAR2_crab[,-which(names(MAR2_crab)
                               %in% c("pos1x","pos1y","pos2x","pos2y","length","middle_x","middle_y","polygon_values"))]

#Number of crabs per annotated image
crab_by_id <- ddply(MAR2_crab, .(id),each(nrow))
toto <- merge(crab_by_id[,], MAR2_crab[,c("DateTime","id","image_id")],
              by = "id")
nb_crabM2_temp = toto[-which(duplicated(toto)==TRUE),]
colnames(nb_crabM2_temp)=c("annot_id","Crabs","DateTime","Image_ID")

#Calculate mean and median for each image
stats_crabM2 <- ddply(nb_crabM2_temp, .(DateTime), with, each(length,mean,sd,median)(Crabs))
#replace NA to keep the lines in the following transformation as 0 buccinid (need to replace id=NA by another)
stats_crabM2$mean[stats_crabM2$mean>180]<-0
stats_crabM2$median[stats_crabM2$median>180]<-0
write.csv(stats_crabM2, file = "buccinid_stats_mission2.csv")

#Ajout de colonne temps utiles au tri
stats_crabM2$TimeUTC=with_tz(as.POSIXct(stats_crabM2$DateTime),tzone="UTC")
stats_crabM2$Date=as.Date(stats_crabM2$DateTime)
stats_crabM2$Hour=format(stats_crabM2$TimeUTC, format='%H:%M')
stats_crabM2$DateHour=format(stats_crabM2$TimeUTC, format='%Y-%m-%d %H:%M')
stats_crabM2$HourRound=round_date(stats_crabM2$TimeUTC,unit='hour')

#Ajout de NA
crab_globalM2=stats_crabM2[,c(9,1,2,3,4,5,7,8,10)]
crab_globalM2_NA=crab_globalM2[-c(1:2),-c(1:2)]%>% pad (by='HourRound') %>% fill_by_value('NA')

#Fichier sans gaps
#crab_globalM2_red=crab_globalM2[-(1:521),]

#Plot median data
par(mfrow=c(2,1))
plot(stats_crabM2$TimeUTC,stats_crabM2$median,type='o',xlab="Date",ylab='Number buccinids',main='Median value of Buccinid numbers')

#raw median data
ggplot(stats_crabM2, aes(TimeUTC, median))+
  geom_line()+
  scale_x_datetime(date_breaks="1 month", date_minor_breaks='1 week', date_labels="%Y-%m-%d")+
  xlab("Date") + ylab("Number of crabs")+
  theme_bw()

#plot mean and error bars
ggplot(stats_crabM2, aes(TimeUTC, mean))+
  geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd),width=0.1,col='grey')+
  #geom_line()+
  geom_point(shape=16,size=1)+
  scale_x_datetime(date_breaks="1 month", date_minor_breaks='1 week', date_labels="%Y-%m-%d")+
  xlab("Date") + ylab("Number of crabs")+
  ggtitle('Evolution of density of crabs')+
  theme_bw()

#histogram with the number of times an image was processed
ggplot(stats_crabM2,aes(length))+
  geom_bar()+
  theme_bw()+
  xlab("Number of times an image was annotated")+
  ylab("Number of images")+
  ggtitle("Number of replicate per image")

#Periodograms on buccinid data - pas r?gulier
#WRbucc=WRperiodogram(bucc_globalM2_NA$median,T2=182,nperm=999)
#plot.WRperio(WRbucc)

###Plot crabs positions ----
r <- raster("C:/Users/mmatabos/Documents/Science_participatives/DSS/Analyses/Images/MOMAR_20141204175330.jpg")
# proj4string(r) <- "+proj=lcc +lat_1=44 +lat_2=49 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs"
proj4string(r) <- "+init=epsg:2154"
proj4string(r) <- "+init=epsg:2154"

plot_jpeg = function(path, add=FALSE)
{
  require('jpeg')
  r = readJPEG("C:/Users/mmatabos/Documents/Science_participatives/DSS/Analyses/Images/MOMAR_20141204175330.jpg", native=T) # read the file
  res = dim(r)[2:1] # get the resolution
  if (!add) # initialize an empty plot area if add==FALSE
    plot(0,0,xlim=c(0, res[1]),ylim=c(0,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  rasterImage(r,0,0,res[1],res[2])
}

image_MARdec=(MAR2_crab[which(MAR2_crab$name=='MOMAR_20141204175330.jpg'),])
plot_jpeg("C:/Users/mmatabos/Documents/Science_participatives/DSS/Analyses/Images/MOMAR_20141204175330.jpg")
par(new=TRUE)
plot(image_MARdec$middle_x[image_MARdec$username=='matamarjo'],
     image_MARdec$middle_y[image_MARdec$username=='matamarjo'],
     xlim=c(1,1920),ylim=c(1080,0),col='yellow',xlab='',ylab='',xaxt='n', yaxt='n',pch=16)
par(new=TRUE)
plot(image_MARdec$middle_x[image_MARdec$username=='margauxdeniel'],
     image_MARdec$middle_y[image_MARdec$username=='margauxdeniel'],
     xlim=c(1,1920),ylim=c(1080,0),col='green',xlab='',ylab='',xaxt='n', yaxt='n',pch=16)
par(new=TRUE)
plot(image_MARdec$middle_x[image_MARdec$username=='grillus33'],
     image_MARdec$middle_y[image_MARdec$username=='grillus33'],
     xlim=c(1,1920),ylim=c(1080,0),col='white',xlab='',ylab='',xaxt='n', yaxt='n',pch=16)
par(new=TRUE)
plot(image_MARdec$middle_x[image_MARdec$username=='fetescience'],
     image_MARdec$middle_y[image_MARdec$username=='fetescience'],
     xlim=c(1,1920),ylim=c(1080,0),col='red',xlab='',ylab='',xaxt='n', yaxt='n',pch=16)
