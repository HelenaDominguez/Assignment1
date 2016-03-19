rm(list=ls())

#Load Libraries
require(ggplot2)
require(plyr)

library(tigerstats)
library(sm)

#Workspace
wspace<-"/Users/Helena/Desktop/DataScience/"

#----------------------------------------------------------------------------------------------------
#Read tables 
 
setClass('myDate')
setAs("character","myDate", function(from) as.Date(from, format="%m/%d/%Y") )

sf_name<-paste(wspace,"sanfrancisco_incidents_summer_2014.csv",sep="")
sf<-read.table(sf_name,header=TRUE,sep=",",colClasses =c('integer','character','character','character','myDate','character','character','character','character','double','double','character','double'))


setClass('myDate')
setAs("character","myDate", function(from) as.Date(from, format="%m/%d/%Y %I:%M:%S %p") )

se_name<-paste(wspace,"seattle_incidents_summer_2014.csv",sep="")
se<-read.csv(se_name,header=TRUE,sep = ",")

#San Francisco data
breaks<-seq(0,23,1)

sf$Hours<-as.numeric(format(strptime(sf$Time, format="%H:%M"), format="%H"))
sf$Month<-as.numeric(format(sf$Date, format="%m"))
sf$Day<-as.numeric(format(sf$Date, format="%d"))

#Seattle data
se$Hours<-as.numeric(format(strptime(as.character(se$Date.Reported),"%m/%d/%Y %I:%M:%S %p"), format="%H"))
se$Day<-as.numeric(format(strptime(as.character(se$Date.Reported),"%m/%d/%Y %I:%M:%S %p"), format="%d"))

    
#Print headers
names(sf)
names(se)

    
#Crime per habitants

sf_pop=837442
crime_pp_sf=length(sf$IncidntNum)/sf_pop  #35/1000 hab

se_pop=670218
crime_pp_se=length(se$Offense.Code)/se_pop #49/1000 hab



#---------------------------
#SEATTLE vs san Francisco
#---------------------------

counts=c(0, crime_pp_sf,crime_pp_se, 0)
barplot(counts*1000, main="Criminality",
   xlab="Cities", ylab="Crimes per 1000 habitants",col=c("black","red","darkblue","black"), width=c(4, 1, 1, 4))
legend("topleft",legend=c("San Francisco","Seattle"),col=c("red","darkblue"), cex=0.8, pch=15,box.lty=0)

quartz.save(paste(wspace,"plots/Crime_SF_vs_SE.png",sep=""),width=6.67,height=5.67)
##Criminality is 40% higher in Seattle


#Histrograms TOTAL crimes in hours
#----------------------------------
    
hist_sf<-hist(sf$Hours,breaks=seq(0,24,1),plot=FALSE,right=FALSE)
hist_se<-hist(se$Hours,breaks=seq(0,24,1),plot=FALSE,right=FALSE)

plot.new()
par(las=1,
xaxs="i",
yaxs="i",
cex.lab="1.2",
mar=c(5, 5, 2, 2) + 0.1
)

plot(hist_sf$mids-diff(breaks)[1]/2,hist_sf$density,type='l',xlim=c(0,23),ylim=c(0,0.08),xaxt='n',yaxt='n',xlab='Hours',ylab='pdf of crimes', col='red', lwd=5)
lines(hist_se$mids-diff(breaks)[1]/2,hist_se$density,col='blue', lwd=5)

legend("topleft",legend=c("San Francisco","Seattle"),col=c("red","darkblue"), cex=0.8, pch=15,box.lty=0) 

axis(1,at=seq(0,24,1),tck=0.02);
axis(3,at=seq(0,24,1),labels=FALSE,tck=0.02);
axis(2,at=seq(0,0.1,0.002),labels=FALSE,tck=0.01);
axis(2,at=seq(0,0.1,0.01),tck=0.02);
axis(4,at=seq(0,0.1,0.002),labels=FALSE,tck=0.01);
axis(4,at=seq(0,0.1,0.02),labels=FALSE,tck=0.02);

quartz.save(paste(wspace,"plots/Crime_per_hour_both.png",sep=""),width=6.67,height=5.67)

  #Histrograms TOTAL crimes per month
#----------------------------------
 #less than 15% variation   
hist_sf<-hist(sf$Month,breaks=seq(6,9,1),plot=FALSE,right=FALSE)
hist_se<-hist(se$Month,breaks=seq(6,9,1),plot=FALSE,right=FALSE)

plot.new()
par(las=1,
xaxs="i",
yaxs="i",
cex.lab="1.2",
mar=c(5, 5, 2, 2) + 0.1,
mfrow=c(1,1)
)

counts=hist_sf$counts
counts=hist_sf$counts


counts<-array(dim=c(2,3))

 counts[1,] <- hist_sf$counts
 counts[2,] <- hist_se$counts


 colnames(counts) <- c("June","July","Augoust")
 rownames(counts) <- c("SF", "SE")  
 
barplot(counts,beside=TRUE,col=c("red","darkblue"),xlab="Month",ylim=c(0,15000),ylab='Number Incidents')
legend("topleft",legend=c("San Francisco","Seattle"),col=c("red","darkblue"), cex=1., pch=15,box.lty=0) 


quartz.save(paste(wspace,"plots/Crime_per_month_both.png",sep=""),width=6.67,height=5.67)

#---------------------------
#SAN FRANCISCO
#---------------------------


# create factors with crime types
sf_class<-(unique(sf$Category))  #34
pc=c(1:length(sf_class)) #% od each class
num_class=c(1:length(sf_class)) 


for (i in 1:length(sf_class)){
    num_class[i]=length(which(sf$Category==sf_class[i]))
    pc[i]=length(which(sf$Category==sf_class[i]))/length(sf$IncidntNum)*100 

        }

#Significant types
sig=which(pc > 1.7)   # 2  3  4  6  8  9 11 12 13 14 15 16
pc_sig=pc[which(pc > 1.7)]
class_sig=sf_class[which(pc > 1.7)]

#Types of Crime per hour
#Only >9%
sig2=which(pc > 9) #2 3 6 9


# 3D Type Crime Pie Chart
#------------------------------
library(plotrix)
pie3D(pc_sig,labels=class_sig,explode=0.1,
   main="Pie Chart of crime class", labelcex=0.7, col=rainbow(length(pc_sig)))  
     
quartz.save(paste(wspace,"plots/Pie_chart_crime_type_SF.png",sep=""),width=6.67,height=5.67)



#Studying significant types only
#-------------------------------
only<-unique(sf$Category)

only_sig<-which(only == "ASSAULT" | only == "DRUG/NARCOTIC" | only == "LARCENY/THEFT" | only == "MISSING PERSON" | only == "NON-CRIMINAL" | only == "OTHER OFFENSES" | only == "SUSPICIOUS OCC" | only == "VEHICLE THEFT" | only == "WARRANTS")

sig<-which(sf$Category == "ASSAULT" | sf$Category == "DRUG/NARCOTIC" | sf$Category == "LARCENY/THEFT" | sf$Category == "MISSING PERSON" | sf$Category == "NON-CRIMINAL" | sf$Category == "OTHER OFFENSES" | sf$Category == "SUSPICIOUS OCC" | sf$Category == "VEHICLE THEFT" | sf$Category == "WARRANTS")

length(sig)/length(sf$Category)



#Histogram hours per class (density)
#--------------------------------------
breaks<-seq(0,24,4)
colores<-c('blue3','skyblue','green3','yellow3','orange','red','brown','violet','black')

plot.new()
par(las=1,
xaxs="i",
yaxs="i",
cex.lab="1.2",
mar=c(5, 5, 2, 2) + 0.1,
mfrow=c(1,1)
)

dat<-data.frame(factor=as.factor(sf$Category[sig]),hours=sf$Hours[sig])
counts<-array(dim=c(length(only_sig),length(breaks)-1))

for (ll in 1:length(only_sig)){
    
    hh<-hist(dat$hours[which(dat$factor == only[only_sig[ll]])],breaks=breaks,plot=FALSE,right=FALSE)
    #counts[ll,] <- hh$density
    counts[ll,] <- hh$counts/length(which(dat$factor == only[only_sig[ll]])) 
    
    
}

colnames(counts) <- c("0-4","4-8","8-12","12-16","16-20","20-24")
rownames(counts) <- only[only_sig]

barplot(counts,beside=TRUE,col=colores,xlab="Hour",ylim=c(0,0.5),ylab='PDF')
legend('topleft',rownames(counts),pch=15,col=colores,box.lty=0,cex=0.9)

quartz.save(paste(wspace,"plots/PDF_per_hour_SF.png",sep=""),width=6.67,height=5.67)

#Histogram hours per class (incidents)
#---------------------------------------
breaks<-seq(0,24,4)
colores<-c('blue3','skyblue','green3','yellow3','orange','red','brown','violet','black')

plot.new()
par(las=1,
xaxs="i",
yaxs="i",
cex.lab="1.2",
mar=c(5, 5, 2, 2) + 0.1,
mfrow=c(1,1)
)

dat<-data.frame(factor=as.factor(sf$Category[sig]),hours=sf$Hours[sig])
counts<-array(dim=c(length(only_sig),length(breaks)-1))

for (ll in 1:length(only_sig)){
    
    hh<-hist(dat$hours[which(dat$factor == only[only_sig[ll]])],breaks=breaks,plot=FALSE,right=FALSE)
    counts[ll,] <- hh$counts
    
    
}

colnames(counts) <- c("0-4","4-8","8-12","12-16","16-20","20-24")
rownames(counts) <- only[only_sig]

barplot(counts,beside=TRUE,col=colores,xlab="Hour",ylim=c(0,3000),ylab='Number of Incidents')
legend('topleft',rownames(counts),pch=15,col=colores,box.lty=0,cex=0.9)

quartz.save(paste(wspace,"plots/Incident_per_class_SF.png",sep=""),width=6.67,height=5.67)


#Histogram weeks per class (density)
#--------------------------------------
    
breaks<-seq(0,32,8)
colores<-c('blue3','skyblue','green3','yellow3','orange','red','brown','violet','black')

plot.new()
par(las=1,
xaxs="i",
yaxs="i",
cex.lab="1.2",
mar=c(5, 5, 2, 2) + 0.1,
mfrow=c(1,1)
)

dat<-data.frame(factor=as.factor(sf$Category[sig]),days=sf$Day[sig])
counts<-array(dim=c(length(only_sig),length(breaks)-1))

for (ll in 1:length(only_sig)){
    
    hh<-hist(dat$days[which(dat$factor == only[only_sig[ll]])],breaks=breaks,plot=FALSE,right=FALSE)
    #counts[ll,] <- hh$density
    counts[ll,] <- hh$counts/length(which(dat$factor == only[only_sig[ll]]))   
    
}

colnames(counts) <- c("1st week","2nd week","3rd week","4th week")
rownames(counts) <- only[only_sig]

barplot(counts,beside=TRUE,col=colores,xlab="",ylim=c(0,0.5),ylab='PDF')
legend('topleft',rownames(counts),pch=15,col=colores,box.lty=0,cex=0.7)
    
quartz.save(paste(wspace,"plots/PDF_per_week_SF.png",sep=""),width=6.67,height=5.67)
    
 #Histogram weeks per class (total number)
#--------------------------------------------
    
breaks<-seq(0,32,8)


plot.new()
par(las=1,
xaxs="i",
yaxs="i",
cex.lab="1.2",
mar=c(5, 5, 2, 2) + 0.1,
mfrow=c(1,1)
)

dat<-data.frame(factor=as.factor(sf$Category[sig]),days=sf$Day[sig])
counts<-array(dim=c(length(only_sig),length(breaks)-1))

for (ll in 1:length(only_sig)){
    
    hh<-hist(dat$days[which(dat$factor == only[only_sig[ll]])],breaks=breaks,plot=FALSE,right=FALSE)
    counts[ll,] <- hh$counts
    
    
}

colnames(counts) <- c("1st week","2nd week","3rd week","4th week")
rownames(counts) <- only[only_sig]

barplot(counts,beside=TRUE,col=colores,xlab="Week",ylim=c(0,3000),ylab='Number of Incidents')
legend('topleft',rownames(counts),pch=15,col=colores,box.lty=0,cex=0.9)
    
quartz.save(paste(wspace,"plots/Incident_per_week_SF.png",sep=""),width=6.67,height=5.67)                          
    
#Plot locations 
#-------------------
plot.new()
par(las=1,
xaxs="i",
yaxs="i",
cex.lab="1.2",
mar=c(5, 5, 2, 2) + 0.1,
mfrow=c(3,3)
)

for (ll in 1:length(only_sig)){
	
    ind=which(dat$factor == only[only_sig[ll]])
    plot(sf$X[ind], sf$Y[ind], xlab='X', ylab='Y',col=colores[ll],  pch=5, cex=0.01, main=class_sig[ll])    
}

quartz.save(paste(wspace,"plots/Location_SF.png",sep=""),width=6.67,height=5.67) 



#---------------------------
#SEATTLE 
#---------------------------
#Plot location

long_min=min(se$Longitude[which(se$Longitude < 0)])
long_max=max(se$Longitude[which(se$Longitude < 0)])

lat_min=min(se$Latitude[which(se$Latitude > 0)])
lat_max=max(se$Latitude[which(se$Latitude > 0)])

qplot(se$Longitude, se$Latitude, shape='dot',  xlab='X', ylab='Y', xlim=c(long_min,long_max), ylim=c(lat_min,lat_max))













