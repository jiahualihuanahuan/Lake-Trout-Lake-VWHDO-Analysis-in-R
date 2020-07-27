# VWHDO analysis in 7 lake trout lakes (A lakes)


# Load "rLakeAnalyzer" package
library(rLakeAnalyzer)

# 1. Calculate volume of each stratum in 1 m interval

# Example calcultion
a1 <- 420800
a2 <- 362800
v1 <- 783000
(a1+a2+sqrt(a1*a2))*2/3

# load contour area data for lake trout lakes

# set working directory
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/the Lake Trout Strategic Project/Bathymetric csv data")

# Load bathymetric data for lake trout lakes
bath.A <- read.csv("Bath.A.csv",header=T, row.names=1)

bath.V <- matrix(nrow=39, ncol=9)

for (i in 1:9){
	for (k in 1:38){
		bath.V[k,i] <- (bath.A[k,i]+bath.A[k+1,i]+sqrt(bath.A[k,i])*sqrt(bath.A[k+1,i]))/3
	}
}
# Write CSV in R
write.csv(bath.V, file = "my.file.csv")

#---------------------------------------------

# 2. Load oxygen profile and calculate VWHDO for each lake and each sampling date

# Load oxy profile data for each lake
# set working directory
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/the Lake Trout Strategic Project/temp and oxy csv data/oxy")

# Load oxygen profile for lake trout lakes
BlueChalk.oxy <- read.csv("Blue Chalk oxy.csv",header=T, row.names=1)
Chub.oxy <- read.csv("Chub oxy.csv",header=T, row.names=1)
Cross.oxy <- read.csv("Cross oxy.csv",header=T, row.names=1)
Dickie.oxy <- read.csv("Dickie oxy.csv",header=T, row.names=1)
Harp.oxy <- read.csv("Harp oxy.csv",header=T, row.names=1)
Plastic.oxy <- read.csv("Plastic oxy.csv",header=T, row.names=1)
RedChalkEast.oxy <- read.csv("Red Chalk East oxy.csv",header=T, row.names=1)
RedChalkMain.oxy <- read.csv("Red Chalk Main oxy.csv",header=T, row.names=1)

head(BlueChalk.oxy)

str(BlueChalk.oxy)

apply(BlueChalk.oxy,2,mean)

#---------------------------------------------
# 3. Load temperature profile data

# set working directory
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/the Lake Trout Strategic Project/temp and oxy csv data/temp")

# Load temperature profiles
BlueChalk.temp <- read.csv("Blue Chalk temp.csv",header=T, row.names=1)
Chub.temp <- read.csv("Chub temp.csv",header=T, row.names=1)
Cross.temp <- read.csv("Cross temp.csv",header=T, row.names=1)
Dickie.temp <- read.csv("Dickie temp.csv",header=T, row.names=1)
Harp.temp <- read.csv("Harp temp.csv",header=T, row.names=1)
Plastic.temp <- read.csv("Plastic temp.csv",header=T, row.names=1)
RedChalkEast.temp <- read.csv("Red Chalk East temp.csv",header=T, row.names=1)
RedChalkMain.temp <- read.csv("Red Chalk Main temp.csv",header=T, row.names=1)

colnames(BlueChalk.temp)

#---------------------------------------------

# 4. Calculate thermocline depth

# Count depths in each lake
BlueChalk.depths <- length(BlueChalk.temp[,1])
Chub.depths <- length(Chub.temp[,1])
Cross.depths <- length(Cross.temp[,1])
Dickie.depths <- length(Dickie.temp[,1])
Harp.depths <- length(Harp.temp[,1])
Plastic.depths <- length(Plastic.temp[,1])
RedChalkEast.depths <- length(RedChalkEast.temp[,1])
RedChalkMain.depths <- length(RedChalkMain.temp[,1])

# Count dates in data
BlueChalk.Ns <- length(BlueChalk.temp[1,])
Chub.Ns <- length(Chub.temp[1,])
Cross.Ns <- length(Cross.temp[1,])
Dickie.Ns <- length(Dickie.temp[1,])
Harp.Ns <- length(Harp.temp[1,])
Plastic.Ns <- length(Plastic.temp[1,])
RedChalkEast.Ns <- length(RedChalkEast.temp[1,])
RedChalkMain.Ns <- length(RedChalkMain.temp[1,])

# Calculate thermocline depth

BlueChalk.t.d <- c()
Chub.t.d  <- c()
Cross.t.d  <- c()
Dickie.t.d  <- c()
Harp.t.d  <- c()
Plastic.t.d  <- c()
RedChalkEast.t.d  <- c()
RedChalkMain.t.d  <- c()

for (i in 1: BlueChalk.Ns){
	BlueChalk.t.d[i] <- thermo.depth(BlueChalk.temp[,i], 1:(BlueChalk.depths))
	}
print(BlueChalk.t.d)

for (i in 1: Chub.Ns){
	Chub.t.d[i] <- thermo.depth(Chub.temp[,i], 1:(Chub.depths))
	}
print(Chub.t.d)

for (i in 1: Cross.Ns){
	Cross.t.d[i] <- thermo.depth(Cross.temp[,i], 1:(Cross.depths))
	}
print(Cross.t.d)

for (i in 1: Dickie.Ns){
	Dickie.t.d[i] <- thermo.depth(Dickie.temp[,i], 1:(Dickie.depths))
	}
print(Dickie.t.d)

for (i in 1: Harp.Ns){
	Harp.t.d[i] <- thermo.depth(Harp.temp[,i], 1:(Harp.depths))
	}
print(Harp.t.d)

for (i in 1: Plastic.Ns){
	Plastic.t.d[i] <- thermo.depth(Plastic.temp[,i], 1:(Plastic.depths))
	}
print(Plastic.t.d)

for (i in 1: RedChalkEast.Ns){
	RedChalkEast.t.d[i] <- thermo.depth(RedChalkEast.temp[,i], 1:(RedChalkEast.depths))
	}
print(RedChalkEast.t.d)

for (i in 1: RedChalkMain.Ns){
	RedChalkMain.t.d[i] <- thermo.depth(RedChalkMain.temp[,i], 1:(RedChalkMain.depths))
	}
print(RedChalkMain.t.d)

# Write CSV in R
write.csv(BlueChalk.t.d, file = "BlueChalk.t.d.csv")
write.csv(Chub.t.d, file = "Chub.t.d.csv")
write.csv(Cross.t.d, file = "Cross.t.d.csv")
write.csv(Dickie.t.d, file = "Dickie.t.d.csv")
write.csv(Harp.t.d, file = "Harp.t.d.csv")
write.csv(Plastic.t.d, file = "Plastic.t.d.csv")
write.csv(RedChalkEast.t.d, file = "RedChalkEast.t.d.csv")
write.csv(RedChalkMain.t.d, file = "RedChalkMain.t.d.csv")

BlueChalk.t.d
Chub.t.d
Cross.t.d
Dickie.t.d
Harp.t.d
Plastic.t.d
RedChalkEast.t.d
RedChalkMain.t.d

#---------------------------------------------
# 5. Calculate Schmits stability

BlueChalk.schmidt
Chub.schmidt 
Cross.schmidt 
Dickie.schmidt 
Harp.schmidt 
Plastic.schmidt 
RedChalkEast.schmidt 
RedChalkMain.schmidt 

BlueChalk.schmidt <- c()
for (i in 1:BlueChalk.Ns){
  BlueChalk.schmidt[i] <- schmidt.stability(BlueChalk.temp[1:BlueChalk.depths,i], 1:BlueChalk.depths, bath.A[1:BlueChalk.depths,1], 1:BlueChalk.depths)
}
print(BlueChalk.schmidt)

Chub.schmidt <- c()
for (i in 1:Chub.Ns){
  Chub.schmidt[i] <- schmidt.stability(Chub.temp[1:Chub.depths,i], 1:Chub.depths, bath.A[1:Chub.depths,2], 1:Chub.depths)
}
print(Chub.schmidt)

Cross.schmidt <- c()
for (i in 1:Cross.Ns){
  Cross.schmidt[i] <- schmidt.stability(Cross.temp[1:Cross.depths,i], 1:Cross.depths, bath.A[1:Cross.depths,3], 1:Cross.depths)
}
print(Cross.schmidt)

Dickie.schmidt <- c()
for (i in 1:Dickie.Ns){
  Dickie.schmidt[i] <- schmidt.stability(Dickie.temp[1:Dickie.depths,i], 1:Dickie.depths, bath.A[1:Dickie.depths,4], 1:Dickie.depths)
}
print(Dickie.schmidt)

Harp.schmidt <- c()
for (i in 1:Harp.Ns){
  Harp.schmidt[i] <- schmidt.stability(Harp.temp[1:Harp.depths,i], 1:Harp.depths, bath.A[1:Harp.depths,5], 1:Harp.depths)
}
print(Harp.schmidt)

Plastic.schmidt <- c()
for (i in 1:Plastic.Ns){
  Plastic.schmidt[i] <- schmidt.stability(Plastic.temp[1:Plastic.depths,i], 1:Plastic.depths, bath.A[1:Plastic.depths,7], 1:Plastic.depths)
}
print(Plastic.schmidt)

RedChalkEast.schmidt <- c()
for (i in 1:RedChalkEast.Ns){
  RedChalkEast.schmidt[i] <- schmidt.stability(RedChalkEast.temp[1:RedChalkEast.depths,i], 1:RedChalkEast.depths, bath.A[1:RedChalkEast.depths,8], 1:RedChalkEast.depths)
}
print(RedChalkEast.schmidt)

RedChalkMain.schmidt <- c()
for (i in 1:RedChalkMain.Ns){
  RedChalkMain.schmidt[i] <- schmidt.stability(RedChalkMain.temp[1:RedChalkMain.depths,i], 1:RedChalkMain.depths, bath.A[1:RedChalkMain.depths,9], 1:RedChalkMain.depths)
}
print(RedChalkMain.schmidt)

write.csv(BlueChalk.schmidt, file = "BlueChalk.schmidt.csv")
write.csv(Chub.schmidt, file = "Chub.schmidt.csv")
write.csv(Cross.schmidt, file = "Cross.schmidt.csv")
write.csv(Dickie.schmidt, file = "Dickie.schmidt.csv")
write.csv(Harp.schmidt, file = "Harp.schmidt.csv")
write.csv(Plastic.schmidt, file = "Plastic.schmidt.csv")
write.csv(RedChalkEast.schmidt, file = "RedChalkEast.schmidt.csv")
write.csv(RedChalkMain.schmidt, file = "RedChalkMain.schmidt.csv")

#---------------------------------------------
# 5. Analyze Schmidt's Stability

# set working directory
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/the Lake Trout Strategic Project/temp and oxy csv data/Schmidt")

# Load temperature profiles
Schmidt <- read.csv("Schmidt's Stability lake trout lakes.csv",header=T)

Schmidt$Date <- as.Date (Schmidt$Date, "%Y-%m-%d")

head(Schmidt)
plot(Schmidt$Date, Schmidt$BC, type="p")

BC.Schmidt.lm <- lm(Schmidt$BC ~ Schmidt$Date)
summary(BC.Schmidt.lm)


#---------------------------------------------
# 5. Calculate VWHDO
# 5.1 Blue.Chalk VWHDO (try different hypolimnion criteria)

# set working directory
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/the Lake Trout Strategic Project/Bathymetric csv data")

# Load bathymetric data for lake trout lakes
bath.A <- read.csv("Bath.A.csv",header=T, row.names=1)

bath.V <- read.csv("Bath.V.csv",header=T, row.names=1)
~

# set working directory
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/the Lake Trout Strategic Project/temp and oxy csv data/VWHDO/different hypolimnion")
VHWDO.BC <- c()
for (i in 1:length(colnames(BlueChalk.oxy))){
	VHWDO.BC[i] <- sum(bath.V[17:23,1]* BlueChalk.oxy[17:23,i])/sum(bath.V[17:23,1])
}
write.csv(VHWDO.BC, file = "VHWDO.BC.16.csv")

# 5.2
VHWDO.Chub <- c()
for (i in 1:length(colnames(Chub.oxy))){
	VHWDO.Chub[i] <- sum(bath.V[6:27,2]* Chub.oxy[6:27,i])/sum(bath.V[6:27,2])
}
write.csv(VHWDO.Chub, file = "VHWDO.Chub.csv")


# 5.3
VHWDO.Cross <- c()
for (i in 1:length(colnames(Cross.oxy))){
	VHWDO.Cross[i] <- sum(bath.V[6:23,3]*Cross.oxy[7:24,i])/sum(bath.V[6:23,3])
}
write.csv(VHWDO.Cross, file = "VHWDO.Cross.csv")

# 5.4
VHWDO.Dickie <- c()
for (i in 1:length(colnames(Dickie.oxy))){
	VHWDO.Dickie[i] <- sum(bath.V[6:12,4]*Dickie.oxy[6:12,i])/sum(bath.V[6:12,4])
}
write.csv(VHWDO.Dickie, file = "VHWDO.Dickie.csv")

# 5.5 Harp Lake
VHWDO.Harp <- c()
for (i in 1:length(colnames(Harp.oxy))){
	VHWDO.Harp[i] <- sum(bath.V[10:38,5]*Harp.oxy[10:38,i])/sum(bath.V[10:38,5])
}
write.csv(VHWDO.Harp, file = "VHWDO.Harp.csv")

# 5.6 Plastic Lake
VHWDO.Plastic <- c()
for (i in 1:length(colnames(Plastic.oxy))){
	VHWDO.Plastic[i] <- sum(bath.V[12:16,7]*Plastic.oxy[12:16,i])/sum(bath.V[12:16,7])
}
write.csv(VHWDO.Plastic, file = "VHWDO.Plastic.csv")


# 5.7 Red Chalk East
VHWDO.RedChalkEast <- c()
for (i in 1:length(colnames(RedChalkEast.oxy))){
	VHWDO.RedChalkEast[i] <- sum(bath.V[10:17,9]* RedChalkEast.oxy[10:17,i])/sum(bath.V[10:17,9])
}
write.csv(VHWDO.RedChalkEast, file = "VHWDO.RedChalkEast.csv")

# 5.8 Red Chalk Main
VHWDO.RedChalkMain <- c()
for (i in 1:length(colnames(RedChalkMain.oxy))){
	VHWDO.RedChalkMain[i] <- sum(bath.V[14:37,8]* RedChalkMain.oxy[14:37,i])/sum(bath.V[14:37,8])
}
write.csv(VHWDO.RedChalkMain, file = "VHWDO.RedChalkMain.csv") 

# 6 Schmidt's Stability Analysis
# set working directory
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/the Lake Trout Strategic Project/temp and oxy csv data/Schmidt")

# Load bathymetric data for lake trout lakes
Schmidt.S <- read.csv("Schmidt's Stability lake trout lakes.csv",header=T)

colnames(Schmidt.S)

Schmidt.S$Date <- as.Date (Schmidt.S$Date, "%Y-%m-%d")

Schmidt.BC <- Schmidt.S[which(Schmidt.S$lake=="BC"),]
Schmidt.Chub <- Schmidt.S[which(Schmidt.S$lake=="Chub"),]
Schmidt.Cross <- Schmidt.S[which(Schmidt.S$lake=="Cross"),]
Schmidt.D <- Schmidt.S[which(Schmidt.S$lake=="D"),]
Schmidt.H <- Schmidt.S[which(Schmidt.S$lake=="H"),]
Schmidt.P <- Schmidt.S[which(Schmidt.S$lake=="P"),]
Schmidt.RCE <- Schmidt.S[which(Schmidt.S$lake=="RCE"),]
Schmidt.RCM <- Schmidt.S[which(Schmidt.S$lake=="RCM"),]

#--------------------------
# Plot all Schmidt's Stability Index curves
# 1 plot all in one graph
#divide the plot window into 8 frames
par(mfrow=c(2,4))

plot(Schmidt.BC[,c(1,4)], type="l") # suggested S onset stratification S=100-200
plot(Schmidt.Chub[,c(1,4)], type="l") # suggested S onset stratification S=220
plot(Schmidt.Cross[,c(1,4)], type="l") # suggested S onset stratification S=120
plot(Schmidt.D[,c(1,4)], type="l") # suggested S onset stratification S=30-40
plot(Schmidt.H[,c(1,4)], type="l") # suggested S onset stratification S=200
plot(Schmidt.P[,c(1,4)], type="l") # suggested S onset stratification S=100
plot(Schmidt.RCE[,c(1,4)], type="l") # suggested S onset S=260
plot(Schmidt.RCM[,c(1,4)], type="l") # suggested S onset stratification S=130-140

# 2. plot all in 24 graphs
# plot Schmidt curve each year all in one
# give the size of the outer margins some space for  text
par(oma=c(2,2,0,0))
#divide the plot window into 24 frames
par(mfrow=c(4,6))
par(mar=c(0,0,0,0))
axis(side=1, )

plot(Schmidt.BC[which(Schmidt.BC$Year==1990),c(3,4)],  xaxt='n', xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year==1991),c(3,4)],  xaxt='n', yaxt='n', xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year==1992),c(3,4)],  xaxt='n', yaxt='n', xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year==1993),c(3,4)],  xaxt='n', yaxt='n', xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year==1994),c(3,4)],  xaxt='n', yaxt='n', xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year==1995),c(3,4)],  xaxt='n', yaxt='n', xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year==1996),c(3,4)],  xaxt='n',xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year==1997),c(3,4)],  xaxt='n', yaxt='n',xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year==1998),c(3,4)],  xaxt='n', yaxt='n',xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year==1999),c(3,4)],  xaxt='n', yaxt='n',xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year==2000),c(3,4)],  xaxt='n', yaxt='n',xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year==2001),c(3,4)],  xaxt='n', yaxt='n', xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year== 2002),c(3,4)],  xaxt='n', xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year== 2003),c(3,4)],  xaxt='n', yaxt='n', xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year== 2004),c(3,4)],  xaxt='n', yaxt='n', xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year== 2005),c(3,4)],  xaxt='n', yaxt='n', xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year== 2006),c(3,4)],  xaxt='n', yaxt='n', xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year== 2007),c(3,4)],  xaxt='n', yaxt='n', xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year== 2008),c(3,4)],  xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year== 2009),c(3,4)],  yaxt='n', xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year==2010),c(3,4)],  yaxt='n', xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year==2011),c(3,4)],  yaxt='n', xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year==2012),c(3,4)],  yaxt='n', xlim=c(50,350), ylim=c(0,500), type="l")
plot(Schmidt.BC[which(Schmidt.BC$Year==2013),c(3,4)],  yaxt='n', xlim=c(50,350), ylim=c(0,500), type="l")
#--------------------------











#--------------------------
# Find max S
#divide the plot window into 8 frames
par(mfrow=c(2,4))
# Blue Chalk
Schmidt.BC.max<- c()
for (i in 1990:2013){
	Schmidt.BC.max[i-1989] <- max(Schmidt.BC[which(Schmidt.BC$Year==i),4], na.rm=T)
}
Schmidt.BC.max
boxplot(Schmidt.BC.max)

# Chub
Schmidt.Chub.max<- c()
for (i in 1990:2013){
	Schmidt.Chub.max[i-1989] <- max(Schmidt.Chub[which(Schmidt.Chub$Year==i),4], na.rm=T)
}
Schmidt.Chub.max
boxplot(Schmidt.Chub.max)

# Cross
Schmidt.Cross.max<- c()
for (i in 1990:2013){
	Schmidt.Cross.max[i-1989] <- max(Schmidt.Cross[which(Schmidt.Cross$Year==i),4], na.rm=T)
}
Schmidt.Cross.max
boxplot(Schmidt.Cross.max)

# Dickie
Schmidt.D.max<- c()
for (i in 1990:2013){
	Schmidt.D.max[i-1989] <- max(Schmidt.D[which(Schmidt.D$Year==i),4], na.rm=T)
}
Schmidt.D.max
boxplot(Schmidt.D.max)

# Harp
Schmidt.H.max<- c()
for (i in 1990:2007){
	Schmidt.H.max[i-1989] <- max(Schmidt.H[which(Schmidt.H$Year==i),4], na.rm=T)
}
Schmidt.H.max
boxplot(Schmidt.H.max)


# Plastic
Schmidt.P.max<- c()
for (i in 1990:2013){
	Schmidt.P.max[i-1989] <- max(Schmidt.P[which(Schmidt.P$Year==i),4], na.rm=T)
}
Schmidt.P.max
boxplot(Schmidt.P.max)

# Red Chalk East
Schmidt.RCE.max<- c()
for (i in 1990:2013){
	Schmidt.RCE.max[i-1989] <- max(Schmidt.RCE[which(Schmidt.RCE$Year==i),4], na.rm=T)
}
Schmidt.RCE.max
boxplot(Schmidt.RCE.max)


# Red Chalk Main
Schmidt.RCM.max<- c()
for (i in 1990:2013){
	Schmidt.RCM.max[i-1989] <- max(Schmidt.RCM[which(Schmidt.RCM$Year==i),4], na.rm=T)
}
Schmidt.RCM.max
boxplot(Schmidt.RCM.max)




# Find onset of thermal stratification day (e.g. S=100 or 200 etc.), calculated by two data points

# set working directory
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/the Lake Trout Strategic Project/temp and oxy csv data/Schmidt")

# Load bathymetric data for lake trout lakes
Schmidt.S <- read.csv("Schmidt's Stability lake trout lakes.csv",header=T)

colnames(Schmidt.S)

Schmidt.S$Date <- as.Date (Schmidt.S$Date, "%Y-%m-%d")

Schmidt.BC <- Schmidt.S[which(Schmidt.S$lake=="BC"),]
Schmidt.Chub <- Schmidt.S[which(Schmidt.S$lake=="Chub"),]
Schmidt.Cross <- Schmidt.S[which(Schmidt.S$lake=="Cross"),]
Schmidt.D <- Schmidt.S[which(Schmidt.S$lake=="D"),]
Schmidt.H <- Schmidt.S[which(Schmidt.S$lake=="H"),]
Schmidt.P <- Schmidt.S[which(Schmidt.S$lake=="P"),]
Schmidt.RCE <- Schmidt.S[which(Schmidt.S$lake=="RCE"),]
Schmidt.RCM <- Schmidt.S[which(Schmidt.S$lake=="RCM"),]

# Find the day of the year when onset of thermal stratfication 
Schmidt.BC.150.day <- c()
for (i in 1:24) {
	S.i <- Schmidt.BC[which(Schmidt.BC$Year==i+1989),]
	n <- length(S.i$Year)
	for (k in (n-1):1){
		if(150 %in% round(S.i$S[k]):round(S.i$S[k+1])==TRUE){
			Schmidt.BC.150.lm <- lm(S.i[c(k,k+1),4] ~ S.i[c(k,k+1),3])
			Schmidt.BC.150.day[i] <- (150 - Schmidt.BC.150.lm$coefficients[1])/Schmidt.BC.150.lm$coefficients[2]
		} else {}
		
	}
	
}

write.csv(Schmidt.BC.150.day, "MY.FILE.csv")
getwd()

# Find the day of the year when fall overturn 
Schmidt.BC.200.day <- c()
for (i in 1:24) {
	S.i <- Schmidt.BC[which(Schmidt.BC$Year==i+1989),]
	n <- length(S.i$Year)
	for (k in 1:(n-1)){
		if(200 %in% round(S.i$S[k]):round(S.i$S[k+1])==TRUE){
			Schmidt.BC.200.lm <- lm(S.i[c(k,k+1),4] ~ S.i[c(k,k+1),3])
			Schmidt.BC.200.day[i] <- (200 - Schmidt.BC.200.lm$coefficients[1])/Schmidt.BC.200.lm$coefficients[2]
		} else {}
		
	}
	
}

write.csv(Schmidt.BC.200.day, "MY.FILE.csv")
getwd()

#---------------------------
# 7 Analyze VWHDO

# set working directory
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/the Lake Trout Strategic Project/temp and oxy csv data/VWHDO")

# Load bathymetric data for lake trout lakes
VWHDO <- read.csv("VWHDO summary.csv",header=T)
head(VWHDO)
colnames(VWHDO)
VWHDO$Date <- as.Date (VWHDO$Date, "%Y-%m-%d")


# Check the highest VWHDO and the distribution of VWHDO
hist(VWHDO[which(VWHDO$Lake=="BC"),4], xlab="VWHDO mg/L", ylab="Frequency", main="Histogram of VWHDO")
# Plot all VWHDO vs Julian day to see the general pattern of the VWHDO depletion
plot(VWHDO[which(VWHDO$Lake=="BC"),c(3,4)], xlab="Day of the year", ylab="VWHDO (mg/L)", main="general pattern of VWHDO vs. Julian day", xlim=c(100,320), ylim=c(0,13))


VWHDO.BC <- VWHDO[which(VWHDO$Lake=="BC"),]

# give the size of the outer margins some space for  text
par(oma=c(2,2,0,0))
#divide the plot window into 24 frames
par(mfrow=c(4,6))
par(mar=c(0,0,0,0))
axis(side=1, )

# 1990
VWHDO.1990 <- VWHDO.BC[which(VWHDO.BC$Year==1990),]
VWHDO.1990
plot(VWHDO.1990[,c(3,4)], xlab="Year", ylab="VWHDO (mg/L)", xlim=c(100,320), ylim=c(0,13))
VWHDO.1990.de <- VWHDO.1990[-1,]
plot(VWHDO.1990.de[,c(3,4)], xlab="Year", ylab="VWHDO (mg/L)", xlim=c(100,320), ylim=c(0,13), xaxt='n', yaxt='n')
VWHDO.1990.lm <- lm(VWHDO.1990.de$VWHDO ~ VWHDO.1990.de$Day.of.the.Year)
VWHDO.1990.lm
summary(VWHDO.1990.lm)
# finite length of line using segment() function
n <- length(VWHDO.1990.de$Day.of.the.Year)
segments(VWHDO.1990.de$Day.of.the.Year[1],fitted(VWHDO.1990.lm)[1],VWHDO.1990.de$Day.of.the.Year[n], fitted(VWHDO.1990.lm)[n], col="red")
summary(VWHDO.1990.lm)
axis(2,at=c(0,5,10,15),tck=0.01)
text(250, 15, labels="1990")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)

# 1991
VWHDO.1991 <- VWHDO.BC[which(VWHDO.BC$Year==1991),]
VWHDO.1991
plot(VWHDO.1991[,c(3,4)], xlab="Year", ylab="VWHDO (mg/L)", xlim=c(100,320), ylim=c(0,13))
VWHDO.1991.de <- VWHDO.1991[-c(1,2),]
plot(VWHDO.1991.de[,c(3,4)], xlab="Year", ylab="VWHDO (mg/L)", xlim=c(100,320), ylim=c(0,13), xaxt='n', yaxt='n')
VWHDO.1991.lm <- lm(VWHDO.1991.de$VWHDO ~ VWHDO.1991.de$Day.of.the.Year)
VWHDO.1991.lm
summary(VWHDO.1991.lm)
# finite length of line using segment() function
n <- length(VWHDO.1991.de$Day.of.the.Year)
segments(VWHDO.1991.de$Day.of.the.Year[1],fitted(VWHDO.1991.lm)[1],VWHDO.1991.de$Day.of.the.Year[n], fitted(VWHDO.1991.lm)[n], col="red")
summary(VWHDO.1991.lm)
# axis(2,at=c(0,5,10,15),tck=0.01)
text(250, 15, labels="1991")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)

