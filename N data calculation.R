# Calculate Annual mean of N data (NH4, NO3, TKN) for epi, meta and hypo

setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 3 DO profile modeling/Data/! Calculated Data/N data/csv")

BC.N <- read.csv("BC-N.csv", header=T)
Chub.N <- read.csv("Chub-N.csv", header=T)
Crosson.N <- read.csv("Crosson-N.csv", header=T)
Dickie.N <- read.csv("Dickie-N.csv", header=T)
Harp.N <- read.csv("Harp-N.csv", header=T)
Plastic.N <- read.csv("Plastic-N.csv", header=T)
RCE.N <- read.csv("RCE-N.csv", header=T)
RCM.N <- read.csv("RCM-N.csv", header=T)

Secchi.N <- read.csv("Secchi.csv", header=T)

# Calculate mean value of enviornmental variables all year
 
# BC.N
BC.N.annual <- matrix(nrow=50, ncol=9)

for (i in 1:50){
	for (k in 1:9){
		BC.N.annual[i,k] <- mean(BC.N[which(BC.N$Year==i+1975),k+2], na.rm=T)
	
	}
	
}

write.csv(BC.N.annual, "BC.N.annual.csv")

# Chub.N
Chub.N.annual <- matrix(nrow=50, ncol=9)

for (i in 1:50){
	for (k in 1:9){
		Chub.N.annual[i,k] <- mean(Chub.N[which(Chub.N$Year==i+1975),k+2], na.rm=T)
	
	}
	
}

write.csv(Chub.N.annual, "Chub.N.annual.csv")


# Crosson.N
Crosson.N.annual <- matrix(nrow=50, ncol=9)

for (i in 1:50){
	for (k in 1:9){
		Crosson.N.annual[i,k] <- mean(Crosson.N[which(Crosson.N$Year==i+1975),k+2], na.rm=T)
	
	}
	
}

write.csv(Crosson.N.annual, "Crosson.N.annual.csv")

# Dickie.N
Dickie.N.annual <- matrix(nrow=50, ncol=9)

for (i in 1:50){
	for (k in 1:9){
		Dickie.N.annual[i,k] <- mean(Dickie.N[which(Dickie.N$Year==i+1975),k+2], na.rm=T)
	
	}
	
}

write.csv(Dickie.N.annual, "Dickie.N.annual.csv")

# Harp.N
Harp.N.annual <- matrix(nrow=50, ncol=9)

for (i in 1:50){
	for (k in 1:9){
		Harp.N.annual[i,k] <- mean(Harp.N[which(Harp.N$Year==i+1975),k+2], na.rm=T)
	
	}
	
}

write.csv(Harp.N.annual, "Harp.N.annual.csv")

# Plastic.N
Plastic.N.annual <- matrix(nrow=50, ncol=9)

for (i in 1:50){
	for (k in 1:9){
		Plastic.N.annual[i,k] <- mean(Plastic.N[which(Plastic.N$Year==i+1975),k+2], na.rm=T)
	
	}
	
}

write.csv(Plastic.N.annual, "Plastic.N.annual.csv")


# RCE.N
RCE.N.annual <- matrix(nrow=50, ncol=9)

for (i in 1:50){
	for (k in 1:9){
		RCE.N.annual[i,k] <- mean(RCE.N[which(RCE.N$Year==i+1975),k+2], na.rm=T)
	
	}
	
}

write.csv(RCE.N.annual, "RCE.N.annual.csv")

# RCM.N
RCM.N.annual <- matrix(nrow=50, ncol=9)

for (i in 1:50){
	for (k in 1:9){
		RCM.N.annual[i,k] <- mean(RCM.N[which(RCM.N$Year==i+1975),k+2], na.rm=T)
	
	}
	
}

write.csv(RCM.N.annual, "RCM.N.annual.csv")

