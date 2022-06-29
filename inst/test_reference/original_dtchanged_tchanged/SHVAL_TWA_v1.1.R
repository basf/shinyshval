# Project: SHVAL tool TWA
# 
# Author: EFSA-AMU-zancaga
###############################################################################

# LOAD REQUIRED PACKAGES #
# install.packages("VGAM", repos="http://cran.at.r-project.org/", dependencies=TRUE)
library(VGAM)
# install.packages("fitdistrplus", repos="http://cran.at.r-project.org/", dependencies=TRUE)
library(fitdistrplus)
# install.packages("mc2d", repos= "http://cran.at.r-project.org/", dependencies= TRUE)
library(mc2d)
# install.packages("rriskDistributions", repos="http://cran.at.r-project.org/", dependencies= TRUE)
library(rriskDistributions)
# install.packages("MASS", repos="http://cran.at.r-project.org/", dependencies= TRUE)
library(MASS)

################### WARNING ###############################################################################
## PATH VARIABLE. To be adapted according to the local situation

# adapt <- "/home/edisz/gitlab/shval/pkg/inst/test_reference/original/twa/"

################################################################
# The root as indicated between quotes ("H:/") must be changed according to the local situation
# e.g. in case the user has Administrator rights, the variable can assume value "C:/"
# In case the user has no Administrator rights, the path must be an accessible drive/folder
# (maybe created ad hoc by IT)
###########################################################################################################

# # CREATING DIRECTORIES #
# dir.create(file.path(paste(adapt,"SHVALoutput",sep="")), showWarnings = FALSE)
# setwd(file.path(paste(adapt,"SHVALoutput",sep="")))


# DATA UPLOAD #
rawSVdata <- read.csv(system.file("test_reference/original_dtchanged_tchanged/original_dtchanged_tchanged.csv", package = "shinyshval"), header=T)


# SETTING THE PARAMETERS
alpha <- 0.01
sim <- 1000
iter <- 1000

###############################################################################################
# FITTING A PROBABILISTIC DISTRIBUTION WHERE POSSIBLE
###############################################################################################

# Matrix to store the estimated moments/parameters
momnts <- matrix(NA, ncol=3, nrow=5)

# RUD (Pollen & Nectar); CONSUMPTION (pollen & sugar)
# 1) start

for (i in 1:4) {	# IF raw data are available & more than 2 values
	if ( ( sum(is.na(rawSVdata[,i])) != length(rawSVdata[,i]) ) & sum(!is.na(rawSVdata[,i]))>2 ) {	
		stp <- fitdist(as.vector(as.numeric(na.omit(rawSVdata[,i]))), "norm", optim.method="L-BFGS-B")
		momnts[i,] 	<- c(as.vector(stp$estimate[1]),as.vector(stp$estimate[2]),NA)  
	} 
	else {			# Raw data are NOT available, but we have mean and StDev
		if (!is.na(rawSVdata[4,i+6])) {  	
		momnts[i,] <- c(rawSVdata[3,i+6], rawSVdata[4,i+6], NA)
		} 
		else {		# MIN, MAX (and Mean) are available
			if (!is.na(rawSVdata[1,i+6])&!is.na(rawSVdata[2,i+6]))  { 
				aver <- (rawSVdata[1,i+6]+rawSVdata[2,i+6])/2
				momnts[i,] <- c(aver,(aver-rawSVdata[1,i+6])/qnorm(1-alpha),NA)
			}
			else { 	# The best guess (mean or median) is the only info available
				momnts[i,] <- c(0,0,NA)
			}}}
} 
# 1) end 

# SUGAR CONTENT
# 2) start
(	# IF raw data are available AND more than 2 records
if ( (sum(is.na(rawSVdata[,5]))!=length(rawSVdata[,5])) & sum(!is.na(rawSVdata[,5]))>2 ) {
		stp <- fitdist(as.vector(as.numeric(na.omit(rawSVdata[,5]))), "beta", optim.method="L-BFGS-B", lower=c(0,0))
		betapar	<- c(as.vector(stp$estimate[1]),as.vector(stp$estimate[2]),NA)
		momnts[5,1] <- betapar[1]
		momnts[5,2] <- betapar[2]  
} 
else {	# IF min, max & BG are available
	if (!is.na(rawSVdata[1,11])&!is.na(rawSVdata[2,11])&!is.na(rawSVdata[3,11])) {
		betapar <- get.beta.par(p=c(0.01,0.5,0.99), q=c(rawSVdata[1,11],rawSVdata[3,11],rawSVdata[2,11]), 
				show.output = FALSE, plot = FALSE)
		momnts[5,1] <- betapar[1]
		momnts[5,2] <- betapar[2]
} 
else {	# IF min & max are available
	if (!is.na(rawSVdata[1,11])&!is.na(rawSVdata[2,11])) {		
		betapar <- get.beta.par(p=c(0.01,0.99), q=c(rawSVdata[1,11],rawSVdata[2,11]), 
				show.output = FALSE, plot = FALSE)
		momnts[5,1] <- betapar[1]
		momnts[5,2] <- betapar[2]
} 
else {	# When only the best guess is available
	momnts[5,1] <- 0
	momnts[5,2] <- 0
}}}
)


# 2) end
## END OF THE FITTING EXERCISE

#######################################################################################################
# VISUAL REPRESENTATION of the FITTED DISTRIBUTION (where available)
#######################################################################################################

varnms <- c("RUD pollen", "RUD nectar", "Pollen consumption", "Sugar consumption", "Sugar content")

# RUD's and CONSUMPTION
# 3) start
for (i in 1:4) {
if ( (sum(is.na(rawSVdata[,i])) != length(rawSVdata[,i])) & sum(!is.na(rawSVdata[,i]))>2 ) {
	pdf(file=paste(varnms[i],".pdf"))
	hst <- hist(rawSVdata[,i], plot=FALSE)
	hist(rawSVdata[,i], ylim=c(0,max(hst$density)), 
		freq=FALSE, 
		xlab=paste(varnms[i],"value"),main=paste(varnms[i],"\n fitted distribution"))
	x <- (min(na.omit(rawSVdata[,i]))+0.5*min(na.omit(rawSVdata[,i]))):(max(na.omit(rawSVdata[,i]))+0.5*max(na.omit(rawSVdata[,i])))
	curve(dnorm(x, mean=momnts[i,1],sd=momnts[i,2]),
		from=(min(na.omit(rawSVdata[,i]))), #+0.5*min(na.omit(rawSVdata[,i]))),
		to=(max(na.omit(rawSVdata[,i]))), #+0.5*max(na.omit(rawSVdata[,i]))),
		add=TRUE, lwd=2, col="blue")
	leg.txt <- c("Distribution: Normal",
			paste("Mean =",round(momnts[i,1],2)),
			paste("St Dev =", round(momnts[i,2],2)))
	legend("topright", legend=leg.txt, bty="n", cex=.8)
	dev.off()
} 
else {
	if (!is.na(rawSVdata[1,i+6])&!is.na(rawSVdata[2,i+6])) {
	x <- (momnts[i,1]-qnorm(0.99)*momnts[i,2]):(momnts[i,1]+qnorm(0.99)*momnts[i,2])
	pdf(file=paste(varnms[i],".pdf"))
	crv <- dnorm(x, mean=momnts[i,1],sd=momnts[i,2])
	curve(dnorm(x, mean=momnts[i,1],sd=momnts[i,2]),
			from=(momnts[i,1]-qnorm(0.99)*momnts[i,2]),
			to=(momnts[i,1]+qnorm(0.99)*momnts[i,2]),
			ylim=c(0,max(crv)), lwd=2, col="blue",
			ylab="Density", xlab=paste(varnms[i],"value"),main=paste(varnms[i],"\n fitted distribution"))
	leg.txt <- c("Distribution: Normal",paste("Mean =",round(momnts[i,1],2)),paste("St Dev =", round(momnts[i,2],2)))
	legend("topleft",
			legend=leg.txt, bty="n", cex=.8)
	dev.off()
} else {
	x <- seq(0,1,0.01)
	y <- seq(0,1,0.01)
	pdf(file=paste(varnms[i],".pdf"))
	plot(x,y, xaxt="n", yaxt="n", ylab="", xlab="", type="n")
	leg.txt <- c("Input is a single point estimate \n No distribution fitted")
	legend("center", legend=leg.txt, bty="n", cex=.8)
	title(main=paste(varnms[i]))
	title(xlab=paste(varnms[i],"value"))
	dev.off()
}}
} 
# 3) end

# SUGAR CONTENT
# 4) start
(	# IF raw data are available AND more than 2 records
if ( sum(is.na(rawSVdata[,5]))!=length(rawSVdata[,5]) & sum(!is.na(rawSVdata[,5]))>2 ) {
	pdf(file=paste(varnms[5],".pdf"))
	hst <- hist(rawSVdata[,5], plot=FALSE)
	hist(rawSVdata[,5], ylim=c(0,max(hst$density)), 
			freq=FALSE, 
			xlab=paste(varnms[5],"value"),main=paste(varnms[5],"\n fitted distribution"))
	x <- (min(na.omit(rawSVdata[,5]))+0.5*min(na.omit(rawSVdata[,5]))):(max(na.omit(rawSVdata[,5]))+0.5*max(na.omit(rawSVdata[,5])))
	curve(dbeta(x, shape1=momnts[5,1],shape2=momnts[5,2]),
			from=(min(na.omit(rawSVdata[,5]))), #+0.5*min(na.omit(rawSVdata[,5]))),
			to=(max(na.omit(rawSVdata[,5]))), #+0.5*max(na.omit(rawSVdata[,5]))),
			add=TRUE, lwd=2, col="blue")
	leg.txt <- c("Distribution: Beta",
			paste("Shape1 =",round(momnts[5,1],2)),
			paste("Shape2 =", round(momnts[5,2],2)))
	legend("topright", legend=leg.txt, bty="n", cex=.8)
	dev.off()
} 
else {
	if ( momnts[5,1]!=0 & momnts[5,2]!=0 ) {
	x <- seq(0,1,0.01)
	pdf(file=paste(varnms[5],".pdf"))
	crv <- dbeta(x, shape1=momnts[5,1], shape2=momnts[5,2])
	curve(dbeta(x, shape1=momnts[5,1], shape2=momnts[5,2]),
			from=0,	to=1,
			ylim=c(0,max(crv)), lwd=2, col="blue",
			ylab="", xlab="")
	leg.txt <- c("Distribution: Beta",
			paste("Shape1 =",  round(momnts[5,1],2)),
			paste("Shape2 =", round(momnts[5,2],2))
			)
	legend("topright", legend=leg.txt, bty="n", cex=.8)
	title(main=paste(varnms[5],"\n fitted distribution"))
	title(xlab=paste(varnms[5],"value"))
	title(ylab="Density")
	dev.off()
}
else {
	x <- seq(0,1,0.01)
	y <- seq(0,1,0.01)
	pdf(file=paste(varnms[5],".pdf"))
	plot(x,y, ylab="", xlab="", type="n")
	points(rawSVdata[3,11],1, pch=20)
	leg.txt <- c("Input is a single point estimate \n No distribution fitted")
	legend("center", legend=leg.txt, bty="n", cex=.8)
	title(main=paste(varnms[5]))
	dev.off()
}}
)
# 4) end


##############################################################################################
# DRAWING VALUES FROM THE FITTED DISTRIBUTION (where available)
##############################################################################################
sim  <-  sim

# Simulation of the CONS values and calculation of the median from each simulation

simfit_CONSp	<- (if (momnts[3,1]!=0 & momnts[3,2]!=0)
			{set.seed(123456); apply(matrix(rnorm(sim*sim, mean=momnts[3,1], sd=momnts[3,2]),nrow=sim, ncol=sim, byrow=TRUE),2,median)}
			else {rep(rawSVdata[3,9],sim)})

simfit_CONSs	<- (if (momnts[4,1]!=0 & momnts[4,2]!=0)
			{set.seed(654321); apply(matrix(rnorm(sim*sim, mean=momnts[4,1], sd=momnts[4,2]), nrow=sim, ncol=sim, byrow=TRUE), 2, median)}
			else {rep(rawSVdata[3,10],sim)})

SVs <- NULL
SV <- NULL
for(j in 1:sim) {
	
simfit_RUDp 	<- (if (momnts[1,1]!=0 & momnts[1,2]!=0) 
			{set.seed(123456+j); exp(rnorm(sim, mean=momnts[1,1], sd=momnts[1,2]))} 
			else {rep(exp(rawSVdata[3,7]),sim)})
	
simfit_RUDn 	<- (if (momnts[2,1]!=0 & momnts[2,2]!=0)
			{set.seed(654321+j); exp(rnorm(sim, mean=momnts[2,1], sd=momnts[2,2]))}
			else {rep(exp(rawSVdata[3,8]),sim)})	
	
simfit_CONTs	<- (if (momnts[5,1]!=0 & momnts[5,2]!=0)
			{set.seed(13579+j); rbeta(sim, shape1=momnts[5,1], shape2=momnts[5,2])}
			else {rep(rawSVdata[3,11],sim)})

	kp <- log(2)/rawSVdata$DT50_p[1]
	
	kn <- log(2)/rawSVdata$DT50_n[1]
	
	tlag <- rawSVdata$t[1]
	
	SVs <- (1/1000)*(  ((simfit_RUDp*simfit_CONSp/(kp*tlag))*(1-exp(-kp*tlag))) 
				+ ((simfit_RUDn*(simfit_CONSs/simfit_CONTs)/(kn*tlag))*(1-exp(-kn*tlag))) )
	temp <- quantile(SVs, probs=0.9)
	SV <- c(SV,temp)
}


simval <- cbind(simfit_RUDp,simfit_RUDn,simfit_CONSp,simfit_CONSs,simfit_CONTs,kp,kn,tlag)
#head(simval)

# #############################################################################
# # PLOTTING THE 90th PERCENTILE VARIABILITY ACROSS THE SIMULATIONS
# #############################################################################

# fdnorm <- fitdistr(SV, "normal")
# hpt <- hist(SV, plot=FALSE)
# yran <- range(hpt$density)
# x <- hpt$breaks
# addval <- 0.2*(yran[2]-yran[1])/yran[2]


# pdf(file="90thSV.pdf")

# hpt <- hist(SV, freq=FALSE, , breaks="FD",  col="white", border="white", 
# 		main="", xlab="", ylim=c(yran[1],yran[2]+addval))
# rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey90")
# hist(SV, freq=FALSE, , breaks="FD",
# 		col="firebrick3", add=TRUE, main="")
# curve(dnorm(x, mean=fdnorm$est[1],sd=fdnorm$est[2]),
# 		add=TRUE, lwd=2, col="blue")

qls <- as.vector(quantile(SV, probs=c(0.025,0.5,0.975)))
# abline(v=c(qls[1],qls[2],qls[3]), lty=2, lwd=2)
# axis(3, at=c(qls[1],qls[3]), line=-1, labels=c(round(qls[1],1),round(qls[3],1)), font.axis=2, cex.axis=.9)
# rect(qls[1], par("usr")[4]-par("usr")[4]*0.03, qls[3], par("usr")[4], col="black")
# text(qls[2],par("usr")[4]-par("usr")[4]*0.013, labels="95%", col="white", cex=0.7, font=2)
# axis(3, at=qls[2], line=-1, labels=round(qls[2],digits=1), col.axis="darkblue", font.axis=4)
# title(main=paste("TWA Shortcut Values' 90th percentile"), cex.main=0.95)
# title(xlab="90th percentile")
# leg.txt <- c(paste("Iterations =",iter),
# 			 paste("Simulations =",sim))
# legend("topright", legend=leg.txt, bty="n", cex=.8)

# dev.off()

# # Show it now

# hpt <- hist(SV, freq=FALSE, , breaks="FD",  col="white", border="white", 
# 		main="", xlab="", ylim=c(yran[1],yran[2]+addval))
# rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey90")
# hist(SV, freq=FALSE, , breaks="FD",
# 		col="firebrick3", add=TRUE, main="")
# curve(dnorm(x, mean=fdnorm$est[1],sd=fdnorm$est[2]),
# 		add=TRUE, lwd=2, col="blue")

# qls <- as.vector(quantile(SV, probs=c(0.025,0.5,0.975)))
# abline(v=c(qls[1],qls[2],qls[3]), lty=2, lwd=2)
# axis(3, at=c(qls[1],qls[3]), line=-1, labels=c(round(qls[1],1),round(qls[3],1)), font.axis=2, cex.axis=.9)
# rect(qls[1], par("usr")[4]-par("usr")[4]*0.03, qls[3], par("usr")[4], col="black")
# text(qls[2],par("usr")[4]-par("usr")[4]*0.013, labels="95%", col="white", cex=0.7, font=2)
# axis(3, at=qls[2], line=-1, labels=round(qls[2],digits=1), col.axis="darkblue", font.axis=4)
# title(main=paste("TWA Shortcut Values' 90th percentile"), cex.main=0.95)
# title(xlab="90th percentile")
# leg.txt <- c(paste("Iterations =",sim),
# 		paste("Simulations =",sim))
# legend("topright", legend=leg.txt, bty="n", cex=.8)
# # stop

paste("The 90th percentile for this particular Shortcut Value is",round(qls[2], digits=3),"(CI=",round(qls[1],digits=1),"-",round(qls[3],digits=1),", CL=95%)")

