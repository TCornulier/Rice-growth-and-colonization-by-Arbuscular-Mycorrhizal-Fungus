# Slightly cleaned-up GitHub version of the script used for the paper
# https://www.frontiersin.org/articles/10.3389/fpls.2019.00633/full
# (although not very clean at all. Includes all the graphical and 
# model-based exploration that was not used in the paper)
# Code for Fig 3 is at line 278
# Code for Fig 6 is at line 856
# Other figures drawn by other authors.

library(Cairo)
library(mgcv)
library(lattice)
library(ggplot2)

r1<- read.delim("EURoot Big experiment for stats_Rep1_AMF.txt")[, 1:19]
r2<- read.delim("EURoot Big experiment for stats_Rep2_AMF.txt")[, 1:19]
r3<- read.delim("EURoot Big experiment for stats_Rep3_AMF.txt")[, 1:19]
r4<- read.delim("EURoot Big experiment for stats_Rep4_AMF.txt")[, 1:19]

dat<- rbind(r1, r2, r3, r4)
dat$Ext<- as.numeric(dat$External.edge == "Y")
dat$Int<- as.numeric(dat$Internal.edge == "Y")
dat$Xold<- dat$X
dat$X<- as.numeric(dat$X)
dat$Position<- 0
dat$Position[dat$Treatment.position == "Middle"]<- 1
dat$Position[dat$Treatment.position == "North"]<- 2
dat$Ycoord<- dat$Position * 17 + dat$Y
dat$Xcoord<- max(dat$X + 1) - dat$X # revert X so it increases West to East
dat$Edge<- as.numeric(dat$X %in% c(1, 20) | dat$Y %in% c(1, 17))
dat$East<- as.numeric(dat$Xold == "A")
dat$West<- as.numeric(dat$Xold == "V")
dat$North<- as.numeric(dat$Y == 17)
dat$South<- as.numeric(dat$Y == 1)
dat$Block<- factor(paste(dat$Treatment.ID, dat$Run, sep= "_"))
dat$Box<- "Box1"
dat$Box[dat$Run %in% c(2, 4)]<- "Box2"
dat$Box<- as.factor(dat$Box)
dat$Treatment.ID<- relevel(dat$Treatment.ID, ref= "None")
dat$T.None<- (dat$Treatment.ID == "None") + 0
dat$T.AMF<- (dat$Treatment.ID == "AMF+RP") + 0
dat$T.RP<- (dat$Treatment.ID == "RP") + 0

dat$SDW.N<- NA
dat$SDW.S<- NA
dat$SDW.E<- NA
dat$SDW.W<- NA
dat$SDW.NE<- NA
dat$SDW.SE<- NA
dat$SDW.SW<- NA
dat$SDW.NW<- NA

for(i in 1:nrow(dat)){
	bla<- dat$SDW[dat$Run == dat$Run[i] & dat$X == dat$X[i] & dat$Ycoord == (dat$Ycoord[i] - 1)]
	bla<- ifelse(is.na(bla), 0, bla)
	dat$SDW.S[i]<- ifelse(length(bla) == 0, 0, bla)
	
	bla<- dat$SDW[dat$Run == dat$Run[i] & dat$X == dat$X[i] & dat$Ycoord == (dat$Ycoord[i] + 1)]
	bla<- ifelse(is.na(bla), 0, bla)
	dat$SDW.N[i]<- ifelse(length(bla) == 0, 0, bla)

	bla<- dat$SDW[dat$Run == dat$Run[i] & dat$X == (dat$X[i] - 1) & dat$Ycoord == (dat$Ycoord[i])]
	bla<- ifelse(is.na(bla), 0, bla)
	dat$SDW.E[i]<- ifelse(length(bla) == 0, 0, bla)
	
	bla<- dat$SDW[dat$Run == dat$Run[i] & dat$X == (dat$X[i] + 1) & dat$Ycoord == (dat$Ycoord[i])]
	bla<- ifelse(is.na(bla), 0, bla)
	dat$SDW.W[i]<- ifelse(length(bla) == 0, 0, bla)
	
	
	bla<- dat$SDW[dat$Run == dat$Run[i] & dat$X == (dat$X[i]-1) & dat$Ycoord == (dat$Ycoord[i]+1)]
	bla<- ifelse(is.na(bla), 0, bla)
	dat$SDW.NE[i]<- ifelse(length(bla) == 0, 0, bla)

	bla<- dat$SDW[dat$Run == dat$Run[i] & dat$X == (dat$X[i]-1) & dat$Ycoord == (dat$Ycoord[i]-1)]
	bla<- ifelse(is.na(bla), 0, bla)
	dat$SDW.SE[i]<- ifelse(length(bla) == 0, 0, bla)
	
	bla<- dat$SDW[dat$Run == dat$Run[i] & dat$X == (dat$X[i]+1) & dat$Ycoord == (dat$Ycoord[i]-1)]
	bla<- ifelse(is.na(bla), 0, bla)
	dat$SDW.SW[i]<- ifelse(length(bla) == 0, 0, bla)
	
	bla<- dat$SDW[dat$Run == dat$Run[i] & dat$X == (dat$X[i]+1) & dat$Ycoord == (dat$Ycoord[i]+1)]
	bla<- ifelse(is.na(bla), 0, bla)
	dat$SDW.NW[i]<- ifelse(length(bla) == 0, 0, bla)
}

#dput(dat, "formatted_data.txt")
#dat<- dget("formatted_data.txt")

## carry out some checks:
#par(mfrow= c(1, 4))
#by(dat, dat$Run, function(x) plot(x$X, x$Ycoord, pch= 16, col= (x$SDW.S>0)+1))
#by(dat, dat$Run, function(x) plot(x$X, x$Ycoord, pch= 16, col= (x$SDW.N>0)+1))
#by(dat, dat$Run, function(x) plot(x$X, x$Ycoord, pch= 16, col= (x$SDW.E>0)+1))
#by(dat, dat$Run, function(x) plot(x$X, x$Ycoord, pch= 16, col= (x$SDW.W>0)+1))
#
#par(mfrow= c(3, 3), mar= c(0.2, 0.2, 3.1, 0.2))
#R1<- dat[dat$Run == 1, ]
#inc<- 0.03
#symbols(R1$X, R1$Ycoord, circles= R1$SDW, fg= grey(0.4), bg= grey(0.4), inches= inc)
#symbols(R1$X, R1$Ycoord, circles= R1$SDW.N, fg= grey(0.4), bg= grey(0.4), inches= inc,  main= "N")
#symbols(R1$X, R1$Ycoord, circles= R1$SDW.S, fg= grey(0.4), bg= grey(0.4), inches= inc,  main= "S")
#symbols(R1$X, R1$Ycoord, circles= R1$SDW.E, fg= grey(0.4), bg= grey(0.4), inches= inc,  main= "E")
#symbols(R1$X, R1$Ycoord, circles= R1$SDW.W, fg= grey(0.4), bg= grey(0.4), inches= inc,  main= "W")
#symbols(R1$X, R1$Ycoord, circles= R1$SDW.NE, fg= grey(0.4), bg= grey(0.4), inches= inc, main= "NE")
#symbols(R1$X, R1$Ycoord, circles= R1$SDW.SE, fg= grey(0.4), bg= grey(0.4), inches= inc, main= "SE")
#symbols(R1$X, R1$Ycoord, circles= R1$SDW.SW, fg= grey(0.4), bg= grey(0.4), inches= inc, main= "SW")
#symbols(R1$X, R1$Ycoord, circles= R1$SDW.NW, fg= grey(0.4), bg= grey(0.4), inches= inc, main= "NW")
#
#par(mfrow= c(1, 4), mar= c(0.2, 0.2, 3.1, 0.2))
#inc<- 0.05
#symbols(R1$X, R1$Ycoord, circles= R1$SDW, fg= grey(0.4), bg= grey(0.4), inches= inc)
#points(R1$X, R1$Ycoord, cex= 1.5, col= ifelse(R1$Ext==1, 2, 0))
#points(R1$X, R1$Ycoord, cex= 1.5, col= ifelse(R1$Int==1, 3, 0))
#symbols(R1$X, R1$Ycoord, circles= R1$SDW.E, fg= grey(0.4), bg= grey(0.4), inches= inc,  main= "E")
#points(R1$X, R1$Ycoord, cex= 1.5, col= ifelse(R1$Ext==1, 2, 0))
#points(R1$X, R1$Ycoord, cex= 1.5, col= ifelse(R1$Int==1, 3, 0))
#symbols(R1$X, R1$Ycoord, circles= R1$SDW.NE, fg= grey(0.4), bg= grey(0.4), inches= inc, main= "NE")
#points(R1$X, R1$Ycoord, cex= 1.5, col= ifelse(R1$Ext==1, 2, 0))
#points(R1$X, R1$Ycoord, cex= 1.5, col= ifelse(R1$Int==1, 3, 0))
#symbols(R1$X, R1$Ycoord, circles= R1$SDW.SE, fg= grey(0.4), bg= grey(0.4), inches= inc, main= "SE")
#points(R1$X, R1$Ycoord, cex= 1.5, col= ifelse(R1$Ext==1, 2, 0))
#points(R1$X, R1$Ycoord, cex= 1.5, col= ifelse(R1$Int==1, 3, 0))

x.coord<- rep(1:3, each= 3) 
y.coord<- rep(1:3, times= 3)

Neighmat<- as.matrix(dat[, c("SDW.N", "SDW.NE", "SDW.E", "SDW.SE", "SDW.S", "SDW.SW", "SDW.W", "SDW.NW")])
Neigh.ID<- matrix(1:8, nrow(dat), 8, byrow= T)
dat$Neigh.SDW<- NA
dat$Neigh.SDW[dat$Ext != 1]<- apply(Neighmat[dat$Ext != 1, ], 1, mean, na.rm= T)
dat$Neigh.SDW[dat$Ext == 1]<- apply(Neighmat[dat$Ext == 1, ], 1, function(x) mean(x[x != 0], na.rm= T))

Genot<- factor(dat$AP.Variety[!is.na(dat$SDW)])
SDW<- dat$SDW[!is.na(dat$SDW)]

##################### Map of experimental design ###################
p<- ggplot(dat, aes(x= Xcoord, y= Ycoord, colour= Treatment.ID)) +
	geom_point()+
	facet_grid(.~Run) +
	theme_light()
p
ggsave("DesignMap.pdf", width= 21, height= 12, units= "cm")

p<- ggplot(dat, aes(x= Xcoord, y= Ycoord, colour= North)) +
	geom_point()+
	facet_grid(.~Run) +
	theme_light()
p

##################### SDW models ####################

# test for genotype*Treatment interactions
# more complex models don't converge with the Genotype*Treatment random effect; Trying to simplify
system.time(mod100<- gamm(log(SDW) ~ factor(Run) + Treatment.ID + Ext + Int + Neigh.SDW + s(Xcoord, Ycoord, by= Box), random= list(AP.Variety=~ 1 + Treatment.ID), family= gaussian, data=dat))
summary(mod100$lme)
summary(mod100$gam)
gam.check(mod100$gam)
dev.new()
par(mfrow= c(1, 5)); plot(mod100$gam, scheme= c(1, 2, 2, 2, 2), n2= 100)
# try with gamm4
library(gamm4)
noNA.subs<- !is.na(dat$SDW)
dat$lSDW<- log(dat$SDW)
system.time(mod100<- gamm4(log(SDW) ~ factor(Run) * Treatment.ID + Edge + Ext * Neigh.SDW + s(Xcoord, Ycoord, by= Box), random= ~ (1 + Treatment.ID | AP.Variety), family= gaussian, data=dat[noNA.subs, ]))
summary(mod100$mer)
summary(mod100$gam)
gam.check(mod100$gam)
dev.new()
par(mfrow= c(1, 2)); plot(mod100$gam, scheme= c(2, 2), n2= 100)


system.time(mod101<- gamm4(log(SDW) ~ factor(Run) * Treatment.ID + Edge + Ext * Neigh.SDW + Neigh.SDW : T.RP + Neigh.SDW : T.AMF + T.None:SDW.N + T.None:SDW.E + T.None:SDW.S + T.None:SDW.W + T.None:SDW.NE + T.None:SDW.SE + T.None:SDW.SW + T.None:SDW.NW + s(Xcoord, Ycoord, by= Box), random= ~ (1 | Subgroup) + (1 | AP.Variety), family= gaussian, data=dat[noNA.subs, ]))
summary(mod101$mer)
summary(mod101$gam)
dev.new()
gam.check(mod101$gam)
dev.new()
par(mfrow= c(1, 2)); plot(mod101$gam, scheme= c(2, 2), n2= 100)


system.time(mod102<- gamm4(log(SDW) ~ factor(Run) * Treatment.ID + Edge + Ext * Neigh.SDW + Neigh.SDW : T.RP + Neigh.SDW : T.AMF + T.None:I(SDW.N+SDW.S) + T.None:I(SDW.E+SDW.W) + T.None:I(SDW.NE+SDW.SW) + T.None:I(SDW.SE+SDW.NW) + s(Xcoord, Ycoord, by= Box), random= ~ (1 | Subgroup) + (1 | AP.Variety), family= gaussian, data=dat[noNA.subs, ]))


system.time(mod103<- gamm4(log(SDW) ~ factor(Run) * Treatment.ID + Edge + Ext * Neigh.SDW + Neigh.SDW : T.RP + Neigh.SDW : T.AMF + T.None:I(SDW.N+SDW.S) + s(Xcoord, Ycoord, by= Box), random= ~ (1 | Subgroup) + (1 | AP.Variety), family= gaussian, data=dat[noNA.subs, ]))
summary(mod103$mer)
summary(mod103$gam)
dev.new()
gam.check(mod103$gam)
dev.new()
par(mfrow= c(1, 2))
plot(mod103$gam, scheme= 2, select= 1, n2= 100, main= "Box 1")
abline(h= c(17.5, 17.5*2))
plot(mod103$gam, scheme= 2, select= 2, n2= 100, main= "Box 2")
abline(h= c(17.5, 17.5*2))

system.time(mod104<- gamm4(log(SDW) ~ factor(Run) * Treatment.ID + Edge + Ext * Neigh.SDW * Treatment.ID + s(Xcoord, Ycoord, by= Box), random= ~ (1 | Subgroup) + (1 | AP.Variety), family= gaussian, data=dat[noNA.subs, ]))


BIC(mod101$mer)
BIC(mod102$mer) # lower
BIC(mod103$mer) # by far lowest
BIC(mod104$mer) # not as good -> evidence for a directional (North-South) effect of neighbours in the control treatment

# select the best way of modelling spatial trends:
system.time(mod103.mean<- gamm4(log(SDW) ~ factor(Run) * Treatment.ID + Edge + Ext * Neigh.SDW + Neigh.SDW : T.RP + Neigh.SDW : T.AMF + T.None:I(SDW.N+SDW.S) + s(Xcoord, Ycoord), random= ~ (1 | Subgroup) + (1 | AP.Variety), family= gaussian, data=dat[noNA.subs, ]))
dev.new()
par(mfrow= c(1, 1)); plot(mod103.mean$gam, scheme= c(2), n2= 100, main= "Average trend")
abline(h= c(17.5, 17.5*2))

system.time(mod103.Run<- gamm4(log(SDW) ~ factor(Run) * Treatment.ID + Edge + Ext * Neigh.SDW + Neigh.SDW : T.RP + Neigh.SDW : T.AMF + T.None:I(SDW.N+SDW.S) + s(Xcoord, Ycoord, by= factor(Run)), random= ~ (1 | Subgroup) + (1 | AP.Variety), family= gaussian, data=dat[noNA.subs, ]))
dev.new()
par(mfrow= c(1, 4))
plot(mod103.Run$gam, scheme= 2, select= 1, n2= 100, main= "Run 1 (Box 1)")
abline(h= c(17.5, 17.5*2))
plot(mod103.Run$gam, scheme= 2, select= 2, n2= 100, main= "Run 2 (Box 2)")
abline(h= c(17.5, 17.5*2))
plot(mod103.Run$gam, scheme= 2, select= 3, n2= 100, main= "Run 3 (Box 1)")
abline(h= c(17.5, 17.5*2))
plot(mod103.Run$gam, scheme= 2, select= 4, n2= 100, main= "Run 4 (Box 2)")
abline(h= c(17.5, 17.5*2))

system.time(mod103.Block<- gamm4(log(SDW) ~ factor(Run) * Treatment.ID + Edge + Ext * Neigh.SDW + Neigh.SDW : T.RP + Neigh.SDW : T.AMF + T.None:I(SDW.N+SDW.S) + s(Xcoord, Y, by= Block), random= ~ (1 | Subgroup) + (1 | AP.Variety), family= gaussian, data=dat[noNA.subs, ]))
dev.new()
par(mfrow= c(3, 4)); plot(mod103.Block$gam, scheme= c(2), n2= 100)


BIC(mod103$mer) 		# -714.3 # Box good
BIC(mod103.mean$mer) 	# -731.5 # average best
BIC(mod103.Run$mer) 	# -663.8 # Run overparameterized
BIC(mod103.Block$mer) 	# -429.1 # very poor




# re-visit the neighbour effect directionality with the best spatial model:
dat.sub<- dat[noNA.subs, ]
dat.sub$fRun<- factor(dat.sub$Run)
Neigh.ID.sub<- Neigh.ID[noNA.subs, ]
Neighmat.None.sub<- Neighmat.None[noNA.subs, ]
Neighmat.RP.sub<- Neighmat.RP[noNA.subs, ]
Neighmat.AMF.sub<- Neighmat.AMF[noNA.subs, ]

system.time(mod105<- gamm4(log(SDW) ~ fRun * Treatment.ID + Edge + Ext + Ext : Neigh.SDW + Neigh.SDW : T.RP + Neigh.SDW : T.AMF + Neigh.SDW : T.None + s(Neigh.ID.sub, by= Neighmat.None.sub, bs= "cc", k= 6) + s(Neigh.ID.sub, by= Neighmat.AMF.sub, bs= "cc", k= 6) + s(Neigh.ID.sub, by= Neighmat.RP.sub, bs= "cc", k= 6) + s(Xcoord, Ycoord), random= ~ (1 | Subgroup) + (1 | AP.Variety), family= gaussian, data=dat.sub))
summary(mod105$gam)
dev.new()
par(mfrow= c(1, 4))
plot(mod105$gam, scheme= c(1, 1, 1, 2), n2= 100, main= "")
abline(h= c(17.5, 17.5*2))

# no evidence for interaction Ext:Neigh.SDW -> replace with Edge:Neigh.SDW

system.time(mod106<- gamm4(log(SDW) ~ fRun * Treatment.ID + Edge + Ext + Edge : Neigh.SDW + Neigh.SDW : T.RP + Neigh.SDW : T.AMF + Neigh.SDW : T.None + s(Neigh.ID.sub, by= Neighmat.None.sub, bs= "cc", k= 6) + s(Neigh.ID.sub, by= Neighmat.AMF.sub, bs= "cc", k= 6) + s(Neigh.ID.sub, by= Neighmat.RP.sub, bs= "cc", k= 6) + s(Xcoord, Ycoord), random= ~ (1 | Subgroup) + (1 | AP.Variety), family= gaussian, data=dat.sub))
summary(mod106$gam)

# no difference -> neighbour effect not different along the edges
# only a suggestion of directionality in the control treatment if anything, and the effect size is minuscule compared to main neighbour effects, so discard.

# simplified model:
system.time(mod107<- gamm4(log(SDW) ~ fRun * Treatment.ID + Edge + Ext + Neigh.SDW : T.RP + Neigh.SDW : T.AMF + Neigh.SDW : T.None + s(Xcoord, Ycoord), random= ~ (1 | Subgroup) + (1 | AP.Variety), family= gaussian, data=dat.sub))
summary(mod107$gam)
dev.new()
par(mfrow= c(1, 1))
plot(mod107$gam, scheme= c(2), n2= 100, main= "")
abline(h= c(17.5, 17.5*2))

dat.sub$Treatment.ID2<- factor(dat.sub$Treatment.ID, levels= levels(dat.sub$Treatment.ID)[c(3, 2, 1)])
system.time(mod107.for.testing1<- gamm4(log(SDW) ~ fRun * Treatment.ID2 + Edge + Ext + Neigh.SDW * Treatment.ID2 + s(Xcoord, Ycoord), random= ~ (1 | Subgroup) + (1 | AP.Variety), family= gaussian, data=dat.sub))
summary(mod107.for.testing1$gam)

# analysis of deviance
anova(mod107$gam)

require(lattice)
xyplot(SDW ~ Neigh.SDW | Treatment.ID, data= dat)
xyplot(log(SDW) ~ Neigh.SDW | Treatment.ID, data= dat)

# Figure 1
# recode and reorder Treatment
dat$newTreat<- "Control"
dat$newTreat[dat$Treatment.ID == "AMF+RP"]<- "AMF+RP"
dat$newTreat[dat$Treatment.ID == "RP"]<- "RP"
dat$newTreat<- factor(dat$newTreat, levels= c("Control", "RP", "AMF+RP"))
dat$Replicate<- factor(dat$Run)
# predicted values for effect of neighbour SDW
Y.m<- mean(dat.sub$Ycoord)
X.m<- mean(dat.sub$Xcoord)

p<- ggplot(dat, aes(y= lSDW, x= Neigh.SDW, colour= Replicate)) +
	geom_point(alpha= 0.5, size= 0.5) +
	facet_grid(. ~ newTreat) + 
	theme_light() +
	# scale_color_brewer(palette="Dark2")
	scale_color_brewer(palette="Set1") +
	xlab("Shoot dry weight of neighbours") +
	ylab("log(Shoot dry weight)")
newt<- factor(c("Control", "RP", "AMF+RP"), levels= c("Control", "RP", "AMF+RP"))
oldt<- c("None", "RP", "AMF+RP")
for(i in 1:3){
	for(j in unique(dat$Replicate)){
		d<- data.frame(Neigh.SDW= seq(from= min(dat$Neigh.SDW[dat$Replicate == j & dat$newTreat == newt[i]], na.rm= T),
				to= max(dat$Neigh.SDW[dat$Replicate == j & dat$newTreat == newt[i]], na.rm= T), length= 20))
		d$lSDW<- predict(mod107$gam, newdata= data.frame(fRun= j, Treatment.ID= oldt[i], Edge= 0, Ext= 0, 
			Neigh.SDW= d$Neigh.SDW, Xcoord= X.m, Ycoord= Y.m, Subgroup= "ADMIX", AP.Variety= "1021",
			T.RP= ifelse(newt[i]== "RP", 1, 0), 
			T.AMF= ifelse(newt[i]== "AMF+RP", 1, 0),
			T.None= ifelse(newt[i]== "Control", 1, 0)), se.fit= F)
		d$newTreat<- newt[i]
		d$Replicate<- j
		p<- p + geom_line(data= d, aes(y= lSDW, x= Neigh.SDW, colour= Replicate), size= 1)
	}
}
p
ggsave("Figure1.pdf", width= 22, height= 8, units= "cm")
ggsave("Figure1.png", width= 22, height= 8, units= "cm", dpi= 600)

# plot effect sizes
str(mod107$gam$coefficients)
mod107.intercept<- as.numeric(mod107$gam$coefficients[1])
mod107.coefs<- c(T.Control= 0,
					T.RP= as.numeric(mod107$gam$coefficients[6]),
					T.AMF.RP= as.numeric(mod107$gam$coefficients[5]),
					Run1= 0,
					Run2= as.numeric(mod107$gam$coefficients[2]),
					Run3= as.numeric(mod107$gam$coefficients[3]),
					Run4= as.numeric(mod107$gam$coefficients[4]),
					RP.Run2= as.numeric(mod107$gam$coefficients[6]+mod107$gam$coefficients[2]+mod107$gam$coefficients[12]),
					RP.Run3= as.numeric(mod107$gam$coefficients[6]+mod107$gam$coefficients[3]+mod107$gam$coefficients[13]),
					RP.Run4= as.numeric(mod107$gam$coefficients[6]+mod107$gam$coefficients[4]+mod107$gam$coefficients[14]),
					AMF.RP.Run2= as.numeric(mod107$gam$coefficients[5]+mod107$gam$coefficients[2]+mod107$gam$coefficients[9]),
					AMF.RP.Run3= as.numeric(mod107$gam$coefficients[5]+mod107$gam$coefficients[3]+mod107$gam$coefficients[10]),
					AMF.RP.Run4= as.numeric(mod107$gam$coefficients[5]+mod107$gam$coefficients[4]+mod107$gam$coefficients[11]),
					Edge= as.numeric(mod107$gam$coefficients[7]),
					Exterior.edge= as.numeric(mod107$gam$coefficients[7]+mod107$gam$coefficients[8]),
					Neighbour.Control= as.numeric(mod107$gam$coefficients[17]),
					Neighbour.RP= as.numeric(mod107$gam$coefficients[15]),
					Neighbour.AMF.RP= as.numeric(mod107$gam$coefficients[16]))

par(mfrow= c(1, 1), mar= c(4.1, 12.1, 3.1, 1.1))
barplot(rev(mod107.coefs), horiz= T, col= grey(0.7), las= 1)

mod107.ranef.Subgroup<- ranef(mod107$mer)$Subgroup$'(Intercept)'
mod107.ranef.Variety<- ranef(mod107$mer)$AP.Variety$'(Intercept)'


# random effects are ignored by predict.gam(), 
# so it doesn't matter which level is used in the predictions
SDW.m<- mean(dat.sub$Neigh.SDW)
Y.m<- mean(dat.sub$Ycoord)
X.m<- mean(dat.sub$Xcoord)
mod107.baseline1234<- mean(predict(mod107$gam, newdata= data.frame(fRun= 1:4, Treatment.ID= "None", Edge= 0, Ext= 0, Neigh.SDW= SDW.m, T.RP= 0, T.AMF= 0, T.None= 1, Xcoord= X.m, Ycoord= Y.m, Subgroup= "ADMIX", AP.Variety= "1021")))
mod107.baseline1<- mean(predict(mod107$gam, newdata= data.frame(fRun= 1, Treatment.ID= "None", Edge= 0, Ext= 0, Neigh.SDW= SDW.m, T.RP= 0, T.AMF= 0, T.None= 1, Xcoord= X.m, Ycoord= Y.m, Subgroup= "ADMIX", AP.Variety= "1021")))
mod107.Run1234.None<- predict(mod107$gam, newdata= data.frame(fRun= 1:4, Treatment.ID= "None", Edge= 0, Ext= 0, Neigh.SDW= SDW.m, T.RP= 0, T.AMF= 0, T.None= 1, Xcoord= X.m, Ycoord= Y.m, Subgroup= "ADMIX", AP.Variety= "1021"), se.fit= F)
mod107.Run1234.RP<- predict(mod107$gam, newdata= data.frame(fRun= 1:4, Treatment.ID= "RP", Edge= 0, Ext= 0, Neigh.SDW= SDW.m, T.RP= 1, T.AMF= 0, T.None= 0, Xcoord= X.m, Ycoord= Y.m, Subgroup= "ADMIX", AP.Variety= "1021"), se.fit= F)
mod107.Run1234.AMF<- predict(mod107$gam, newdata= data.frame(fRun= 1:4, Treatment.ID= "AMF+RP", Edge= 0, Ext= 0, Neigh.SDW= SDW.m, T.RP= 0, T.AMF= 1, T.None= 0, Xcoord= X.m, Ycoord= Y.m, Subgroup= "ADMIX", AP.Variety= "1021"), se.fit= F)
mod107.Edge<- mean(predict(mod107$gam, newdata= data.frame(fRun= 1, Treatment.ID= "None", Edge= 1, Ext= 0, Neigh.SDW= SDW.m, T.RP= 0, T.AMF= 0, T.None= 1, Xcoord= X.m, Ycoord= Y.m, Subgroup= "ADMIX", AP.Variety= "1021"), se.fit= F))
mod107.Ext<- mean(predict(mod107$gam, newdata= data.frame(fRun= 1, Treatment.ID= "None", Edge= 1, Ext= 1, Neigh.SDW= SDW.m, T.RP= 0, T.AMF= 0, T.None= 1, Xcoord= X.m, Ycoord= Y.m, Subgroup= "ADMIX", AP.Variety= "1021"), se.fit= F))
#mod107.Neigh.None<- predict(mod107$gam, newdata= data.frame(fRun= 1, Treatment.ID= "None", Edge= 0, Ext= 0, Neigh.SDW= dat.sub$Neigh.SDW[dat.sub$T.None==1], T.RP= 0, T.AMF= 0, T.None= 1, Xcoord= X.m, Ycoord= Y.m, Subgroup= "ADMIX", AP.Variety= "1021"), se.fit= F)
#mod107.Neigh.RP<- predict(mod107$gam, newdata= data.frame(fRun= 1, Treatment.ID= "None", Edge= 0, Ext= 0, Neigh.SDW= dat.sub$Neigh.SDW[dat.sub$T.RP==1], T.RP= 1, T.AMF= 0, T.None= 0, Xcoord= X.m, Ycoord= Y.m, Subgroup= "ADMIX", AP.Variety= "1021"), se.fit= F)
#mod107.Neigh.AMF<- predict(mod107$gam, newdata= data.frame(fRun= 1, Treatment.ID= "None", Edge= 0, Ext= 0, Neigh.SDW= dat.sub$Neigh.SDW[dat.sub$T.AMF==1], T.RP= 0, T.AMF= 1, T.None= 0, Xcoord= X.m, Ycoord= Y.m, Subgroup= "ADMIX", AP.Variety= "1021"), se.fit= F)
mod107.Neigh.None<- predict(mod107$gam, newdata= data.frame(fRun= 1, Treatment.ID= "None", Edge= 0, Ext= 0, Neigh.SDW= dat.sub$Neigh.SDW, T.RP= 0, T.AMF= 0, T.None= 1, Xcoord= X.m, Ycoord= Y.m, Subgroup= "ADMIX", AP.Variety= "1021"), se.fit= F)
mod107.Neigh.RP<- predict(mod107$gam, newdata= data.frame(fRun= 1, Treatment.ID= "None", Edge= 0, Ext= 0, Neigh.SDW= dat.sub$Neigh.SDW, T.RP= 1, T.AMF= 0, T.None= 0, Xcoord= X.m, Ycoord= Y.m, Subgroup= "ADMIX", AP.Variety= "1021"), se.fit= F)
mod107.Neigh.AMF<- predict(mod107$gam, newdata= data.frame(fRun= 1, Treatment.ID= "None", Edge= 0, Ext= 0, Neigh.SDW= dat.sub$Neigh.SDW, T.RP= 0, T.AMF= 1, T.None= 0, Xcoord= X.m, Ycoord= Y.m, Subgroup= "ADMIX", AP.Variety= "1021"), se.fit= F)
mod107.Neigh.inclT<- predict(mod107$gam, newdata= data.frame(fRun= 1, Treatment.ID= dat.sub$Treatment.ID, Edge= 0, Ext= 0, Neigh.SDW= dat.sub$Neigh.SDW, T.RP= dat.sub$T.RP, T.AMF= dat.sub$T.AMF, T.None= dat.sub$T.None, Xcoord= X.m, Ycoord= Y.m, Subgroup= "ADMIX", AP.Variety= "1021"), se.fit= F) # inclusive of treatment effect
mod107.Neigh.inclTR<- predict(mod107$gam, newdata= data.frame(fRun= dat.sub$fRun, Treatment.ID= dat.sub$Treatment.ID, Edge= 0, Ext= 0, Neigh.SDW= dat.sub$Neigh.SDW, T.RP= dat.sub$T.RP, T.AMF= dat.sub$T.AMF, T.None= dat.sub$T.None, Xcoord= X.m, Ycoord= Y.m, Subgroup= "ADMIX", AP.Variety= "1021"), se.fit= F) # inclusive of treatment and run effects
mod107.spatial<- predict(mod107$gam, newdata= data.frame(fRun= 1, Treatment.ID= "None", Edge= 0, Ext= 0, Neigh.SDW= SDW.m, T.RP= 0, T.AMF= 0, T.None= 1, Xcoord= dat.sub$Xcoord[dat.sub$Treatment.ID=="None"], Ycoord= dat.sub$Ycoord[dat.sub$Treatment.ID=="None"], Subgroup= "ADMIX", AP.Variety= "1021"))

#dev.new()
# CairoPNG("SDW_EffectSize.png", width= 10, height= 10, pointsize= 3, units= "in", dpi= 600)
pdf("SDW_EffectSize.pdf", width= 10, height= 10, pointsize= 12)
par(mfrow= c(1, 1), mar= c(4.1, 12.1, 3.1, 1.1), lwd= 5)
plot(1:11, 1:11, xlim= c(-1, 1), col= NULL, yaxt= "n", xlab= "", ylab= "", bty= "n")
axis(1, lwd= 5)
abline(v= 0)
abline(h= 1:11, col= grey(0.8))
gap<- 0#1/9
points(x= mean(mod107.Run1234.None) - mod107.baseline1, y= 1, pch= 18, cex= 3, col= 2)
points(x= mod107.Run1234.None - mod107.baseline1, y= seq(from= 1-1.5*gap, to= 1+1.5*gap, l= 4), pch= 15:18, cex= 1.5, col= grey(c(0.7, 0.55, 0.45, 0.3)-0.3))
points(x= mean(mod107.Run1234.RP) - mod107.baseline1, y= 2, pch= 18, cex= 3, col= 2)
points(x= mod107.Run1234.RP - mod107.baseline1, y= seq(from= 2-1.5*gap, to= 2+1.5*gap, l= 4), pch= 15:18, cex= 1.5, col= grey(c(0.7, 0.55, 0.45, 0.3)-0.3))
points(x= mean(mod107.Run1234.AMF) - mod107.baseline1, y= 3, pch= 18, cex= 3, col= 2)
points(x= mod107.Run1234.AMF - mod107.baseline1, y= seq(from= 3-1.5*gap, to= 3+1.5*gap, l= 4), pch= 15:18, cex= 1.5, col= grey(c(0.7, 0.55, 0.45, 0.3)-0.3))
points(x= mod107.Edge - mod107.baseline1, y= 4, pch= 18, cex= 3, col= 2)
points(x= mod107.Edge - mod107.baseline1, y= 4, pch= 16, cex= 1.5)
points(x= mod107.Ext - mod107.baseline1, y= 5, pch= 18, cex= 3, col= 2)
points(x= mod107.Ext - mod107.baseline1, y= 5, pch= 16, cex= 1.5)
points(x= mean(mod107.Neigh.None) - mod107.baseline1, y= 6, pch= 18, cex= 3, col= 2)
points(x= mod107.Neigh.None - mod107.baseline1, y= rep(6, length(mod107.Neigh.None)), pch= 3, lwd= 4, cex= 1)
points(x= mean(mod107.Neigh.RP) - mod107.baseline1, y= 7, pch= 18, cex= 3, col= 2)
points(x= mod107.Neigh.RP - mod107.baseline1, y= rep(7, length(mod107.Neigh.RP)), pch= 3, lwd= 4, cex= 1)
points(x= mean(mod107.Neigh.AMF) - mod107.baseline1, y= 8, pch= 18, cex= 3, col= 2)
points(x= mod107.Neigh.AMF - mod107.baseline1, y= rep(8, length(mod107.Neigh.AMF)), pch= 3, lwd= 4, cex= 1)
points(x= mean(mod107.spatial) - mod107.baseline1, y= 9, pch= 18, cex= 3, col= 2)
points(x= mod107.spatial - mod107.baseline1, y= rep(9, length(mod107.spatial)), pch= 3, lwd= 4, cex= 1)
points(x= mod107.ranef.Subgroup, y= rep(10, length(mod107.ranef.Subgroup)), pch= 3, lwd= 4, cex= 1)
points(x= mod107.ranef.Variety, y= rep(11, length(mod107.ranef.Variety)), pch= 3, lwd= 4, cex= 1)
axis(2, at= 1:11, labels= c("T Control, Runs 1,2,3,4",
							"T RP,      Runs 1,2,3,4",
							"T AMF+RP,  Runs 1,2,3,4",
							"Edge",
							"Exterior edge",
							"Neighbour SDW | Control",
							"Neighbour SDW | RP",
							"Neighbour SDW | AMF+RP",
							"spatial variation",
							"Subgroup effect",
							"Variety effect"), las= 1, tick= F)
dev.off()


#dev.new()
# CairoPNG("SDW_EffectSize.png", width= 10, height= 10, pointsize= 3, units= "in", dpi= 600)
# pdf("SDW_EffectSize2.pdf", width= 10, height= 10, pointsize= 12) #  this plot seems wrong - ignore or double check!
# par(mfrow= c(1, 1), mar= c(4.1, 12.1, 3.1, 1.1), lwd= 5)
# plot(1:8, 1:8, xlim= c(-1, 1), col= NULL, yaxt= "n", xlab= "", ylab= "", bty= "n")
# axis(1, lwd= 5)
# abline(v= 0)
# abline(h= 1:8, col= grey(0.8))
# gap<- 0#1/9
# # points(x= mean(mod107.Run1234.None) - mod107.baseline1, y= 1, pch= 18, cex= 3, col= 2)
# # points(x= mod107.Run1234.None - mod107.baseline1, y= seq(from= 1-1.5*gap, to= 1+1.5*gap, l= 4), pch= 15:18, cex= 1.5, col= grey(c(0.7, 0.55, 0.45, 0.3)-0.3))
# # points(x= mean(mod107.Run1234.RP) - mod107.baseline1, y= 2, pch= 18, cex= 3, col= 2)
# # points(x= mod107.Run1234.RP - mod107.baseline1, y= seq(from= 2-1.5*gap, to= 2+1.5*gap, l= 4), pch= 15:18, cex= 1.5, col= grey(c(0.7, 0.55, 0.45, 0.3)-0.3))
# # points(x= mean(mod107.Run1234.AMF) - mod107.baseline1, y= 3, pch= 18, cex= 3, col= 2)
# # points(x= mod107.Run1234.AMF - mod107.baseline1, y= seq(from= 3-1.5*gap, to= 3+1.5*gap, l= 4), pch= 15:18, cex= 1.5, col= grey(c(0.7, 0.55, 0.45, 0.3)-0.3))
# points(x= mod107.Edge - mod107.baseline1, y= 4-3, pch= 18, cex= 3, col= 2)
# points(x= mod107.Edge - mod107.baseline1, y= 4-3, pch= 16, cex= 1.5)
# points(x= mod107.Ext - mod107.baseline1, y= 5-3, pch= 18, cex= 3, col= 2)
# points(x= mod107.Ext - mod107.baseline1, y= 5-3, pch= 16, cex= 1.5)
# points(x= mean(mod107.Neigh.inclT[dat.sub$Treatment.ID=="None"]) - mod107.baseline1, y= 6-3, pch= 18, cex= 3, col= 2)
# points(x= mod107.Neigh.inclT[dat.sub$Treatment.ID=="None"] - mod107.baseline1, y= rep(6-3, length(mod107.Neigh.inclT[dat.sub$Treatment.ID=="None"])), pch= 3, lwd= 4, cex= 1)
# points(x= mean(mod107.Neigh.inclT[dat.sub$Treatment.ID=="RP"]) - mod107.baseline1, y= 7-3, pch= 18, cex= 3, col= 2)
# points(x= mod107.Neigh.inclT[dat.sub$Treatment.ID=="RP"] - mod107.baseline1, y= rep(7-3, length(mod107.Neigh.inclT[dat.sub$Treatment.ID=="RP"])), pch= 3, lwd= 4, cex= 1)
# points(x= mean(mod107.Neigh.inclT[dat.sub$Treatment.ID=="AMF+RP"]) - mod107.baseline1, y= 8-3, pch= 18, cex= 3, col= 2)
# points(x= mod107.Neigh.inclT[dat.sub$Treatment.ID=="AMF+RP"] - mod107.baseline1, y= rep(8-3, length(mod107.Neigh.inclT[dat.sub$Treatment.ID=="AMF+RP"])), pch= 3, lwd= 4, cex= 1)
# points(x= mean(mod107.spatial) - mod107.baseline1, y= 9-3, pch= 18, cex= 3, col= 2)
# points(x= mod107.spatial - mod107.baseline1, y= rep(9-3, length(mod107.spatial)), pch= 3, lwd= 4, cex= 1)
# points(x= mod107.ranef.Subgroup, y= rep(10-3, length(mod107.ranef.Subgroup)), pch= 3, lwd= 4, cex= 1)
# points(x= mod107.ranef.Variety, y= rep(11-3, length(mod107.ranef.Variety)), pch= 3, lwd= 4, cex= 1)
# axis(2, at= 1:8, labels= c(#"T Control, Runs 1,2,3,4",
# 							#"T RP,      Runs 1,2,3,4",
# 							#"T AMF+RP,  Runs 1,2,3,4",
# 							"Edge",
# 							"Exterior edge",
# 							"T Control +\nNeighbour SDW | Control",
# 							"T RP +\nNeighbour SDW | RP",
# 							"T AMF+RP +\nNeighbour SDW | AMF+RP",
# 							"spatial variation",
# 							"Subgroup effect",
# 							"Variety effect"), las= 1, tick= F)
# dev.off()


CairoPNG("SDW_SpatialTrend_Run.png", width= 4.4*4, height= 10, pointsize= 160, units= "in", dpi= 600)
par(mfrow= c(1, 4), lwd= 10)
vis.gam(mod103.Run$gam, view=c("Xcoord","Ycoord"), cond= list(Run= 1), plot.type= "contour", drawlabels= F, n.grid= 100, main= "Run 1 (Box 1)")
abline(h= c(17.5, 17.5*2))
vis.gam(mod103.Run$gam, view=c("Xcoord","Ycoord"), cond= list(Run= 2), plot.type= "contour", drawlabels= F, n.grid= 100, main= "Run 2 (Box 2)")
abline(h= c(17.5, 17.5*2))
vis.gam(mod103.Run$gam, view=c("Xcoord","Ycoord"), cond= list(Run= 3), plot.type= "contour", drawlabels= F, n.grid= 100, main= "Run 3 (Box 1)")
abline(h= c(17.5, 17.5*2))
vis.gam(mod103.Run$gam, view=c("Xcoord","Ycoord"), cond= list(Run= 4), plot.type= "contour", drawlabels= F, n.grid= 100, main= "Run 4 (Box 2)")
abline(h= c(17.5, 17.5*2))
dev.off()

CairoPNG("SDW_SpatialTrend_Box.png", width= 4.4*2, height= 10, pointsize= 160, units= "in", dpi= 600)
par(mfrow= c(1, 2), lwd= 10)
vis.gam(mod103$gam, view=c("Xcoord","Ycoord"), cond= list(Box= "Box1"), plot.type= "contour", drawlabels= F, n.grid= 100, main= "Box 1")
abline(h= c(17.5, 17.5*2))
vis.gam(mod103$gam, view=c("Xcoord","Ycoord"), cond= list(Box= "Box2"), plot.type= "contour", drawlabels= F, n.grid= 100, main= "Box 2")
abline(h= c(17.5, 17.5*2))
dev.off()


CairoPNG("SDW_SpatialTrend_Average.png", width= 4.7*500, height= 10*500, pointsize= 100)
par(mfrow= c(1, 1), lwd= 4)
vis.gam(mod107$gam, view=c("Xcoord","Ycoord"), plot.type= "contour", drawlabels= T, n.grid= 100, main= "Spatial trend", contour.col= 1)
abline(h= c(17.5, 17.5*2), col= "blue")
dev.off()

CairoPNG("SDW_SpatialTrend_Block.png", width= 4.4*4, height= 14, pointsize= 160, units= "in", dpi= 600)
par(mfrow= c(3, 4), lwd= 10)
vis.gam(mod103.Block$gam, view=c("Xcoord","Y"), cond= list(Block= "None_1"  ), plot.type= "contour", drawlabels= F, n.grid= 40, main= "Control_1"  )
vis.gam(mod103.Block$gam, view=c("Xcoord","Y"), cond= list(Block= "None_2"  ), plot.type= "contour", drawlabels= F, n.grid= 40, main= "Control_2"  )
vis.gam(mod103.Block$gam, view=c("Xcoord","Y"), cond= list(Block= "None_3"  ), plot.type= "contour", drawlabels= F, n.grid= 40, main= "Control_3"  )
vis.gam(mod103.Block$gam, view=c("Xcoord","Y"), cond= list(Block= "None_4"  ), plot.type= "contour", drawlabels= F, n.grid= 40, main= "Control_4"  )
vis.gam(mod103.Block$gam, view=c("Xcoord","Y"), cond= list(Block= "RP_1"    ), plot.type= "contour", drawlabels= F, n.grid= 40, main= "RP_1"    )
vis.gam(mod103.Block$gam, view=c("Xcoord","Y"), cond= list(Block= "RP_2"    ), plot.type= "contour", drawlabels= F, n.grid= 40, main= "RP_2"    )
vis.gam(mod103.Block$gam, view=c("Xcoord","Y"), cond= list(Block= "RP_3"    ), plot.type= "contour", drawlabels= F, n.grid= 40, main= "RP_3"    )
vis.gam(mod103.Block$gam, view=c("Xcoord","Y"), cond= list(Block= "RP_4"    ), plot.type= "contour", drawlabels= F, n.grid= 40, main= "RP_4"    )
vis.gam(mod103.Block$gam, view=c("Xcoord","Y"), cond= list(Block= "AMF+RP_1"), plot.type= "contour", drawlabels= F, n.grid= 40, main= "AMF+RP_1")
vis.gam(mod103.Block$gam, view=c("Xcoord","Y"), cond= list(Block= "AMF+RP_2"), plot.type= "contour", drawlabels= F, n.grid= 40, main= "AMF+RP_2")
vis.gam(mod103.Block$gam, view=c("Xcoord","Y"), cond= list(Block= "AMF+RP_3"), plot.type= "contour", drawlabels= F, n.grid= 40, main= "AMF+RP_3")
vis.gam(mod103.Block$gam, view=c("Xcoord","Y"), cond= list(Block= "AMF+RP_4"), plot.type= "contour", drawlabels= F, n.grid= 40, main= "AMF+RP_4")
dev.off()


gam.check(mod107$gam)

mod107.res<- residuals(mod107$mer)
hist(mod107.res, nclass= 50)




# Map spatial residuals
pdf("SpatialResidualsMap.pdf", 16, 10)
par(mfrow= c(1, 4))
inc<- 0.1
subs<- dat.sub$Run == 1
symbols(dat.sub$X[subs], dat.sub$Ycoord[subs], circles= abs(mod107.res[subs]), fg= c(2, 4)[(mod107.res[subs]>0) + 1], bg= c(2, 4)[(mod107.res[subs]>0) + 1], inches= inc, main= "Run 1")
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Ext[subs]==1, 3, 0))
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Int[subs]==1, grey(0.3), 0))

subs<- dat.sub$Run == 2
symbols(dat.sub$X[subs], dat.sub$Ycoord[subs], circles= abs(mod107.res[subs]), fg= c(2, 4)[(mod107.res[subs]>0) + 1], bg= c(2, 4)[(mod107.res[subs]>0) + 1], inches= inc, main= "Run 2")
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Ext[subs]==1, 3, 0))
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Int[subs]==1, grey(0.3), 0))

subs<- dat.sub$Run == 3
symbols(dat.sub$X[subs], dat.sub$Ycoord[subs], circles= abs(mod107.res[subs]), fg= c(2, 4)[(mod107.res[subs]>0) + 1], bg= c(2, 4)[(mod107.res[subs]>0) + 1], inches= inc, main= "Run 3")
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Ext[subs]==1, 3, 0))
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Int[subs]==1, grey(0.3), 0))

subs<- dat.sub$Run == 4
symbols(dat.sub$X[subs], dat.sub$Ycoord[subs], circles= abs(mod107.res[subs]), fg= c(2, 4)[(mod107.res[subs]>0) + 1], bg= c(2, 4)[(mod107.res[subs]>0) + 1], inches= inc, main= "Run 4")
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Ext[subs]==1, 3, 0))
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Int[subs]==1, grey(0.3), 0))
dev.off()




# predict values from model
	# raw from fixed effects with spatial trend untouched
	# raw from fixed effects with spatial trend removed
	# raw from fixed effects + random effects added back in


# store random effects
dat.sub$link.ranef.Subgroup<- ranef(mod107$mer)$Subgroup[dat.sub$Subgroup, ]
dat.sub$link.ranef.Variety<- ranef(mod107$mer)$AP.Variety[dat.sub$AP.Variety, ]
dat.sub$link.ranef.Total<- dat.sub$link.ranef.Variety + dat.sub$link.ranef.Subgroup
# store fitted values
dat.sub$link.fixed.fitted<- predict(mod107$gam, newdata= data.frame(fRun= dat.sub$fRun, Treatment.ID= dat.sub$Treatment.ID, Edge= dat.sub$Edge, Ext= dat.sub$Ext, Neigh.SDW= dat.sub$Neigh.SDW, T.RP= dat.sub$T.RP, T.AMF= dat.sub$T.AMF, T.None= dat.sub$T.None, Xcoord= dat.sub$Xcoord, Ycoord= dat.sub$Ycoord, Subgroup= "ADMIX", AP.Variety= "1021"))
# fitted values without spatial variation (predict for middle of box, average neighbours, not on edge) # 12 unique values
dat.sub$link.fixed.fitted.nospace<- predict(mod107$gam, newdata= data.frame(fRun= dat.sub$fRun, Treatment.ID= dat.sub$Treatment.ID, Edge= 0, Ext= 0, Neigh.SDW= mean(dat.sub$Neigh.SDW), T.RP= dat.sub$T.RP, T.AMF= dat.sub$T.AMF, T.None= dat.sub$T.None, Xcoord= mean(dat.sub$Xcoord), Ycoord= mean(dat.sub$Ycoord), Subgroup= "ADMIX", AP.Variety= "1021"))
# fitted values without Run OR spatial variation (predict for Run 1, middle of box, average neighbours, not on edge) # 3 unique values
dat.sub$link.fixed.fitted.norun.nospace<- predict(mod107$gam, newdata= data.frame(fRun= "1", Treatment.ID= dat.sub$Treatment.ID, Edge= 0, Ext= 0, Neigh.SDW= mean(dat.sub$Neigh.SDW), T.RP= dat.sub$T.RP, T.AMF= dat.sub$T.AMF, T.None= dat.sub$T.None, Xcoord= mean(dat.sub$Xcoord), Ycoord= mean(dat.sub$Ycoord), Subgroup= "ADMIX", AP.Variety= "1021"))
# general mean
dat.sub$link.general.mean<- mean(log(dat.sub$SDW))
# store working residuals (those do not include the random effects - see plots under)
dat.sub$link.res<- residuals(mod107$gam, type= "working")

write.table(dat.sub, "RiceGWAS_ExportThomas2Alex.txt", quote= F, row.names= F, sep= "\t")


par(mfrow= c(1, 2))
plot(log(dat.sub$SDW), dat.sub$link.fixed.fitted + dat.sub$link.res)
plot(log(dat.sub$SDW), dat.sub$link.fixed.fitted + dat.sub$link.res + dat.sub$link.ranef.Total)



save.image("C:\\Users\\nhy577\\Documents\\_toma\\CONSULTING\\AdamPriceBigExp\\Workspace19_05_2014.RData")


# Map residuals for visual comparison
pdf("SpatialResidualsMap2.pdf", 16, 10)
par(mfrow= c(1, 4))
inc<- 0.1
subs<- dat.sub$Run == 1
randev<- rnorm(nrow(dat.sub))
symbols(dat.sub$X[subs], dat.sub$Ycoord[subs], circles= exp(abs(randev[subs])), fg= c(2, 4)[(randev[subs]>0) + 1], bg= c(2, 4)[(randev[subs]>0) + 1], inches= inc, main= "Run 1")
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Ext[subs]==1, 3, 0))
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Int[subs]==1, grey(0.3), 0))

subs<- dat.sub$Run == 2
symbols(dat.sub$X[subs], dat.sub$Ycoord[subs], circles= exp(abs(randev[subs])), fg= c(2, 4)[(randev[subs]>0) + 1], bg= c(2, 4)[(randev[subs]>0) + 1], inches= inc, main= "Run 2")
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Ext[subs]==1, 3, 0))
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Int[subs]==1, grey(0.3), 0))

subs<- dat.sub$Run == 3
symbols(dat.sub$X[subs], dat.sub$Ycoord[subs], circles= exp(abs(randev[subs])), fg= c(2, 4)[(randev[subs]>0) + 1], bg= c(2, 4)[(randev[subs]>0) + 1], inches= inc, main= "Run 3")
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Ext[subs]==1, 3, 0))
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Int[subs]==1, grey(0.3), 0))

subs<- dat.sub$Run == 4
symbols(dat.sub$X[subs], dat.sub$Ycoord[subs], circles= exp(abs(randev[subs])), fg= c(2, 4)[(randev[subs]>0) + 1], bg= c(2, 4)[(randev[subs]>0) + 1], inches= inc, main= "Run 4")
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Ext[subs]==1, 3, 0))
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Int[subs]==1, grey(0.3), 0))
dev.off()

pdf("SpatialResidualsMap3.pdf", 16, 10)
par(mfrow= c(1, 4))
inc<- 0.1
subs<- dat.sub$Run == 1
randev<- rnorm(nrow(dat.sub))
symbols(dat.sub$X[subs], dat.sub$Ycoord[subs], circles= exp(abs(randev[subs])), fg= c(2, 4)[(randev[subs]>0) + 1], bg= c(2, 4)[(randev[subs]>0) + 1], inches= inc, main= "Run 1")
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Ext[subs]==1, 3, 0))
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Int[subs]==1, grey(0.3), 0))

subs<- dat.sub$Run == 2
symbols(dat.sub$X[subs], dat.sub$Ycoord[subs], circles= exp(abs(randev[subs])), fg= c(2, 4)[(randev[subs]>0) + 1], bg= c(2, 4)[(randev[subs]>0) + 1], inches= inc, main= "Run 2")
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Ext[subs]==1, 3, 0))
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Int[subs]==1, grey(0.3), 0))

subs<- dat.sub$Run == 3
symbols(dat.sub$X[subs], dat.sub$Ycoord[subs], circles= exp(abs(randev[subs])), fg= c(2, 4)[(randev[subs]>0) + 1], bg= c(2, 4)[(randev[subs]>0) + 1], inches= inc, main= "Run 3")
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Ext[subs]==1, 3, 0))
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Int[subs]==1, grey(0.3), 0))

subs<- dat.sub$Run == 4
symbols(dat.sub$X[subs], dat.sub$Ycoord[subs], circles= exp(abs(randev[subs])), fg= c(2, 4)[(randev[subs]>0) + 1], bg= c(2, 4)[(randev[subs]>0) + 1], inches= inc, main= "Run 4")
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Ext[subs]==1, 3, 0))
points(dat.sub$X[subs], dat.sub$Ycoord[subs], cex= 1.5, col= ifelse(dat.sub$Int[subs]==1, grey(0.3), 0))
dev.off()













#############################################################################
####################### Analysis of Hyphae prevalence #######################
#############################################################################

library(gamm4)

AMFsubs<- dat$Treatment.ID == "AMF+RP" & !is.na(dat$Hyphae) & !is.na(dat$Arbuscule) & !is.na(dat$Vesicle)

AMF<- dat[AMFsubs,]

AMF$fRun<- factor(AMF$Run)
AMF$fBlock<- factor(AMF$Block)
AMF$Ycoord.AMF.rel<- AMF$Ycoord
AMF$Ycoord.AMF.rel[AMF$fBlock == "AMF+RP_1"]<- AMF$Ycoord.AMF.rel[AMF$fBlock == "AMF+RP_1"] - min(AMF$Ycoord.AMF.rel[AMF$fBlock == "AMF+RP_1"]) + 1
AMF$Ycoord.AMF.rel[AMF$fBlock == "AMF+RP_4"]<- AMF$Ycoord.AMF.rel[AMF$fBlock == "AMF+RP_4"] - min(AMF$Ycoord.AMF.rel[AMF$fBlock == "AMF+RP_4"]) + 1

AMF$Xcoord4<- AMF$Xcoord3<- AMF$Xcoord2<- AMF$Xcoord1<- AMF$Xcoord
AMF$Ycoord4<- AMF$Ycoord3<- AMF$Ycoord2<- AMF$Ycoord1<- AMF$Ycoord
AMF$Xcoord1[AMF$fBlock != "AMF+RP_1"]<- mean(AMF$Xcoord1[AMF$fBlock == "AMF+RP_1"])
AMF$Xcoord2[AMF$fBlock != "AMF+RP_2"]<- mean(AMF$Xcoord2[AMF$fBlock == "AMF+RP_2"])
AMF$Xcoord3[AMF$fBlock != "AMF+RP_3"]<- mean(AMF$Xcoord3[AMF$fBlock == "AMF+RP_3"])
AMF$Xcoord4[AMF$fBlock != "AMF+RP_4"]<- mean(AMF$Xcoord4[AMF$fBlock == "AMF+RP_4"])
AMF$Ycoord1[AMF$fBlock != "AMF+RP_1"]<- mean(AMF$Ycoord1[AMF$fBlock == "AMF+RP_1"])
AMF$Ycoord2[AMF$fBlock != "AMF+RP_2"]<- mean(AMF$Ycoord2[AMF$fBlock == "AMF+RP_2"])
AMF$Ycoord3[AMF$fBlock != "AMF+RP_3"]<- mean(AMF$Ycoord3[AMF$fBlock == "AMF+RP_3"])
AMF$Ycoord4[AMF$fBlock != "AMF+RP_4"]<- mean(AMF$Ycoord4[AMF$fBlock == "AMF+RP_4"])
AMF$B1<- (AMF$fBlock == "AMF+RP_1") + 0
AMF$B2<- (AMF$fBlock == "AMF+RP_2") + 0
AMF$B3<- (AMF$fBlock == "AMF+RP_3") + 0
AMF$B4<- (AMF$fBlock == "AMF+RP_4") + 0

matplot(cbind(AMF$Xcoord1, AMF$Xcoord2, AMF$Xcoord3, AMF$Xcoord4), type= "l")
matplot(cbind(AMF$Ycoord1, AMF$Ycoord2, AMF$Ycoord3, AMF$Ycoord4), type= "l")


AMF$Neigh1.Hyphae.mean<- NA # mean for existing 1st order neighbours
AMF$Neigh2.Hyphae.mean<- NA # mean for existing 2nd order neighbours
AMF$Neigh1.Hyphae.meanAll<- NA # mean for all 8 potential 1st order neighbours, existing or not
AMF$Neigh2.Hyphae.meanAll<- NA # mean for all 8 potential 2nd order neighbours, existing or not
AMF$Neigh1.SDW<- NA
AMF$Neigh2.SDW<- NA

for(i in 1:nrow(AMF)){
	bla1<- AMF$Hyphae[AMF$Run == AMF$Run[i] & AMF$Xcoord >= (AMF$Xcoord[i] - 1) & AMF$Xcoord <= (AMF$Xcoord[i] + 1) & AMF$Ycoord >= (AMF$Ycoord[i] - 1) & AMF$Ycoord <= (AMF$Ycoord[i] + 1)]
	bla1<- bla1[!is.na(bla1)]
	tmp1<- sum(bla1) - AMF$Hyphae[i]
	AMF$Neigh1.Hyphae.mean[i]<- tmp1 / (length(bla1)-1)
	AMF$Neigh1.Hyphae.meanAll[i]<- tmp1 / 8
	bla2<- AMF$Hyphae[AMF$Run == AMF$Run[i] & AMF$Xcoord >= (AMF$Xcoord[i] - 2) & AMF$Xcoord <= (AMF$Xcoord[i] + 2) & AMF$Ycoord >= (AMF$Ycoord[i] - 2) & AMF$Ycoord <= (AMF$Ycoord[i] + 2)]
	bla2<- bla2[!is.na(bla2)]
	tmp2<- sum(bla2) - AMF$Hyphae[i] - tmp1
	AMF$Neigh2.Hyphae.mean[i]<- tmp2 / (length(bla2) - length(bla1))
	AMF$Neigh2.Hyphae.meanAll[i]<- tmp2 / 16
	sel1<- AMF$Run == AMF$Run[i] & AMF$Xcoord >= (AMF$Xcoord[i] - 1) & AMF$Xcoord <= (AMF$Xcoord[i] + 1) & AMF$Ycoord >= (AMF$Ycoord[i] - 1) & AMF$Ycoord <= (AMF$Ycoord[i] + 1)
	sel2<- AMF$Run == AMF$Run[i] & AMF$Xcoord >= (AMF$Xcoord[i] - 2) & AMF$Xcoord <= (AMF$Xcoord[i] + 2) & AMF$Ycoord >= (AMF$Ycoord[i] - 2) & AMF$Ycoord <= (AMF$Ycoord[i] + 2)
	AMF$Neigh1.SDW[i]<- mean(AMF$SDW[as.logical(sel1 - ((1:nrow(AMF))==i))], na.rm= T)
	AMF$Neigh2.SDW[i]<- mean(AMF$SDW[as.logical(sel2 - sel1)], na.rm= T)
}

pairs2(AMF[, c("SDW", "Neigh1.Hyphae.mean", "Neigh1.SDW", "Neigh2.Hyphae.mean", "Neigh2.SDW", "Hyphae")])

AMF$SDW01<- AMF$SDW - AMF$Neigh1.SDW
AMF$SDW02<- AMF$SDW - AMF$Neigh2.SDW
AMF$SDW12<- AMF$Neigh1.SDW - AMF$Neigh2.SDW

par(mfrow= c(1, 4))
inc<- 0.03
symbols(AMF$X[AMF$Run == 1], AMF$Ycoord[AMF$Run == 1], circles= AMF$Neigh1.Hyphae.mean[AMF$Run == 1], fg= grey(0.4), bg= grey(0.4), inches= inc)
i<- 34
sel1<- AMF$Run == AMF$Run[i] & AMF$Xcoord >= (AMF$Xcoord[i] - 1) & AMF$Xcoord <= (AMF$Xcoord[i] + 1) & AMF$Ycoord >= (AMF$Ycoord[i] - 1) & AMF$Ycoord <= (AMF$Ycoord[i] + 1)
sel2<- AMF$Run == AMF$Run[i] & AMF$Xcoord >= (AMF$Xcoord[i] - 2) & AMF$Xcoord <= (AMF$Xcoord[i] + 2) & AMF$Ycoord >= (AMF$Ycoord[i] - 2) & AMF$Ycoord <= (AMF$Ycoord[i] + 2)
points(AMF$X[sel2], AMF$Ycoord[sel2], pch= 20, col= 3, cex= 2)
points(AMF$X[sel1], AMF$Ycoord[sel1], pch= 20, col= 2, cex= 2)
points(AMF$X[i], AMF$Ycoord[i], pch= "x", cex= 2)
i<- 72
sel3<- AMF$Run == AMF$Run[i] & AMF$Xcoord >= (AMF$Xcoord[i] - 1) & AMF$Xcoord <= (AMF$Xcoord[i] + 1) & AMF$Ycoord >= (AMF$Ycoord[i] - 1) & AMF$Ycoord <= (AMF$Ycoord[i] + 1) & !(AMF$Xcoord == AMF$Xcoord[i] & AMF$Ycoord == AMF$Ycoord[i])
points(AMF$X[sel3], AMF$Ycoord[sel3], pch= 20, col= 2, cex= 2)
points(AMF$X[i], AMF$Ycoord[i], pch= "x", cex= 2)

symbols(AMF$X[AMF$Run == 1], AMF$Ycoord[AMF$Run == 1], circles= AMF$Neigh2.Hyphae.mean[AMF$Run == 1], fg= grey(0.4), bg= grey(0.4), inches= inc)
symbols(AMF$X[AMF$Run == 1], AMF$Ycoord[AMF$Run == 1], circles= AMF$Neigh1.Hyphae.meanAll[AMF$Run == 1], fg= grey(0.4), bg= grey(0.4), inches= inc)
symbols(AMF$X[AMF$Run == 1], AMF$Ycoord[AMF$Run == 1], circles= AMF$Neigh2.Hyphae.meanAll[AMF$Run == 1], fg= grey(0.4), bg= grey(0.4), inches= inc)


hist(AMF$Hyphae)
pairs2(AMF[, c("SDW", "Hyphae", "Arbuscule", "Vesicle")])

par(mfrow= c(2, 2))
hist(AMF$Arbuscule, breaks= -0.5:55.5)
hist(AMF$Vesicle, breaks= -0.5:55.5)
hist(log(AMF$Arbuscule))
hist(log(AMF$Vesicle))

################# HYPHAE #################
############## AMF - Hyphae re-analysis (2017) ################
# not investigating 2nd order neighbour effects
# 1st order neighbour Hyphae effects only via MRF smoother
# 1st order neighbour SDW effects

par(mfrow= c(2, 2))
for(i in unique(AMF$fBlock)){
	plot(AMF$Xcoord, AMF$Ycoord, main= i); points(AMF$Xcoord[AMF$fBlock==i], AMF$Ycoord[AMF$fBlock==i], col= 2)
}

AMF$SDW.diff<- AMF$SDW - AMF$Neigh1.SDW

XYlocs<- expand.grid(x= sort(unique(AMF$Xcoord)), y= sort(unique(AMF$Ycoord.AMF.rel)))
row.names(XYlocs)<- paste("X", XYlocs$x, "Y", XYlocs$y, sep= "")
AMF$location<- factor(paste("X", AMF$Xcoord, "Y", AMF$Ycoord.AMF.rel, sep= ""), levels= row.names(XYlocs))
nb.polys<- sapply(row.names(XYlocs), function(x){
	rbind(XYlocs[x, c("x", "y")],
			XYlocs[x, c("x", "y")] + c(0, 1),
			XYlocs[x, c("x", "y")] + c(1, 1),
			XYlocs[x, c("x", "y")] + c(1, 0),
			XYlocs[x, c("x", "y")])
	}, simplify= F)

modHyphae100<- gamm(Hyphae ~ fRun + Edge + Ext + SDW * Neigh1.SDW + s(location, bs= "mrf", xt= list(polys=  nb.polys), by= fBlock, k= 333), random= list(Subgroup= ~ 1, AP.Variety= ~ 1), family= gaussian, data= AMF, drop.unused.levels= FALSE)
summary(modHyphae100$gam)
dev.new()
par(mfrow= c(2, 2))
plot(modHyphae100$gam, scheme= c(2), n2= 100, main= "")
# no indication of an effect of External borders vs. internal -> remove

# analysis of deviance
anova(modHyphae100$gam)


modHyphae101<- gamm(Hyphae ~ fRun + Edge + SDW * Neigh1.SDW + s(location, bs= "mrf", xt= list(polys=  nb.polys), by= fBlock, k= 333), random= list(Subgroup= ~ 1, AP.Variety= ~ 1), family= gaussian, data= AMF, drop.unused.levels= FALSE)
summary(modHyphae101$gam)
# no indication of an Edge effect -> remove

# modHyphae102no<- gamm(Hyphae ~ fRun + SDW + Neigh1.SDW + s(location, bs= "mrf", xt= list(polys=  nb.polys), by= fBlock, k= 333), random= list(Subgroup= ~ 1, AP.Variety= ~ 1), family= gaussian, data= AMF, drop.unused.levels= FALSE)
# summary(modHyphae102no$gam)

modHyphae102<- gamm(Hyphae ~ fRun + SDW * Neigh1.SDW + s(location, bs= "mrf", xt= list(polys=  nb.polys), by= fBlock, k= 333), random= list(Subgroup= ~ 1, AP.Variety= ~ 1), family= gaussian, data= AMF, drop.unused.levels= FALSE)
summary(modHyphae102$gam)


# approximate variance components using LM
modHy102<- lm(Hyphae ~ fRun + SDW * Neigh1.SDW, data= AMF)
anova(modHy102)
sum(anova(modHy102)$'Sum Sq'[2:4])/sum(anova(modHy102)$'Sum Sq') # 0.0592547

modHy102a<- lm(Hyphae ~ fRun + SDW + Neigh1.SDW, data= AMF)
anova(modHy102a)
sum(anova(modHy102a)$'Sum Sq'[2:3])/sum(anova(modHy102a)$'Sum Sq') # 0.0539735


mod107vc<- lm(log(SDW) ~ fRun * Treatment.ID + Edge + Ext + Neigh.SDW : T.RP + Neigh.SDW : T.AMF + Neigh.SDW : T.None, data=dat.sub)
anova(mod107vc)
sum(anova(mod107vc)$'Sum Sq'[6:8])/sum(anova(mod107vc)$'Sum Sq') # 



vis.gam(modHyphae102$gam, c("SDW", "Neigh1.SDW"))

CairoPNG("Hyphae_MRF_trends.png", width= 10000, height= 2500, res= 300, pointsize= 30)#, type = c("cairo-png"))
par(mfrow= c(1, 4))
plot(modHyphae102$gam, terms= c("location"))
dev.off()

modHyphae102a<- gamm(Hyphae ~ fRun + SDW + Neigh1.SDW + s(location, bs= "mrf", xt= list(polys=  nb.polys), by= fBlock, k= 333), random= list(Subgroup= ~ 1, AP.Variety= ~ 1), family= gaussian, data= AMF, drop.unused.levels= FALSE)
summary(modHyphae102a$gam)

modHyphae102b<- gamm(Hyphae ~ fRun + te(SDW, Neigh1.SDW) + s(location, bs= "mrf", xt= list(polys=  nb.polys), by= fBlock, k= 333), random= list(Subgroup= ~ 1, AP.Variety= ~ 1), family= gaussian, data= AMF, drop.unused.levels= FALSE)
summary(modHyphae102b$gam)

vis.gam(modHyphae102$gam, view= c("SDW", "Neigh1.SDW"), theta= 30, too.far= 0.3)

plot(modHyphae102$gam, view= c("location"), theta= 30, cond= list(fBlock = "AMF+RP_1"))


# compare spline spatial trends with MRFs
par(mfrow= c(2, 4))
modHyphae102.spl<- gamm(Hyphae ~ fRun + SDW * Neigh1.SDW + s(Xcoord, Ycoord.AMF.rel, by= fBlock), random= list(Subgroup= ~ 1, AP.Variety= ~ 1), family= gaussian, data= AMF, drop.unused.levels= FALSE)
modHyphae102.spl.gam<- gam(Hyphae ~ fRun + SDW * Neigh1.SDW + s(Xcoord, Ycoord.AMF.rel, by= fBlock) + Subgroup + s(AP.Variety, bs= "re"), family= gaussian, data= AMF, drop.unused.levels= FALSE)
plot(modHyphae102.spl$gam, scheme= c(2), n2= 100, main= "")
summary(modHyphae102.spl$gam)
summary(modHyphae102.spl.gam)
AIC(modHyphae102.spl.gam)
# [1] 11135.88

for(i in 1:4){
	modHyphae102tmp<- gamm(Hyphae ~ SDW * Neigh1.SDW + s(location, bs= "mrf", xt= list(polys=  nb.polys), k= 333), family= gaussian, data= AMF, drop.unused.levels= FALSE, subset= fRun == i)
	print(summary(modHyphae102tmp$gam)$p.coeff)
	plot(modHyphae102tmp$gam, scheme= c(2), n2= 100, main= "")
}
# MRFs capture similar broad patterns plus more local variation as expected
# seems visually there is a negative edge effect. Could that be absorbed and masked by MRF term?
# -> try edge effect with splines (more smoothing)

hyph.mrf.coefs<- summary(modHyphae102$gam)$p.coeff
hyph.spl.coefs<- summary(modHyphae102.spl$gam)$p.coeff
# quite similar except coefs for fRun2


modHyphae103.spl<- gamm(Hyphae ~ fRun + Edge + SDW * Neigh1.SDW + s(Xcoord, Ycoord.AMF.rel, by= fBlock), random= list(Subgroup= ~ 1, AP.Variety= ~ 1), family= gaussian, data= AMF, drop.unused.levels= FALSE)
par(mfrow= c(1, 4))
plot(modHyphae103.spl$gam, scheme= c(2), n2= 100, main= "")
sum.modHyphae103.spl<- summary(modHyphae103.spl$gam)
sum.modHyphae103.spl
# no statistical evidence of edge effect -> keep model 102

gam.check(modHyphae102.spl$gam)
gam.check(modHyphae102$gam); abline(0, 1)
# tighter fit, as expected, but systematic underestimation of large values and overestimation of small values (suggests over-smoothing?)

vis.gam(modHyphae102$gam, c("Neigh1.SDW", "SDW"), se= 1.96, type= "response", color= "heat", ticktype="detailed", zlim= c(0, 100), theta= -30, phi= 15, lwd= 2, cex.lab= 1.5, cex.axis= 1.2)

png("Hyphae~SDWinterSDWneigh.png", width= 2500, height= 2500, res= 300, type = c("cairo-png"))
vis.gam(modHyphae102.spl$gam, c("Neigh1.SDW", "SDW"), se= 1.96, type= "response", color= "heat", ticktype="detailed", zlim= c(0, 100), theta= -30, phi= 15, lwd= 2, cex.lab= 1.5, cex.axis= 1.2, zlab= "Hyphae")
dev.off()

modHyphae104.spl<- gamm(Hyphae ~ fRun + SDW + Neigh1.SDW + s(Xcoord, Ycoord.AMF.rel, by= fBlock), random= list(Subgroup= ~ 1, AP.Variety= ~ 1), family= gaussian, data= AMF, drop.unused.levels= FALSE)
plot(modHyphae104.spl$gam, scheme= c(2), n2= 100, main= "")
summary(modHyphae104.spl$gam)
modHyphae104.spl.gam<- gam(Hyphae ~ fRun + SDW + Neigh1.SDW + s(Xcoord, Ycoord.AMF.rel, by= fBlock) + Subgroup + s(AP.Variety, bs= "re"), family= gaussian, data= AMF, drop.unused.levels= FALSE)
summary(modHyphae104.spl.gam)
AIC(modHyphae104.spl.gam)
# [1] 11135.51

png("Hyphae~SDWplusSDWneigh.png", width= 2500, height= 2500, res= 300, type = c("cairo-png"))
vis.gam(modHyphae104.spl$gam, c("Neigh1.SDW", "SDW"), se= 1.96, type= "response", color= "heat", ticktype="detailed", zlim= c(0, 100), theta= -30, phi= 15, lwd= 2, cex.lab= 1.5, cex.axis= 1.2, zlab= "Hyphae")
dev.off()

png("Hyphae~SDW_and_SDWneigh.png", width= 5000, height= 2500, res= 300, type = c("cairo-png"))
par(mfrow= c(1, 2))
vis.gam(modHyphae102$gam, c("Neigh1.SDW", "SDW"), se= 1.96, type= "response", color= "heat", ticktype="detailed", zlim= c(0, 100), theta= -30, phi= 15, lwd= 2, cex.lab= 1.5, cex.axis= 1.2, zlab= "Proportion with hyphae", xlab= "Neighbour SDW (g)", ylab= "SDW (g)")
vis.gam(modHyphae102a$gam, c("Neigh1.SDW", "SDW"), se= 1.96, type= "response", color= "heat", ticktype="detailed", zlim= c(0, 100), theta= -30, phi= 15, lwd= 2, cex.lab= 1.5, cex.axis= 1.2, zlab= "Proportion with hyphae", xlab= "Neighbour SDW (g)", ylab= "SDW (g)")
dev.off()

##################### test if SDW depends on hyphal colonization ####################
# system.time(mod107<- gamm4(log(SDW) ~ fRun * Treatment.ID + Edge + Ext + Neigh.SDW : T.RP + Neigh.SDW : T.AMF + Neigh.SDW : T.None + s(Xcoord, Ycoord), random= ~ (1 | Subgroup) + (1 | AP.Variety), family= gaussian, data=dat.sub))
# summary(mod107$gam)

dat.sub$Hyphae2<- dat.sub$Hyphae
dat.sub$Hyphae2[is.na(dat.sub$Hyphae)]<- 0

# simplified model:
system.time(mod108<- gamm4(log(SDW) ~ fRun * Treatment.ID + Edge + Ext + Neigh.SDW : T.RP + Neigh.SDW : T.AMF + Neigh.SDW : T.None + s(Xcoord, Ycoord) + s(Hyphae2), random= ~ (1 | Subgroup) + (1 | AP.Variety), family= gaussian, data=dat.sub))
summary(mod108$gam)
dev.new()
par(mfrow= c(1, 2))
plot(mod108$gam, scheme= c(2), n2= 100, main= "", select= 2)
plot(mod108$gam, scheme= c(2), n2= 100, main= "", select= 1)
abline(h= c(17.5, 17.5*2))

# analysis of deviance
anova(mod108$gam)

system.time(mod108.lin<- gamm4(log(SDW) ~ fRun * Treatment.ID + Edge + Ext + Neigh.SDW : T.RP + Neigh.SDW : T.AMF + Neigh.SDW : T.None + s(Xcoord, Ycoord) + Hyphae2, random= ~ (1 | Subgroup) + (1 | AP.Variety), family= gaussian, data=dat.sub))
summary(mod108.lin$gam)


# growth reduced in AMF treatment but plants bigger when hyphal colonization is greater (or could be the reverse: colonization is greater on bigger plants?)
# could lead to lesser variance in AMF treatment due to hyphae preferentially colonizing faster growing plants and reducing their growth??

par(mfrow= c(1, 2))
plot(resid(mod108$gam) ~ dat.sub$Treatment.ID)
hist(resid(mod108$gam), freq= F)
for(i in 1:3){
	lines(density(resid(mod108$gam)[dat.sub$Treatment.ID == unique(dat.sub$Treatment.ID)[i]]), lwd= 2, col= i)
}
# no difference in residual variance between treatments



# check if effect of neighbour size is different when hyphal colonization is stronger
system.time(mod109<- gamm4(log(SDW) ~ fRun * Treatment.ID + Edge + Ext + Neigh.SDW : T.RP + Neigh.SDW : T.AMF + Neigh.SDW : T.None + s(Xcoord, Ycoord) + Hyphae2 + Neigh.SDW : T.AMF : Hyphae2, random= ~ (1 | Subgroup) + (1 | AP.Variety), family= gaussian, data=dat.sub))
summary(mod109$gam)
# no indication of interaction -> keep model 108



