udat3 <- read.csv ("y1.csv")
udat4 <- read.csv ("y2.csv")
udat5 <- udat3[,5:7]
udat6 <- udat3[,7:8]
udat7 <- read.csv ("y3.csv")
udat8 <- udat7[,5:7]
udat10 <- read.csv ('y5.csv')
udat11 <- udat10[,7:8]
udat12 <- udat10 [,9:10]
udat2 <- read.csv("Smadja et al 2015.csv")

head (udat3)
par(mar=c(4,5,1,1), las=1, mgp=c(2.5, 0.75, 0), mfrow=c(1,2))
boxplot(udat5$A_snif_.s. ~ udat5$stimA, boxwex=0.4, xlab="", ylab="time sniffing in sec", col='white')
boxplot(udat8$A_snif_.s. ~ udat8$stimA, boxwex=0.4, xlab="", ylab="time sniffing in sec", col='white')
dev ()
aov(udat8$A_snif_.s. ~ udat8$B_snif_.s.)

t.test(udat11$A_snif_.s., udat11$B_snif_.s.)
t.test(udat12$Smadja_1, udat12$Smadja_2)
t.test (ndat$Bradford.Protein.Assay, ndat$Dominance.Status)

aov(udat8$A_snif_.s. ~ udat8$stimA)



boxplot(udat2[,9], udat2[,10], boxwex=0.2, col='white', names=c("dom", "mus"))

par(mar=c(4,5,1,1), las=1, mgp=c(2.5, 0.75, 0), mfrow=c(1,2))
boxplot(udat8$A_snif_.s. ~ udat8$stimA, boxwex=0.4, xlab="", ylab="time sniffing in sec", col='white')
boxplot(udat2[,9], udat2[,10], boxwex=0.2, col='white', names=c("dom", "mus"))

ndat <- read.csv("Nelson2015_edit.csv")
head(ndat)
t.test (ndat$Bradford.Protein.Assay, ndat$Dominance.Status)
ndat2 <- ndat[,]
boxplot(ndat$Bradford.Protein.Assay~ndat$Dominance.Status, boxwex=0.2, col='white', xlab="dominance", ylab="Bradford protein assay")
