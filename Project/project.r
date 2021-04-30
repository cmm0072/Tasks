getwd ()
#install.packages ("readxl")
library(readxl)
Mus1UsedData <- read_excel("Mus1UsedData.xlsx")
View(Mus1UsedData)
d = read.table (file = "Peromyscus_matechoice.txt", header = TRUE, sep = "\t")
HybridFert <- read_excel("HybridFertilityData.xlsx")
#'I want to change my hypothesis to mice that are pure bred (non hybrids) have more offspring than mixed bred (hybrids).'
#'For my graph, I would want number of offspring on the y axis and two groups, 'pure' and 'mixed' on the x axis. I would like the chart to have the large block with the error bars.'
#'For my simple analysis plan, I would like to prefrom a 2 sample variance test to see if variance is the same. If that pans out to be equal, then I want to do a one-tailed t-Test to see if the pure group have more offspring than the mixed group.'
#'


# urine sniffing thing
udat1 <- read.csv("Yasmin et al 2014.csv")
udat1 <- udat1[,1:10]
udat2 <- read.csv("Smadja et al 2015.csv")
udat3 <- read.csv ("y1.csv")
udat4 <- read.csv ("y2.csv")
getwd ()

par(mar=c(4,5,1,1), las=1, mgp=c(2.5, 0.75, 0), mfrow=c(1,2))
boxplot(udat1$A_snif_.s. ~ udat1$stimA, boxwex=0.4, xlab="", ylab="time sniffing in sec", col='white')
boxplot(udat2[,9], udat2[,10], boxwex=0.2, col='white', names=c("dom", "mus"))

aov(udat1$A_snif_.s. ~ udat1$stimA)



##############
ndat <- read.csv("Nelson2015_edit.csv")
head(ndat)

par(mar=c(4,5,1,1), las=1, mgp=c(2.5, 0.75, 0))
boxplot(ndat$Bradford.Protein.Assay~ndat$Dominance.Status, boxwex=0.2, col='white', xlab="dominance", ylab="Bradford protein assay")

###############
x <- read.csv("allmice_Mus.csv")
head(x)
#x$inbreeding.coefficient <- gsub("n.a.", NA, x$inbreeding.coefficient)
x$inbreeding.coefficient <- as.numeric(x$inbreeding.coefficient)
Year <- x$Birth
Year <- sapply(Year, function(x) strsplit(x, "/")[[1]][3])
#'So, right now I am think about looking at how an inbreeding coefficient of 0.25 does in regards to number of offspring between mates with inbreeding coefficients of 0.25, 0.15, and 0.0.'
#'I can use 2007 data as a control or the 0.0 inbreeding coefficent data pool and the I can use 2008-09 data for 0.25 and 2009 data for 0.15 inbreeding coefficent.'
#'This makes my current hypothesis, "As the inbreeding coefficent increases, the number of offspring had per litter decreases.'
#'
#'I looked into the paper a bit more, and I was still not sure what those data were. However, there was another excel document that had the genotype of the mother and father mice and how many offspring they had.'
#'Could I test when two purebred mice of the same genotype, so both GG or FF, they have more offspring than when two purebred but opp. genotypes, so a GG mating with a FF, and better than when two hybrids mate with one another, or when one purebred and one hybrid mate with each other?'
#'So basically, the testable hypothesis would be that a breeding event that creates all purebred offspring will have more offspring in the liter than a breeding event that did not produce all purebred offspring.'

#'Actually, I can do the hybrid hypo on just the F1 matings and then for all matings I can do inbreeding's effects, three groups - no inbred, mother-son/daughter-father, and fullsibs.'
#'
#'Can we make a box plot for breedings events that will produce hybyrids vs ones that do not, removing phase 10. Maybe after this draft, it would be good to go back in and remodel this a bit, incorperating phase ten and turining the origins into percent G or F and then use to to determine approx. how many of offspring would be hybrid (not 100% G or F) or pure (100% G or F) and use that to better analyze how the production of hybrid offspring affect number of offspring.'
#'Also, can we do a one tailed t-test to see if events that produce all purebreds have a larger liter size than events that create hybrids?' 
#'
offspring <- read.csv("MonteroOffspringData.csv")

head(offspring)


# First, exclude phase 10
DropPhase10 <- which(offspring$Phase_siring != 10)
offspring1 <- offspring[DropPhase10,]

Pairing <- apply(offspring1, 1, function(x) paste(x[9], x[10], sep=""))
isHybrid <- sapply(Pairing, function(x) length(unique(strsplit(x, split="")[[1]]))) - 1

par(mar=c(4,5,1,1), las=1, mgp=c(2.25, 0.25, 0), tck=-0.01)
boxplot(offspring1$Number_offspring ~ isHybrid, boxwex=0.25, col='white', xlab="", ylab="number of offspring", names=c("pure", "mixed"))
t.test(offspring1$Number_offspring~isHybrid)


# Second, percent hybrid offspring
percMixed <- function(mother, father){
  Mgams <- strsplit(mother, split="")[[1]]
  Fgams <- strsplit(father, split="")[[1]]

  if ("." %in% Mgams) {
      Mgams <- Mgams[-which(Mgams == ".")]
  }
  if ("." %in% Fgams) {
    Fgams <- Fgams[-which(Fgams == ".")]
  }
  out <- c()
  out[1] <- ifelse(length(unique(Mgams)) == 1, 0, 1)
  out[2] <- ifelse(length(unique(Fgams)) == 1, 0, 1)
  return(out)
}

crossType <- apply(offspring, 1, function(x) percMixed(x[9], x[10]))
crossTypeScore <- apply(crossType, 2, sum)

par(mar=c(4,5,1,1), las=1, mgp=c(2.25, 0.25, 0), tck=-0.01)
boxplot(offspring$Number_offspring ~ crossTypeScore, boxwex=0.25, col='white', xlab="num. hybrid parents", ylab="number of offspring", names=c("0", "1", "2"))
ANOVA <- aov(offspring$Number_offspring~crossTypeScore)
summary(ANOVA)
ANOVA2 <- lm(offspring$Number_offspring~ 1 + crossTypeScore)
summary(ANOVA2)



