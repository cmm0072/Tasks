getwd ()
library (phytools)
trees<-vector("list",1)
births<-c()
fractions<-c()
r<-c()
s<-c()
branch<-c()
?pbtree

for(i in 1:100) {
  births[i]<- runif(1)
  fractions[i]<- runif(1)
  trees[[i]] <- pbtree(b=births[i], d=(fractions[i]*births[i]),n=100)
  r[[i]]<- (births[i]- (fractions[i]*births[i]))
  s[[i]]<-births[i]
  branch[[i]]<-mean(trees[[i]]$edge.length)
}

str(trees)
pdf("Q1-3_TreesPBplot",height=8.96,width=5.6)
plot(trees[[i]])
dev.off()
trees[[100]]$tip.label
sapply(trees, Ntip)

#'Question 4, the code is below.'
tips<-log(sapply(trees, Ntip))
tips
r1<-unlist(r)
pdf("Q4_diversification_logofnumberoftips",height=4,width=4)
plot(tips,r1, xlab="log of number of tips" , ylab= "diversification", col="orange", pch=18)
abline(lm(tips ~ r1))
dev.off()
cor(tips,r1)
#'The cor test gave a value of 0.2060723. This means there is a positive correlation between diversification rate and the log of the number of tips.'

#'Question 5, the code is below.'
s1<-unlist(s)
branch <-unlist(branch)
pdf("Q5_speciationrate_and_avgbranchlength",height=4,width=4)
plot(branch,s1, xlab="Average branch length" , ylab= "speciaiton rate", col="orange", pch=1)
abline(lm(branch ~ s1))
dev.off()

#'Question 6, the code is below.'
cor(branch,s1)
#'The cor test gave a value of -0.5720068, which means there is quite a strong negative correlation between speciation rate and average branch length.'


#'Question 7, the code is below.'
Tree<-trees[[which.max(tips)]]

pdf("Q7_largestTreePBplot",height=8.96,width=5.6)
plot(Tree)
dev.off()

rates<-c()
traits<-vector("list",1)
meantraits<-c()
vartraits<-c()
for(i in 1:100) {
  rates[i]<- runif(1)
  traits[[i]]<-fastBM(tree = Tree, sig2 = rates[i])
  meantraits[[i]]<-mean(traits[[i]])
  vartraits[[i]]<-var(traits[[i]])
}

#'Question 8, the code is below.'
meantraits<-unlist(meantraits)

pdf("Q8_rates_and_meantraits",height=4,width=4)
plot(meantraits, rates)
dev.off()
cor(meantraits, rates)
#'The cor test returns a value of -0.08399239, which means they are very very slightly negatively correlated with one another.'


#Question 9, the code is below.'
vartraits<-unlist(vartraits)

pdf("Q9_rates_and_vartraits",height=4,width=4)
plot(vartraits, rates)
dev.off()

cor(vartraits, rates)
#'The cor test returns a value of 0.7523002, which means there is a very strong positive correlation between the two.'

#'Question 10, the code is below.'
cor(traits[[1]], traits[[2]])
#'The cor test returned a value of -0.1125329, which means there is a slight negative correlation between the two. A value -0.11 is not really significant, perhaps it does hint towards things but it is not indicating a high enough correlation to considered very significant.'

pdf("Q10_traits[[2]]_and_traits[[1]]",height=6,width=6)
plot(traits[[1]], traits[[2]])
dev.off()

traitMat<-cbind(traits[[1]], traits[[2]])

#'Extra Credit, the code is below.'

?phylomorphospace
pdf("Extra Credit plot",height=20,width=20)
phylomorphospace(Tree, traitMat, xlab= "trait 1", ylab= "trait 2")
dev.off()

