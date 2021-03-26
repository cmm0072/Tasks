getwd ()
install.packages ("readxl")
library(readxl)
Mus1UsedData <- read_excel("Mus1UsedData.xlsx")
View(Mus1UsedData)
#'I want to change my hypothesis to mice that are pure bred (non hybrids) have more offspring than mixed bred (hybrids).'
#'For my graph, I would want number of offspring on the y axis and two groups, 'pure' and 'mixed' on the x axis. I would like the chart to have the large block with the error bars.'
#'For my simple analysis plan, I would like to prefrom a 2 sample variance test to see if variance is the same. If that pans out to be equal, then I want to do a one-tailed t-Test to see if the pure group have more offspring than the mixed group.'

install.packages('ape')
library (abe)
install.packages('phytools')
library (phytools)

text.string <-
  "(((((((cow, pig), whale), (bat, (lemur, human))), (robin, iguana)), coalacanth), (goldfish, trout)), shark);"
vert.tree <- read.tree (text=text.string)
plot (vert.tree, edge.width=2)
nodelabels (frame="circle", bg='white', cex=1)
#'Question 1, goldfish is closer related to humans than sharks are.'
vert.tree
#'Question 2, there are no branch lengths in this tree.'
str (vert.tree)
tree <- read.tree (text= "(((A,B), (C,D)), E);")
plotTree (tree,offset = 1)
tiplabels (frame="circle", bg='lightblue', cex=1)
nodelabels (frame = "circle", bg ='white', cex=1)
tree$tip.label
AnolisTree <- force.ultrametric (read.tree ("https://jonsmitchell.com/data/anolis.tre"))
par (las=1)
hist (AnolisTree$edge.length, col='black', border='white', main="", xlab="edge lengths for Anolis tree", ylim=c (0, 50), xlim = c (0,6))
tipEdges <- which (AnolisTree$edge [,2] <= Ntip (AnolisTree))
Lengths <- AnolisTree$edge.length
names (Lengths) <- AnolisTree$tip.label
names (Lengths) [which (Lengths == min (Lengths))]
plot (AnolisTree, cex=0.25)
Labs <- sapply (AnolisTree$edge.length, round, digits=2)
edgelabels (text=Labs, cex=0.25)
?plot.phylo

#'Question 3, code below'
plot (AnolisTree, edge.width=2, show.tip.label = FALSE)

#'Question 4, code below'
plot (AnolisTree, edge.width=2, show.tip.label = FALSE, type = "fan")

#'Question 5, code below'
plot (AnolisTree, edge.width=2, show.tip.label = FALSE, type = "fan", edge.color = "red")
#'Or"
#'plot (AnolisTree, edge.width=2, show.tip.label = FALSE, edge.color = 'red') I do not know if you wanted it in fan form or not.'

#'Question 6-8, code below'
TipEdge2 <- (which (AnolisTree$edge [,2] <= Ntip (AnolisTree)))
Lengths2 <- AnolisTree$edge.length
smallEdge <- which (Lengths == min (Lengths))
smallEdge

AnolisTree2 <- drop.tip (AnolisTree, 82, trim.internal = TRUE, subtree = TRUE, root.edge = 0)

plot (AnolisTree2, edge.width=2, show.tip.label = FALSE, type = "fan", edge.color = "red")

ltt (AnolisTree)
abline (0, 1, lwd = 2, col = 'red', lty = 2)

#'Question 9, The line constantly goes up. It does thing because more and more species are branching from each new branch. One species can turn into two, but two can turn into 4, etc.. The slope of the line itself is not, no. The slope tells you how fast new species are being created.'
#'Question 10, code below'
?fit.bd
??fit.bd
library (phytools)
fit.bd (AnolisTree, b+NULL, d=NULL, rho=0.2)
#'Dr. Mitchell, I had turned this in before 11am today, but I was not aware that there were questions 9 and 10. So, when I learned of them I did them and now I am turning it in again, at 7:20pm.'