
library (phytools)
getwd ()
tree <- AnolisTree <- force.ultrametric (read.tree ("https://jonsmitchell.com/data/anolis.tre"))
plot (tree, type = "fan")
tree
#'There are 82 tips, and when I print tree it tells me there are branch lengths and that it is rooted.'
data <- read.csv ("https://jonsmitchell.com/data/svl.csv", stringsAsFactors = F, row.names = 1)
data
#'This data should be a list, it has one row names with their associated value beside them.'
svl <- setNames (data$svl, rownames (data))
Ancestors <- fastAnc (tree, svl, vars = TRUE, CI = TRUE)
Ancestors
?fastAnc
#'The estimated values are stored in the object Ancestors which is a object of class fastAnc or the tips of the tree.'
#'The CI95 element is, well the CI element lets you choose if you want to do a 95 percent confidence interval on state estimates.'
#'Two asumptions made by fastAnc are that the trait is continious and that the MLE is the same as the state computed for the root node during Felenstein's (1985) contrasts algorithm.'
par (mar = c (0.1, 0.1, 0.1, 0.1))
plot (tree, type = "fan", lwd = 2, show.tip.label = F)
tiplabels (pch = 16, cex = 0.25 * svl [tree$tip.label])
nodelabels (pch = 16, cex = 0.25 * svl [tree$tip.label])
obj <- contMap (tree, svl, plot=F)
plot (obj, type = "fan", legend = 0.7 * max (nodeHeights(tree)), sig = 2, fsize = c (0.7, 0.9))
fossilData <- data.frame (svl = log (c ( 25.4, 23.2, 17.7, 19.7, 24, 31)), tip1 = c ("Anolis_aliniger", "Anolis_aliniger", "Anolis_occultus", "Anolis_ricordii", "Anolis_cristatellus", "Anolis_occultus"), tip2 = c ("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "anolis_cybotes", "Anolis_angusticeps", "Anolis_angusticeps"))
#'Code for for () loop below.'
fossilNodes <- c ()
nodeN <- c()
'{
  for (i in 1:nrow (fossilData))
    i <- 1 
  if (i == 1) {
      print (Ancestors)}
} this was the for loop I had.'

for (i in 1:nrow (fossilData)) {
  Node <- fastAnc (tree, svl, vars = TRUE, CI = TRUE)
  fossilNodes [i] <- fossilData [i, "svl"]
  nodeN[i] <- Node
}
Ancestors_withFossils <- fastAnc (tree, svl, anc.states = fossilNodes, CI=TRUE, var=TRUE)
Ancestors_withFossils

install.packages ("geiger")
library (geiger)
?fitContinuous

bmModel <- fitContinuous(tree,svl, SE = 0,
              model = "BM")
ouModel <- fitContinuous(tree,svl, SE = 0,
                         model = "OU")
ebModel <- fitContinuous(tree,svl, SE = 0,
                         model = "EB")
rate_trendModel <- fitContinuous(tree,svl, SE = 0,
                                 model = "rate_trend")
lambdaModel <- fitContinuous(tree,svl, SE = 0,
                             model = "lambda")
kappaModel <- fitContinuous(tree,svl, SE = 0,
                            model = "kappa")
deltaModel <- fitContinuous(tree,svl, SE = 0,
                            model = "delta")
mean_trendModel <- fitContinuous(tree,svl, SE = 0,
                                 model = "mean_trend")
whiteModel <- fitContinuous(tree,svl, SE = 0,
                            model = "white")
#'The EB model is the best because it works with the lowest AIC, which is -7.235124.'
#'The EB model is different from the fastAnc model because it is set by the a rate parameter and fastAnc reboots the tree at each internal node and computes the contrast state at each respective node each respective time. I believe that the fastAnc is more like the BM than the EB model.'
#'After checking the reddit, I seen your comment on the for loop, so I changed it to what you had put, and repushed it at 5:21 pm, but I had pushed the orignial one with the old for loop before 11 am.'