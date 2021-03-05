library (learnPopGen)
?coalescent.plot()
coalescent.plot(n=25, ngen =100, col.order = "alternating")
coalescent.plot(n=12, ngen =100, col.order = "alternating")
coalescent.plot(n=10, ngen =100, col.order = "alternating")
coal1 <- coalescent.plot(n=25, ngen =50, col.order = "alternating")
coal2 <- coalescent.plot(n=12, ngen =24, col.order = "alternating")
coal3 <- coalescent.plot(n=10, ngen =20, col.order = "alternating")
coalescent.plot(n=3, ngen =10, col.order = "alternating")
coalescent.plot(n=4, ngen =10, col.order = "alternating")
coalescent.plot(n=5, ngen =10, col.order = "alternating")
coal4 <- coalescent.plot(n=3, ngen =10, col.order = "alternating")
coal5 <- coalescent.plot(n=4, ngen =10, col.order = "alternating")
coal6 <- coalescent.plot(n=5, ngen =10, col.order = "alternating")
var (c (1, 2, 0))
var (c (1, 2, 0, 2, 0, 1))
#'On average, my simulations started with 4 alleles and this was based on the population size. So, increasing Ne would increase the alleles. So, for code sake, I suppose all you can do is make the pop size larger.'
#'On average, it took 5 generations on average for a gene to reach fixation for my 3 simulations'
#'On average, each haploid has 1 offspring. As for the variance, each generation would have a variance of 1, but this would change looking at it across multiple generations as once.'
#'So, for fitness. Does the simulation take that into account? I know if this was a coalesnce plot that represents something that actually happened, natural selection would push for what allowed for what gave the most offspring. Now that I am typing this, it actually makes sense. So, since the <insert color that fixated here> had more offspring, it fixed. So, in a practical sense they had allowed for a higher fitness or was linked with a gene that did. However, in these simulations the populations are so small that drift probably was so strong that natural selection did not really do too much.'
#'No, not the most common ancestor is not typically in gen 0, even though the 3rd plot shows it being there.'
install.packages("coala")
install.packages ("phytools")
library (coala)
library (phytools)
model <- coal_model (sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) +
  feat_mutation (10) +
  feat_recombination (10) +
  sumstat_trees () +
  sumstat_nucleotide_div ()
stats <- simulate (model, nsim = 1)
Diversity <- stats$pi
Diversity
#'There are differences and the instructions says this shows their genetic diversity. There are 10 numbers because there are 10 chromosomes in the population. Each chromosome could have different verisons of alleles between them and then between entire indidivuals as well - which makes the differences in the numbers.)
Nloci <- length (stats$trees)
t1 <- read.tree (text=stats$trees [[1]][1])
plot (t1)
axisPhylo()
#'The tips represent each chromosome.'
Age1 <- max (nodeHeights (t1))
t2 <- read.tree (text=stats$trees [[2]][1])
plot (t2)
axisPhylo ()
#'No, the phylogenies do not match.'
par (mfrow=c(1,2))
plot (t1)
axisPhylo ()
plot (t2)
axisPhylo ()
compare.chronograms (t1, t2)
t1_1 <- read.tree (text=stats$trees [[1]][1])
t1_2 <- read.tree (text=stats$trees [[1]][2])
compare.chronograms (t1_1, t1_2)
for (locus in 1:Nloci) {
  ntrees <- length (stats$trees [[locus]])
  for (n in 1:ntrees) {
    if (locus == 1 && n ==1) {
      outPhy <- read.tree (text=stats$trees [[locus]][n])
    }
    else {
      outPhy <- ape:::c.phylo (outPhy, read.tree (text=stats$trees [[locus]] [n]))
    }
  }
}
par (mfrow = c(1,1))
densityTree (outPhy)


model2 <- coal_model (sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) +
  feat_mutation (10) +
  feat_recombination (20) +
  sumstat_trees () +
  sumstat_nucleotide_div ()
#'Based on the recombination rate going up, I would assume that there more distance between the branches that are super close together because there shouldn't be as strong of a linkage anymore.'
#'After running both models and looking at the plots, it seems like in the plot that was super close (phylogeny on the left) the branches branched earlier so the diverage of these groups occured eariler. Perhaps these loci were getting inherited together and the recombination rate split them eariler when the recombination rate was increased. As for the second plot (Phylogeny on the right), the new model actually has the branches branching later. So, it seems to kind of do the opposite in both cases. I suppose if these genes were not getting inherited together, recombination could have caused them to recombine and get inherited together more often, leading to more time before divergence in the phylogeny occured.'
#''Btw, to run this model, I did the libraries and then I ran this model2 line and then started with the code under model 1. So if you just run all the code in its order, keep this in mind.'


model3 <- coal_model (10, 50) +
  feat_mutation (par_prior ("theta", sample.int (100, 1))) +
  sumstat_nucleotide_div ()
stats <- simulate (model3, nsim = 40)
mean_pi <- sapply (stats, function (x) mean (x$pi))
theta <- sapply (stats, function (x) x$pars [["theta"]])
