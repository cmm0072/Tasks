source ("http://jonsmitchell.com/code/fxn05.R")
simPop (Popsize = 50, nGeneration = 100, initial_p = 0.5, h= 1, s = 0)
Pop1 <- simPop (Popsize = 50, nGeneration = 100, initial_p = 0.5, h= 1, s = 0)
plot (1:nrow (Pop1), Pop1[,1], ylim = c (0,1), type = "l", xlab = "generation", ylab = "allele freq.", lwd = 2)
lines (1:nrow (Pop1), Pop1[,2], lwd = 2, col = 'red')
legend ("topleft", legend = c ("a", "b"), col = c ("black", "red"), lwd = 2, bty = "n")
plotFit (nruns = 10, n = 50, ngens = 100, init_p = 0.5, h = 1, s = 0)
Expectation <- c (10, 10, 10, 10)
Observed <- c (15, 15, 5, 5)
Chisq <- sum (((Expectation - Observed)^2) / Expectation)
barplot (rbind (Expectation - Observed), beside = T, main = bquote (chi^2 ~ "=" ~. (Chisq)), legend.text = c ("expected", "observed"))
results <- read.csv ("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors = F)
counts <- results [, c("yellow", "red", "green", "blue", "black", "tan")]
backgrounds <- c ("White", "Red", "Yellow", "Green", "Blue", "Black")
backgroundCol <- c ("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")
calcChi (counts [1,])
Chisqs <- apply (counts, 1, calcChi)
plotChis (counts)
Avg <- mean (Chisqs)
backgroundAvgs <- tapply (Chisqs, results [,3], mean)
propSig <- length (which (Chisqs > 11.70)) / length (Chisqs)
precSig <- round (100 * propSig)
par (las=1, mar=c(4, 4, 1, 1), mgp =c(2, 0.5, 0), tck = -0.01, cex.axis = 1)
hist (Chisqs, main = "", xlab = "chi-squared values", ylab = "frequency")
par (las = 1, mar = c (4, 4, 1, 1), mgp = c (2, 0.5, 0), tck = -0.01, cex.axis = 1)
plot (1, 1, xlim = c (0, 400), ylim = c (1, 8.5), xlab = "", ylab = "", type = "n", yaxt = "n")
axis (2, at = 1:length (backgrounds), labels = backgrounds)
mtext (side=1, expression (chi^2), cex=1.75, line=2.5)
counter <- 1
for (i in backgrounds) {
  Data <- Chisqs [which (results [,3] == i)]
  addHist (Y=counter, Dat = Data, Color = backgroundCol [counter])
  counter <- counter + 1
}
abline (v = 11.70, lty = 2, lwd = 2, col = 'black')
Simulation <- simDraws (10000)
addHist (Y=7, Dat=Simulation, Color="lightgray")
mtext (side = 2, at = 7, line = 0, "simulated")
abline (v = 11.70, lty = 2, lwd =2)
Fit <- c (1, 1, 1, 1, 1, 1)
names (Fit) <- 1:6
Simulation2 <- simDraws (1e4, w = Fit)
addHist (Y=8, Dat=Simulation2, Color=rgb (0, 0, 0, 0.25))
Fit <- c (0.1, 1, 1, 1, 1, 1)
names (Fit) <- 1:6
Simulation3 <- simDraws (1e4, w = Fit)
addHist (Y=8, Dat=Simulation3, Color = rgb (0, 0, 0, 0.25))
Fit <- c (0.5, 0.6, 0.7, 1, 1, 1)
names (Fit) <- 1:6
Simulation4 <- simDraws (1e4, w = Fit)
addHist (Y=8, Dat=Simulation4, Color=rgb (0, 0, 0, 0.25))
Fit <- c (0.1, 0.2, 0.3, 0.4, 0.5, 1)
names (Fit) <- 1:6
Simulation5 <- simDraws (1e4, w = Fit)
addHist (Y=8, Dat=Simulation5, Color = rgb (0, 0, 0, 0.25))
Fit <- c (0.1, 0.1, 0.1, 0.1, 0.1, 1)
names (Fit) <- 1:6
Simulation6 <- simDraws (1e4, w = Fit)
addHist (Y=8, Dat=Simulation6, Color=rgb (0, 0, 0, 0.25))
mtext (side=2, at=8, line=0, "sel. sim")
Simulation7 <- c (Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist (Y=8, Dat=Simulation7, Color=rgb (0, 0, 1, 0.25))
#'As for what evolutionary forces are at work for when the students are picking the colored sticks,I would say selection. For selection, the way I see it is that if the stick is larger - there is a higher chance of it being grabbed so smaller sticks should be selected for. Also, perhaps a little drift from like someone distracting them and they knocked the sticks into the floor or something.'
#'As for the simulation, I would assume both drift, selection, gene flow, mutation, etc. are taken into account.'
#'Honestly, the way the graphs are popping up in my R, it is hard to exactly read them. Also, I think I may be a little confused and I would like to discuss this some when we have our meeting next week.' 
#'I think comparing the students to a critical value instead of the simulation is more accurate because a simulation is just a single option or example of how it could go. It could just so have happened that the simulation produced some not to predictable numbers and making that a basis of comparison would be bad in that instance I think. I think it is safer to go with the critical value in most cases. However, if the chi test doesn't take drift into account then it would be interesting to compare the students data to the simulation to see how the lack of drift affects the freq found in the experiment.'
#'If we allowed a stick to mutate to a different type, it would introduce drift a lot more because it could increase genetic diversity, which would give drift a larger impact. So, there are two options here, if the chi x2 value incorperates the idea of drift in its method then the X2 value would become more "normal" I think. However, if the chi test does not consider this when conducting and comparing the values to a critical value then I think the values would become less normal or further from the critical value.'

