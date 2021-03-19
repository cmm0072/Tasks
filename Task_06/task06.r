source ("http://jonsmitchell.com/code/reformatData07.R")
source ("http://jonsmitchell.com/code/simFxn.R")
plot (1, 1, type = "n", xlim=c (1998, 2013), ylim=c (0, 1))
s <- apply (overallFreq, 2, function (x) lines (overallFreq [,1], x, col=rgb (0, 0, 0, 0.01)))
rescaleFreq <- apply (overallFreq [,3:ncol (overallFreq)], 2, function (x) x - x[1])
plot (1, 1, type = "n", xlim=c (1998, 2013), ylim=c (-0.25, 0.25))
s <- apply (rescaleFreq, 2, function (x) lines (overallFreq [,1], x, col = rgb (0,0,0,0.01)))
dYear <- c()
dAlleles <- c ()
for (i in 3:ncol (overallFreq)) {
  dYear <- c (dYear, overallFreq [,1])
  Vec <- overallFreq [,i]
  Init <- overallFreq [1, i]
  dAlleles <- c (dAlleles, Vec - Init)
}
smoothScatter (dYear, dAlleles, colramp = Pal, nbin = 100)
smoothScatter (dYear, dAlleles, colramp = Pal, nbin = 100, xlab = "year", ylab = "change in allele freq. since 1998")
addFit (nruns = 50, n=100, ngens = 18, startT = 1997, simCol = "gray40", rescale = TRUE)
addFit <- function (nruns, n = 100, ngens = 100, init_p = 0.5, h=1, s=0, mu = 1e-6, cpt = 1.2, cLwd = 2, Alpha = 1, startT = 0, rescale = F, simCol = NULL) {
  if (is.null (simCol)) {
    Cols <- c(rgb(166/255,206/255,227/255,Alpha),rgb(31/255,120/255,180/255,Alpha),rgb(178/255,223/255,138/255,Alpha),rgb(51/255,160/255,44/255,Alpha),rgb(251/255,154/255,153/255,Alpha),rgb(227/255,26/255,28/255,Alpha),rgb(253/255,191/255,111/255,Alpha),rgb(255/255,127/255,0,Alpha),rgb(202/255,178/255,214/255,Alpha))
    Pal <- colorRampPalette (Cols, interpolate = "spline", alpha = T)
    simCol <- Pal (nruns)
  }
  else if (length (simCol) < nruns) {
    cat ("Plotting all lines using", simCol [1])
    simCol <- rep (simCol [1], nruns)
  }
}
#'So, n =100, h = 0.5, and s = 0.'
plot (alleleFreqs$d_freq, alleleFreqs$d_imm, xlim=c (-0.15, 0.15), xlab = "overall freq. change", ylab = "freq. change in subject")
points (alleleFreqs$d_freq, alleleFreqs$d_birth, col - 'blue')
points (alleleFreqs$d_freq, alleleFreqs$d_surv, col = 'red')