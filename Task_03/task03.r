trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm (1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm (1e6, trueMean2, trueSD2)
Size <- 50
Sample1 <- sample (population1, Size)
Sample2 <- sample (population2, Size)
Sample1
Sample2
#'Yes the populations were different. You could know that from the code making the populations. Even though the mean is the same, the standard deviations are different (in the populations). So, Since the populations were different, it is likley that the samples of those said populations are different as they are meant to represent the populations themselves.' 
boxplot (Sample1, Sample2)
source ("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder ("grandma_mom")
MatGrandpa <- makeFounder ("grandpa_mom")
PatGrandma <- makeFounder ("grandma_da")
PatGrandpa <- makeFounder ("grandpa_da")
Alan <- makeBaby (PatGrandma, PatGrandpa)
Brenda <- makeBaby (MatGrandma, MatGrandpa)
head (MatGrandma)
head (MatGrandpa)
nrow (MatGrandma)
nrow (MatGrandpa)
head (PatGrandma)
head (PatGrandpa)
nrow (PatGrandma)
nrow (PatGrandpa)
head (Alan)
nrow (Alan)
tail (Alan)
head (Brenda)
nrow (Brenda)
Focus <- makeBaby (Brenda, Alan)
#'It should be 50% from Brenda right?'
ToMom <- length (grep ("mom", Focus)) / length (Focus)
#'Okay so, these are coming from each of these, it should be for roughly 25%, it just must add to 50% all together. After checking, yes this is correct.'
ToMomMom <- length (grep ("grandma_mom", Focus)) / length (Focus)
ToMomDad <- length (grep ("grandpa_mom", Focus)) / length (Focus)
ToDadDad <- length (grep ("grandpa_da", Focus)) / length (Focus)
ToDadMom <- length (grep ("grandma_da", Focus)) / length (Focus)
#'Focus is not equally related among all grandparents, in the paternal or maternal groups or both groups combined. However, the average, has to be 25% (of DNA passed down) because all the grandparent's percentages must add to 100% total. 100%/4 = 25%. Is this what I expected - theoretically (because they're not all an equal 25%) no but practically yes. The results are not hard to believe.'
Sibling_01 <- makeBaby (Brenda, Alan)
#'Um, the amount shared between siblings can vary, just as the amounts from the grandparents do. However, theoretically it should be 50% shared between siblings. It came out as 42.585% which is pretty close to 50%, so I am not surprised at this result."
ToSib <- length (intersect (Focus, Sibling_01)) / length (Focus)
#'How many genes are shared between Focus and all the siblings. Well, that'll vary but the mean should be around 50%. It does not HAVE to be this certain percent like in the grandparent situation or the parent situation though.'
ManySiblings <- replicate (1e3, length (intersect (Focus, makeBaby (Brenda, Alan))) / length (Focus))
quantile (ManySiblings)
mean (ManySiblings)
plot (density (ManySiblings), main="", xlab = "proportion shared genes")
#'Why do we see a range of loci shared? This is a good thing to understand. So, when sexual reproduction take place, 50% of the passed down loci are from one parent and the other 50% come from the other parent. However, the 50% that is passed down from each parent is completely random. So, theorectically (practically not going to happen) you can have NO loci shared between 2 siblings. If the 50% of the two parents don't overlap with what is passed down to each sibling respectively in each parent. I hope this answered the question, I understand the concept and don't want to get too much into the nitty gritty details. Just the 50% from each parent is random so the offspring will represent this randomness.'
HWE <- function (p) {
  aa <- p^2
  ab <- 2 * p * (1-p)
  bb <- (1 - p)^2
  return (c (aa=aa, ab=ab, bb=bb))
}
HWE (0.5)
plot (1, 1, type = "n", xlim = c (0,1), ylim = c (0,1), xlab = "freq. allele a", ylab = "geno. freq")
p <- seq (from = 0, to = 1, by = 0.01)
GenoFreq <- t(sapply (p, HWE))
lines (p, GenoFreq [, "aa"], lwd = 2, col = "red")
#'Yes I understand what the plot is saying. We went over this in class. As the freq of a increase in the population so does the amount of homozygous aa members and vice versa. Is geographic space? Time is not depicted on this plot. No I don't think the plot has anything to do with that.'
lines (p, GenoFreq [, "ab"], lwd = 2, col = "purple")
lines (p, GenoFreq [, "bb"], lwd = 2, col = "blue")
legend ("top", legend = c ("aa", "ab", "bb"), col = c ("red", "purple", "blue"), lty = 1, lwd = 2, bty = "n")
Pop <- simPop (500)
points (Pop [, "freqa"], Pop [, "Genotypes.aa"] / 500, pch = 21, bg = "red")
Pop <- simPop (50)
#' Um, hmm. I would say there are more aa than what there should be.'
points (Pop [, "freqa"], Pop [, "Genotypes.aa"] / 50, pch = 22, bg = "red")
#'To me, it seems like the range actually increased and it seems it conforms slightly better to the HWE. I might need this explained alittle better.'
library (learnPopGen)
x <- genetic.drift (Ne = 200, nrep = 5, pause = 0.01)
x <- genetic.drift (Ne = 200, nrep = 5, pause = 0.01)
x <- genetic.drift (Ne = 200, nrep = 5, pause = 0.01)
x <- genetic.drift (Ne = 200, nrep = 5, pause = 0.01)
PopSizes <- 5:50
Samples <- rep (PopSizes, 5)
tExt <- sapply (Samples, function (x) nrow (simPop (x, 500)))
Line <- lm (tExt ~ Samples)
summary (Line)
Line$coef
plot (Samples, tExt)
abline (Line)
plot (Samples, tExt)
Line2 <- lm (tExt ~ Samples + 0)
abline (Line2)
#'Line2 looks to start from basically 0 and slopes up further. I actually like this line better because it slopes up firther and should decrease heteroskedasticity - I think."
#'The points get further from the line as the population size increases. From the extra credit, I know this is due to heteroskedasticity.'

install.packages("lmtest")
install.packages("sandwich")
library (lmtest)
library (sandwich)

NewLine <- lm (tExt ~ Samples)
summary (NewLine)
coeftest (NewLine, vcov = vcovHC (NewLine, type = "HC0"))
Robust <- coeftest (NewLine, vcov = vcovHC (NewLine, type = "HC0"))
summary (Robust)
NewLine$coef
plot (Samples, tExt)
abline (Robust)
