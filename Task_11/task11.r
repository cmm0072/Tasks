x <- rnorm (100, mean = 5, sd = 2)
var (x)
mean (x)

y <- ((x * 5) + 2) + runif (100, 0, 0.1)
plot (x, y)
abline (lm (y~x) , col = 'red')

coef (lm (y~x))
lm (y~x)


#'The slope should be ~ 5 and the x intercept would be ~ 4.998. I chose two points on the graph and did it myself.'
#'The y intercept is ~2.054'
z <- c ()
x <- rnorm (100, mean = 5, sd = 2)

for (i in 1:100) {
  z[i] <- runif (1)
  y <- (x * z[i]) +2 + (rnorm (100, 0:0,1))
  lM <- coef (lm (z[1:100]~y))
}
plot (z [1:100], y )
abline (lm (y~z[1:100]))
coef (lm (y~x))


install.packages ('meme')
library (meme)
install.packages( ('imga'))
dir ()


u <- "pic.jpg"


meme1 <- meme (u, "Brace For Impact", "Genetic Drift Incoming")

