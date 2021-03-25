getwd ()
install.packages ("readxl")
library(readxl)
Mus1UsedData <- read_excel("Mus1UsedData.xlsx")
View(Mus1UsedData)
#'I want to change my hypothesis to mice that are pure bred (non hybrids) have more offspring than mixed bred (hybrids).'
#'For my graph, I would want number of offspring on the y axis and two groups, 'pure' and 'mixed' on the x axis. I would like the chart to have the large block with the error bars.'
#'For my simple analysis plan, I would like to prefrom a 2 sample variance test to see if variance is the same. If that pans out to be equal, then I want to do a one-tailed t-Test to see if the pure group have more offspring than the mixed group.'