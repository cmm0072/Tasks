install.packages('diversitree')
library('diversitree')
transition_0to1 <- 0.1
transition_1to0 <- 0.1

speciation_0 <- 0.2
extinction_0 <- 0.15

speciation_1 <- 0.4
extinction_1 <- 0.1	

maxN <- 1e3
maxT <- 50

Pars <- c(speciation_0, speciation_1, extinction_0, extinction_1, transition_0to1, transition_1to0)

simTree <- tree.bisse(Pars, max.taxa = maxN, max.t = maxT)
str(simTree)
?tree.bisse ()

stateTable <- table(simTree$tip.state)
stateTable / sum(stateTable)


#'Question 1 code is below.'


Frequencies <- c('State 0', 'State 1')
Dataq1 <- read.csv ("Q1_Data.csv")
Dataq1
Dataq1Freq <- Data1[,1:2]
Dataq1Div <- Data1 [,5]
?barplot

Data1 <- matrix(c(0.6, 0.7, 0.5, 0.65, 0.21, 0.4, 0.3, 0.5, 0.35, 0.79, 0.50, 0.4, 0.4, 0.3, 0.55, 0.3, 0.25, 0.3, 0.25, 0.50), nrow = 4, ncol = 5, byrow=TRUE)
Data1
Differences <- c()
Freq1 <- c(0.6, 0.7, 0.5, 0.65, 0.21)
Freq0 <- c(0.4, 0.3, 0.5, 0.35, 0.79)

pdf('Q1.pdf', height =6, width=8)
barplot(Data1, names.arg=Difference,main = 'Diversification Rate Verses Frequency',xlab = '',ylab = 'Frequency and Diversification Rate',beside=TRUE,col = c('blue', 'black', 'red', 'green'))
?legend
legend('left', Frequencies, fill = 'blue', 'Stage0 Freq')
legend ('top', Frequencies, fill = 'black', 'Stage1 Freq')
legend ('right', Frequencies, fill = 'red', 'Stage0 DIversification')
legend('bottom', Frequencies, fill = 'green', 'Stage1 Diversification')
dev.off()
?boxplot
#'Question 1 con't - Yes, there is a instance where Stage 1 had a higher freq but lower diversification rate.'
#''Please ignore the numbers on the x axis, I cannot get them to remove. They are not relevant, I am sorry.'

#'Question 2 code is below.'

Frequencies <- c('State 0', 'State 1')
Data2 <- matrix(c(0.5, 0.7, 0.9, 1, 0.9, 0.5, 0.3, 0.1, 0, 0.1, 0.1, 0.1, 0, 0.1, 0.2, 0.2, 0.2, 0.1, 0.1, 0.1), nrow = 4, ncol = 5, byrow=TRUE)
Freq1.1 <- c (0.5, 0.7, 0.9, 1, 0.9)
Freq2.1 <- c (0.5, 0.3, 0.1, 0, 0.1)


pdf('Q2.pdf', height = 6, width = 8)
barplot(Data2, names.arg=Difference,main = 'Freq and Transition',xlab = '',ylab = 'Frequency and Transition Rate',beside=TRUE,col = c('blue', 'black', 'red', 'green'))
barplot(Data, names.arg=Difference, main='State 1 At Its Lowest Frequencies',xlab='Differences in Diversification Rate',ylab='Frequencies',col=c('blue', 'black'))
legend('left', Frequencies, fill = 'blue', 'Stage0 Freq')
legend ('top', Frequencies, fill = 'black', 'Stage1 Freq')
legend ('right', Frequencies, fill = 'red', 'Stage0 Trans')
legend('bottom', Frequencies, fill = 'green', 'Stage1 Trans')

dev.off()
#'Question 2 con't - When Stage 1 reaches 0, there is a 0.1 transition rate in both stages observed. '
#''Again, please ignore the numebers along the x-axis - I am sorry.'

#' I plan to finish this afer class today, but I wanted to submit what I have on time. I  am extremely confused with task but I have given it my best attempt and will do the same when as I try to finish it."
#' I think I saw in the sylabus that if we work with someone, we should put that in the comments. I asked Raza for help and he showed me some of the code that he used. I didn't really understand it but I don't feel right about probing him with questions, I know everyone is busy. However, technically we worked together since he showed me an example of how he did the task. Just putting this here because I think the syllabus tells me to. I also did not use his numbers and went about the the 2nd Question/senerio different.'


#'Question 3, code below'
dir ()
Data3 <- matrix (0.5, 0.7, 0.8, 0.35, 0.1, )

Freq1trail1 <- Data3[,2]
Freq1trail2 <- Data3[,4]
Freq1trail3 <- Data3[,6]

VarT1 <- var (Freq1trail1)
VarT2 <- var (Freq1trail2)
VarT3 <- var (Freq1trail3)
VarianceT123 <- c (Freq1trail1, Freq1trail2, Freq1trail3)
VarianceT123

Trials <- c (1,2,3)
barplot(VarianceT123, main='Variance Over Freqs',xlab='Freq',ylab='Variance of Frequencies', col='blue')
#'Each bar represents a freq, and are grouped in 5 - as there are five freqs per trail. Freq has high variance at the start and it reduces in the second trail, but shoots back up in the 2nd trail.'

#'Question 4 - Evolutionary processes such as natural selection and drift shape frequencies. Then factors affect those processes, such as Ne does with drift, which will end up shaping frequencies through manipulating the extent of effect in the evolutionary processes they influence.'

#'Honestly, I know this task was not well done by me. I will be the first to admit that. I did, however, do the best I felt I could.'
