setwd('C:\\Users\\USER\\Desktop\\Evolution\\Tasks\\Task_02')
getwd()
Data <- read.csv('http://jonsmitchell.com/data/beren.csv' , stringsAsFactors=F)
write.csv (Data, 'rawdata.csv', quote=F)
length (Data)
nrow (Data)
ncol (Data)
colnames (Data)
#'The data found here are the columns of the matrix. So, we have time data - event data, etc.'
head (Data)
Data [1,]
Data [2,]
Data [1:3,]
Data [1:3, 4]
Data [1:5, 1:3]
#'I played around with this a bit, after running through the code. It seems as tho, the first value is for the rows and the second is for the columns when there is two. When you only add one value, it gives all the columns and just the number of rows equal to the value you put in'
#'I am not entirely sure how I would go about getting to the 257th observation. I need to come to your office.'
Feeds <- which (Data[,9] == 'bottle')
berenMilk <- Data[Feeds ,]
head (berenMilk)
#'Each row represents a date and the amount of oz of milk beren drank on the corresponding date along with the caregiver that gave him the milk.'

Feeds <- which (Data[,'event'] == 'bottle')
Feeds <- which (Data$event == 'bottle')
dayID <- apply (Data, 1, function (x) paste (x[1:3], collapse ='-'))

dateID <- sapply (dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data$age <- dateID - dateID[which (Data$event == 'birth')]
#'I keep getting an error here. "Error in `$<-.data.frame`(`*tmp*`, age, value = c(`2019 -` = NA_real_,  :"' 
#  replacement has 4068 rows, data has 1356. This was fixed by typing collapse correctly."'

head (Data)
#'Does not change the file, got it'
beren2 <- Data
beren3 <- beren2 [order (beren2$age),]
write.csv (beren3, 'beren_new.csv', quote=F, row.names=FALSE)
Data [1,]
Data [1:3]
Data [1:3, 4]
Data [1:5, 1:3]
#'The Data [] lines above this, I was just playing around with it to understand it better'
#'My understanding of length is that is shows the length of a vector or a sequence or something of the sort. So, length (1) = 1 because there is 1 "thing" which is 1. However, 1 <- c(1,2,3) , then length (1) should theoretically equal 3 if you can name vectors numbers. So, length (Data) equals 12 because there is 12 different things (columns) in that data.
#'My understanding of nrow is that it shows the number if rows or columns present (that is what google tells me). But it confuses me a bit on why nrow (Data) = 1356'
#'Upon further research, it seems that length () gives the number of colums and nrow () the number of rows, which makes more sense.'
#'From the swirl lessons. I think the way they explained the difference is that a matrix is like a 2 dimensional vector. The length of a vector would be the columns and then if you add rows to it, you get a matrix'
#'Referring to my swirl lesson notes, the which () function is used to determine what parts of a vector return as true. I understand the theory of which () but as for applying in the two exmaples, I would need that to be explained to me.'
#'I looked through my swirl notes and I came across this 'x[!is.na(x)]' and it says that it gives all the non NA things in a vector. So, I think I could use this in order to see how many different numbers were in the list. Then if you didnt want to count, could you do sum (x[!is.na(x)]) ?'
length (dayID)
length (dateID)
setwd('C:\\Users\\USER\\Desktop\\Evolution\\Tasks\\Task_02')
getwd()
Data <- read.csv('http://jonsmitchell.com/data/beren.csv' , stringsAsFactors=F)
write.csv (Data, 'rawdata.csv', quote=F)
length (Data)
nrow (Data)
ncol (Data)
colnames (Data)
head (Data)
Data [1,]
Data [2,]
Data [1:3,]
Data [1:3, 4]
Data [1:5, 1:3]
Feeds <- which (Data[,9] == 'bottle')
berenMilk <- Data[Feeds ,]
head (berenMilk)
Feeds <- which (Data[,'event'] == 'bottle')
Feeds <- which (Data$event == 'bottle')
dayID <- apply (Data, 1, function (x) paste (x[1:3], collapse ='-'))
dateID <- sapply (dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data$age <- dateID - dateID[which (Data$event == 'birth')]
head (Data)
beren2 <- Data
beren3 <- beren2 [order (beren2$age),]
write.csv (beren3, 'beren_new.csv', quote=F, row.names=FALSE)
beren <- read.csv("http://jonsmitchell.com/data/beren.csv", stringsAsFactors=F)
dayID <- apply(beren, 1, function(x) paste(x[1:3], collapse="-"))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
beren$age <- dateID - dateID[which(beren$event == "birth")]
beren2 <- beren[order(beren$age),]
beren2$value <- as.numeric(beren2$value)
#'Question 1: Both the first two hypotheses involve comparing two different events to one another, while the 3rd one simply looks at one event and how it changes over time.'
Feeds <- which (beren3$event == 'bottle')
avgMilk <- mean (beren3$value [Feeds])
#' I feel like the units for this would be oz unless I am missing something'
#' You used the value column because that is where the amount of milk (in oz) that beren drank.)
#' The [Feeds] tells it to only look at the values that go with the event of bottle. So, yes it is important because you only want to take the mean of the values that go along with the bottle event'
#' 
avgFeed <- tapply (beren3$value [Feeds], beren3$age [Feeds], mean)
varFeed <- tapply (beren3$value [Feeds], beren3$age [Feeds], var)
totalFeed <- tapply (beren3$value [Feeds], beren3$age [Feeds], sum)
numFeeds <- tapply (beren3$value [Feeds], beren3$age [Feeds], length)
cor (beren3$value [Feeds], beren3$age [Feeds])
#'-0.08224024'
cor.test (beren3$value [Feeds], beren3$age [Feeds])
#' t = -0.18983242, df = 320, p-value = 0.1409. 98 pervent confidence intereval : -0.18983242 and 0.02730373.'
berenCor <- cor.test (beren3$value [Feeds], beren3$age [Feeds])
summary (berenCor)
berenANOVA <- aov (beren3$value [Feeds] ~ beren3$caregiver [Feeds])
boxplot (beren3$value [Feeds] ~ beren3$caregiver [Feeds], xlab = "who gave the bottle", ylab = "amount of milk consumed (oz)")
par (las = 1, mar = c (5,5,1,1), mgp = c (2, 0.5, 0), tck = -0.01)
?par
#' las - style of axis labels. So one means "always horizontal". mar seems to me to be controling the outline of the graph. mgp - I had to google this one.Google says that that it controls the axis label position in relation to the "inner plot window". The first number controls location of axis labels. The second one  . tck - the length of the tick marks and kind of where they are located by the - or + number'
plot (as.numeric (names (totalFeed)), totalFeed, type = "b", pch = 16, xlab = "age in days", ylab = "ounces of milk")
abline (h = mean (totalFeed), lty = 2, col = 'red')
pdf ('r02b-totalMilkByDay.pdf', height = 4, width = 4)
#'I get an error here. - Error: unexpected symbol in "pdf (''r02b"'
par (las = 1, mar = c (5,5,1,1), mgp = c (2, 0.5, 0), tck = -0.01)
plot (as.numeric (names (totalFeed)), totalFeed, type = "b", pch = 16, xlab = "age in days", ylab = "ounces of milk")
abline (h = mean (totalFeed), lty = 2, col = 'red')
dev.off ()
#'My hypothesis is that beren's length will increase as he ages.'

Bonus <- which (Data[,9] == 'nap')
Naps <- Data [Bonus,]
head (Naps)
Bonus <- which (Data [,'event'] == 'nap')
Bonus <- which (Data$event == 'nap')
startHour <- (Naps$start_hour)
startMin <- (Naps$start_minute)
stopHour <- (Naps$end_hour)
stopMin <- (Naps$end_minute)
startHour
startMin
stopHour
stopMin
Naps$NapDuration <- ((stopHour - startHour) * 60 + (stopMin - startMin))
head (Naps)
TotalSleep<-tapply(Naps$NapDuration, Naps$age, sum)
TotalSleep
par(las = 1, mar= c (5,5,1,1), mgp = c (2,0.5,0), tck = -0.01)
plot(as.numeric (names (TotalSleep)), TotalSleep, type = "b", pch = 16, xlab = "Age in Days", ylab = "Nap Duration in Minutes")
cor.test (Naps$start_hour, Naps$NapDuration)
#'The cor test gave a cor value of -0.1079752, t = -1.3781, df = 161, p-value = 0.1701. 95 percent confidence interval returned - -0.25742303 and 0.04651734. The alternative hypothesus us that the correlation is not equal to 0'
NapsCor <- cor.test (Naps$start_hour, Naps$NapDuration)
pdf ('r02b-totaltimesleptagaisntage.pdf', height = 4, width = 4)
#'Everything below this line was some of the code I attempted in order to get the bonus. I moved what actually worked under the lines of code from the actualy assignment so that if you were to just run the code from start to this line, it should work correctly. Also, I left the other attempts so that you can see them and so that I can look back on them and compare them to the correct lines at a later date if needed'


Bonus <- which (Data[,9] == 'nap')
Naps <- Data [Bonus,]
head (Naps)
Bonus <- which (Data [,'event'] == 'nap')
Bonus <- which (Data$event == 'nap')
StartTimeID <- apply(beren, 1, function(x) paste(x[5:6], collapse="-"))
StopTimeID <- apply(beren, 1, function(x) paste(x[7:8], collapse="-"))



NapDuration <- difftime (StartTimeID, StopTimeID)
Data$Duration <- difftime (StartTimeID, StopTimeID)
DurationID <- sapply(StartTimeID, as.Time, format = "%I-%M-%p",)
Data$Duration <-  StartTimeID - StopTimeID[which (Data$event == 'nap')]
timevector <- c (StartTimeID, StopTimeID)
diff (timevector)
?difftime
NapDurationID <- sapply(StopTimeID, as.numeric, format = "%I-%M-%p", origin = "StartTimeID")
Data$Duration <- NapDurationID - NapDurationID [which (Data$event == 'nap')]
NapDurationID <- 1

start_hour - end_hour
StartTimeID - StopTimeID
head(Naps)
StartTimeID <- as.numeric
StopTimeID <- as.numeric
StopTimeID - StartTimeID
Naps <- as.numeric (Naps$start_hour)
Naps <- as.numeric (Naps$StartTimeID)
Duration <- diff (StopTimeID, StartTimeID, lag = 1)
beren <- read.csv("http://jonsmitchell.com/data/beren.csv", stringsAsFactors=F)
unique (beren3$event)
HourDif <- Diff (Naps [5,7])

Bonus <- which (Data[,9] == 'nap')
Naps <- Data [Bonus,]
head (Naps)
Bonus <- which (Data [,'event'] == 'nap')
Bonus <- which (Data$event == 'nap')
startHour <- (Naps$start_hour)
startMin <- (Naps$start_minute)
stopHour <- (Naps$end_hour)
stopMin <- (Naps$end_minute)
startHour
startMin
stopHour
stopMin
Naps$NapDuration <- ((stopHour - startHour) * 60 + (stopMin - startMin))
head (Naps)
TotalSleep<-tapply(Naps$NapDuration, Naps$age, sum)
TotalSleep
par(las = 1, mar= c (5,5,1,1), mgp = c (2,0.5,0), tck = -0.01)
plot(as.numeric (names (TotalSleep)), TotalSleep, type = "b", pch = 16, xlab = "Age in Days", ylab = "Nap Duration in Minutes")
cor.test (Naps$start_hour, Naps$NapDuration)
#'The cor test gave a cor value of -0.1079752, t = -1.3781, df = 161, p-value = 0.1701. 95 percent confidence interval returned - -0.25742303 and 0.04651734. The alternative hypothesus us that the correlation is not equal to 0'
pdf ('r02b-totaltimesleptagaisntage.pdf', height = 4, width = 4)

TotalSleep <- sum (Naps$NapDuration)
TotalSleep
?sum
Totalsleep <- sum (Naps$NapDuration, na.rm = FALSE)
Totalsleep

totalSleep <- tapply (Naps$SleepDuration [Bonus], sum)



tail (Naps)


#'My hypothesis is that beren's length will increase as he ages.'