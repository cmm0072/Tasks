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
  replacement has 4068 rows, data has 1356. This was fixed by typing collapse correctly."'
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