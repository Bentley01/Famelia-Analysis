## Read data from csv - make sure dollar amounts do not have comma separaters
data <- read.csv("Famelia Sales and COS 2011-2015.csv", colClasses = "character")

head(data, 5)


## Convert string date to R Date
data$Date <- as.Date(data$Date, "%d-%m-%y")
## Make Debit and Credit columns numeric
data$Debit <- as.numeric(data$Debit)
data$Credit <- as.numeric(data$Credit)

str(data)
## Create new Amount column Debit(+) and Credit(-)
for (i in 1:nrow(data)){
    if (is.na(data$Debit[i])){
        data$Amount[i] <- - data$Credit[i]
    }
    else{
        data$Amount[i] <- data$Debit[i]
    }
}

## get rid of Debit and Credit columns - no longer needed
data <- data[-c(3,4)]

## create list of Sales account levels for subsetting
sales <- c("Food Sales","Non-Alcoholic Sales", "Soft Drink Sales", "Beer Sales","Liquor Sales","Wine Sales","Catering Sales")
## create list of restaurant sales levels - no catering/function room rentals
restSales <- c("Food Sales","Non-Alcoholic Sales", "Soft Drink Sales", "Beer Sales","Liquor Sales","Wine Sales")

## Create list of COS account levels for subsetting
cos <- c("Food Purchases","Beer Purchases","Liquor Purchases","Wine Purchases","Misc Food","Kitchen Wine & Cooking Alcohol","Soft Drinks", "Bar Fruit and Herbs","Tea & Coffee")

## Create subset of sales account data only
salesdata <- subset(data, Account %in% sales, drop=TRUE, na.rm=T)
salesdata <- na.omit(salesdata)
restdata <- subset(data, Account %in% restSales, drop=TRUE, na.rm=T)
restdata <- na.omit(restdata)
cosdata <- subset(data, Account %in% cos, drop=TRUE, na.rm=T)
cosdata <- na.omit(cosdata)
data <- na.omit(data)
## Make Account a factor, with levels equal to sales categories
salesdata$Account <- as.factor(salesdata$Account)
restdata$Account <- as.factor(restdata$Account)
cosdata$Account <- as.factor(cosdata$Account)

library("reshape2")
## Convert salesdata from long to wide, using Account 
## results in columns for sales categories
## entries by date equal sum of Amounts, by column
salesdata <- dcast(salesdata, Date~Account, sum)
restdata <- dcast(restdata, Date~Account, sum)
cosdata <- dcast(cosdata, Date~Account, sum)
salesdata$`Beverage Sales` <- salesdata$`Non-Alcoholic Sales` + salesdata$`Soft Drink Sales`
salesdata$`Food Sales` <- salesdata$`Food Sales` + salesdata$`Catering Sales`
head(salesdata)
salesdata <- salesdata[-c(3,6,7)]
head(salesdata)

restdata$`Beverage Sales` <- restdata$`Non-Alcoholic Sales` + restdata$`Soft Drink Sales`
head(restdata)
restdata <- restdata[-c(5,6)]
head(restdata)

cosdata$`Beverage COS` <- cosdata$`Soft Drinks` + cosdata$`Tea & Coffee`
cosdata$`Food Purchases` <- cosdata$`Food Purchases` + cosdata$`Kitchen Wine & Cooking Alcohol` + cosdata$`Misc Food`
cosdata$`Liquor Purchases` <- cosdata$`Liquor Purchases` + cosdata$`Bar Fruit and Herbs`
head(cosdata)
cosdata <- cosdata[-c(2,5,7,8,9)]
head(cosdata)

## Create a total sales column equal to sum of sales category columns
salesdata$Sales <- rowSums(salesdata[,2:6])
restdata$Sales <- rowSums(restdata[,2:6])
## Create a Day of Week column for the actual day of week, based on Date
salesdata$DofW <- format(salesdata$Date, "%a")
restdata$DofW <- format(restdata$Date, "%a")




restdata$Month <- format(restdata$Date, "%m")
## Make DofW a factor, with levels equal to 7 days of the week
salesdata$DofW <- as.factor(salesdata$DofW)
restdata$DofW <- as.factor(restdata$DofW)

## Change order of day of week factor levels 
salesdata$DofW <- factor(salesdata$DofW, levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
restdata$DofW <- factor(restdata$DofW, levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))

## Create seasonal classes - using month # (start, end)
winter = c(1,3)
spring = c(4,5)
summer = c(6,8)
fall = c(9,12)

winterdata <- subset(restdata, as.numeric(format(restdata$Date, "%m")) >= winter[1] & as.numeric(format(restdata$Date, "%m")) <= winter[2], drop=T, na.rm=T)
winterdata <- na.omit(winterdata)
springdata <- subset(restdata, as.numeric(format(restdata$Date, "%m")) >= spring[1] & as.numeric(format(restdata$Date, "%m")) <= spring[2], drop=T, na.rm=T)
springdata <- na.omit(springdata)
summerdata <- subset(restdata, as.numeric(format(restdata$Date, "%m")) >= summer[1] & as.numeric(format(restdata$Date, "%m")) <= summer[2], drop=T, na.rm=T)
summerdata <- na.omit(summerdata)
falldata <- subset(restdata, as.numeric(format(restdata$Date, "%m")) >= fall[1] & as.numeric(format(restdata$Date, "%m")) <= fall[2], drop=T, na.rm=T)
falldata <- na.omit(falldata)


## Calculate average sales by day of week and standard deviation
dayMeans <- tapply((-salesdata$Sales), salesdata$DofW, mean)
daySds <- tapply((-salesdata$Sales), salesdata$DofW, sd)
restdayMeans <- tapply((-restdata$Sales), restdata$DofW, mean)
restdaySds <- tapply((-restdata$Sales), restdata$DofW, sd)

winterdayMeans <- tapply((-winterdata$Sales), winterdata$DofW, mean)
winterdaySds <- tapply((-winterdata$Sales), winterdata$DofW, sd)
springdayMeans <- tapply((-springdata$Sales), springdata$DofW, mean)
springdaySds <- tapply((-springdata$Sales), springdata$DofW, sd)
summerdayMeans <- tapply((-summerdata$Sales), summerdata$DofW, mean)
summerdaySds <- tapply((-summerdata$Sales), summerdata$DofW, sd)
falldayMeans <- tapply((-falldata$Sales), falldata$DofW, mean)
falldaySds <- tapply((-falldata$Sales), falldata$DofW, sd)

summary(restdata)
summary(cosdata)

library("ggplot2")
## Box plot of sales by day
ggplot(salesdata, aes(x=factor(DofW), y=-Sales)) + geom_boxplot(outlier.size=1.5, outlier.shape=21) + stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")

## Violin plot of sales by day
ggplot(salesdata, aes(x=factor(DofW), y=-Sales)) + geom_violin(trim=FALSE, scale="count", fill="blue") + geom_boxplot(width=.1, fill="lightblue", outlier.colour=NA) + stat_summary(fun.y="mean", geom="point", shape=21, size=2.5, fill="white")

ggplot(restdata, aes(x=factor(DofW), y=-Sales)) + geom_violin(trim=FALSE, scale="count", fill="blue") + geom_boxplot(width=.1, fill="lightblue", outlier.colour=NA) + stat_summary(fun.y="mean", geom="point", shape=21, size=2.5, fill="white")

## Histogram of sales by day of week with density plot overlay
ggplot(salesdata, aes(x=-Sales, y=..density..)) + geom_histogram(fill="lightblue", colour="black", binwidth=200) + facet_grid(DofW ~ .) + geom_density(colour="red")

ggplot(restdata, aes(x=-Sales, y=..density..)) + geom_histogram(fill="lightblue", colour="black", binwidth=200) + facet_grid(DofW ~ .) + geom_density(colour="red")

vline.data <- data.frame(lower = dayMeans - daySds, upper = dayMeans + daySds)
vline.data$DofW <- rownames(vline.data)

restvline.data <- data.frame(lower = restdayMeans - restdaySds, upper = restdayMeans + restdaySds)
restvline.data$DofW <- rownames(restvline.data)

## Histogram, density and 1 st.dev. vertical lines
ggplot(salesdata, aes(x=-Sales, y=..density..)) + geom_histogram(fill="lightblue", colour="black", binwidth=200) + facet_grid(DofW ~ .) + geom_density(colour="red") + geom_vline(data=vline.data, aes(xintercept=lower)) + geom_vline(data=vline.data, aes(xintercept=upper)) + xlim(0, max(-(salesdata$Sales)))

ggplot(restdata, aes(x=-Sales, y=..density..)) + geom_histogram(fill="lightblue", colour="black", binwidth=200) + facet_grid(DofW ~ .) + geom_density(colour="red") + geom_vline(data=restvline.data, aes(xintercept=lower)) + geom_vline(data=restvline.data, aes(xintercept=upper)) + xlim(0, max(-(restdata$Sales)))


## Line graph of daily sales over time
ggplot(aes(x=Date, y=-Sales), data = restdata) + geom_line()


library(plyr)
## Overall means by season
mean(-restdata$Sales)
mean(-winterdata$Sales)
mean(-springdata$Sales)
mean(-summerdata$Sales)
mean(-falldata$Sales)

## Summary statistics for each season & overall
summary(restdata)
summary(winterdata)
summary(springdata)
summary(summerdata)
summary(falldata)

## Means by day of week for each season
winterdayMeans
springdayMeans
summerdayMeans
falldayMeans
restdayMeans

meansales = mean(-restdata$Sales)

## Deseasoning factors, by day
winterfactor = winterdayMeans / mean(-restdata$Sales)
springfactor = springdayMeans / mean(-restdata$Sales)
summerfactor = summerdayMeans / mean(-restdata$Sales)
fallfactor = falldayMeans / mean(-restdata$Sales)
winterfactor
springfactor
summerfactor
fallfactor



## USING DATA.TABLE INSTEAD OF DATAFRAME
library(data.table)

DT <- data.table(restdata)

DT[, Season := ifelse(Month %in% c("01","02","03"), "Winter",
               ifelse(Month %in% c("04","05"), "Spring",
               ifelse(Month %in% c("06","07","08"), "Summer",
               ifelse(Month %in% c("09","10","11","12"), "Fall", NA))))]

DT <- DT[, Season := as.factor(Season)]




DT[,Test] <- mapply(normalize, DT[,Season])

testfunction <- function(df, meansales = meansales){
  if (df$DofW == 'Mon') { ndx = 1 }
  else if (df$DofW == 'Tue') { ndx = 2 }
  else if (df$DofW == 'Wed') { ndx = 3 }
  else if (df$DofW == 'Thu') { ndx = 4 }
  else if (df$DofW == 'Fri') { ndx = 5 }
  else if (df$DofW == 'Sat') { ndx = 6 }
  else { ndx = 7 }
  
  result = (df['Sales'] / (restdayMeans[[ndx]] / meansales))
  }
