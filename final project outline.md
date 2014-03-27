Final Project Outline  Junzhao Hu
========================================================
Topic Name: How to avoid incovenience in taking flight.

Below is the link for the data, I downloaded data for October, November and December of 2013.

I am interested in several things:

1 Is there a day of the week/ time of day effect on departure or arrival delays
2 Which city(Origin and Destination) cancels or delays the most.
3 Which Carrier delays or cancels most.
4 What kind of factors affect the delay time of the flight(like distance,depart time, arrive time)
5 Which company(UniqueCarrier) delays or cancels most.


**Links:**
* [http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time]



Fetch 2014 Olympic Medal Counts

```r
url <- "http://www.sochi2014.com/en/medal-standings"
tables <- readHTMLTable(url)
```

```
## Error: could not find function "readHTMLTable"
```

```r
medal.counts = tables[[1]]
```

```
## Error: object 'tables' not found
```

```r
medal.counts[, 3] <- as.numeric(medal.counts[, 3])  # Gold
```

```
## Error: object 'medal.counts' not found
```

```r
medal.counts[, 4] <- as.numeric(medal.counts[, 4])  # Silver
```

```
## Error: object 'medal.counts' not found
```

```r
medal.counts[, 5] <- as.numeric(medal.counts[, 5])  # Bronze
```

```
## Error: object 'medal.counts' not found
```

```r
medal.counts[, 6] <- as.numeric(medal.counts[, 6])  # Total
```

```
## Error: object 'medal.counts' not found
```

Create a Scatterplot matrix of the Gold, Silver, and Bronze counts

```r
ggpairs(data = medal.counts, columns = 3:5)
```

```
## Error: could not find function "ggpairs"
```


In the scatterplot matrix above, all three plots show a general positive linear trend: the more medals of one kind a country has, the more medals of another kind a country is likely to have.  This probably due to the fact that countries that send the most athletes will win more medals of all kinds by sheer volume! The strongest relationship is between gold and silver, with the weakest relationship between gold and bronze.

Exercise 2: Number of Medals by Country Across All Years
-----------
* Now look at the historical medals, but you should follow links in order to get to the history data
* Pull out the total number of medals for all of the years in the historical records.  Using these type of commands will help:
  * getNodeSet(root, "//option[@value]")
  * ldplyr(yearsdetails,function(x) xmlAttrs(x)["value"])
* Make a parallel coordinate plot showing the medal tallies by year.
* Write a paragraph summarizing what you learn about historical medal tallies.


```r
# === Get Historical Medals ===
url2010 <- "http://www.sochi2014.com/en/medal-history?year=2010"
doc <- htmlParse(url2010)
```

```
## Error: could not find function "htmlParse"
```

```r
opt_nodes <- getNodeSet(doc, "//option[@value >1920]")  # <option value='2010' > Vancouver 2010</option>
```

```
## Error: could not find function "getNodeSet"
```

```r
years <- ldply(opt_nodes, function(x) xmlAttrs(x)["value"])
```

```
## Error: could not find function "ldply"
```

```r
URLS <- paste("http://www.sochi2014.com/en/medal-history?year=", years$value, 
    sep = "")
```

```
## Error: object 'years' not found
```

```r

medal.df <- function(URL) {
    medalcount <- readHTMLTable(URL, stringsAsFactors = F)[[1]]
    n <- nrow(medalcount)
    year <- strsplit(URL, "year=")[[1]][2]
    yearcol <- rep(year, n)
    return(cbind(Year = yearcol, medalcount))
}

tables.all <- ldply(URLS, function(x) medal.df(x))
```

```
## Error: could not find function "ldply"
```

```r
tables.all$Year <- as.numeric(as.character(tables.all$Year))
```

```
## Error: object 'tables.all' not found
```

```r
tables.all[, 7] <- as.numeric(tables.all[, 7])  # Total
```

```
## Error: object 'tables.all' not found
```

```r
tables.all[, 6] <- as.numeric(tables.all[, 6])  # Bronze
```

```
## Error: object 'tables.all' not found
```

```r
tables.all[, 5] <- as.numeric(tables.all[, 5])  # Silver
```

```
## Error: object 'tables.all' not found
```

```r
tables.all[, 4] <- as.numeric(tables.all[, 4])  # Gold
```

```
## Error: object 'tables.all' not found
```

```r

medal.counts$Year <- 2014
```

```
## Error: object 'medal.counts' not found
```

```r
medal.counts <- medal.counts[, c(7, 1:6)]
```

```
## Error: object 'medal.counts' not found
```

```r

all.medals <- rbind(medal.counts, tables.all)
```

```
## Error: object 'medal.counts' not found
```

Parallel coordinate plot showing the medal tallies by year.

```r
qplot(data = all.medals, x = Year, y = Total, geom = "line", group = Country)
```

```
## Error: could not find function "qplot"
```


Parallel Coordinate Plot for Countries that win at least 3 medals for all years.

```r
big.winners <- group_by(all.medals, Country) %.% mutate(Min.medals = min(Total)) %.% 
    filter(Min.medals >= 3)
```

```
## Error: could not find function "%.%"
```

```r

qplot(data = big.winners, x = Year, y = Total, geom = "line", group = Country, 
    color = Country)
```

```
## Error: could not find function "qplot"
```

The historical medal tallies show first that the number of countries participating in the winter Olympics (and winning medals) has increased over the years.  There are also some countries that are only in a few Olympics in the middle of the 20th century.  When looking at the second graph, we see that they are the USSR and the GDR, which stands for German Democratic Republic, also known as East Germany. Both of these countries only existed until about 1990, which is why they stop winning medals! Also, it appears that these communist countries were exceptionally good at winning medals in the years they participated 

<!-- Commentted out anything after this ================
Samantha Tyner
//```{r fig.width=7, fig.height=6}
# place code here
library(ggplot2)
library(XML)
# === Fetch 2014 Olympic Medal Counts ===
url2014<-"http://www.sochi2014.com/en/medal-standings"
tables<-readHTMLTable(url2014,stringsAsFactors=F)
class(tables) # returns a list
medal.counts=tables[[1]]
head(metal.counts)
medal.counts[,3]<-as.numeric(medal.counts[,3])
medal.counts[,4]<-as.numeric(medal.counts[,4])
medal.counts[,5]<-as.numeric(medal.counts[,5])
# === Scatterplot matrix of the Gold, Silver, and Bronze counts like example? ===
#install.packages('GGally')
library(GGally)
ggpairs(data=medal.counts,columns=3:5)
# will need to write a paragraph summarizing the association between metal counts
#stuff




# === Get Historical Medals ===
# hmm actually http://www.sochi2014.com/en/medal-history?year=2010 is this year
year<-seq(from=1924, to=2010, by=4) #drat, this doesn't work, because some of the years are missing on the website... Oh well.

url2010<-"http://www.sochi2014.com/en/medal-history?year=2010"
doc<-htmlParse(url2010)
root<-xmlRoot(doc)
length(xmlChildren(root))
xmlName(xmlChildren(root)[[2]])
length(xmlChildren(xmlChildren(root)[[2]]))

s<-getNodeSet(doc, "//option[@value >1920]")

# looked at the html file
# <select name="year" size=10>
# <option value="2010" > Vancouver 2010</option>

#http://www.sochi2014.com/en/medal-history?year=2006&sport=all

years<-ldply(s, function(x) xmlAttrs(x)["value"])

#strsplit('http://www.sochi2014.com/en/medal-history?year=2006','year=')[[1]][2]
URLS<-paste('http://www.sochi2014.com/en/medal-history?year=',years$value,sep='')

medal.df<-function(URL){
  medalcount<-readHTMLTable(URL,stringsAsFactors=F)[[1]]
  n<-nrow(medalcount)
  year<-strsplit(URL,'year=')[[1]][2]
  yearcol<-rep(year,n)
  return(cbind(Year=yearcol,medalcount))
}

tables.all<-ldply(URLS,function(x) medal.df(x))
tables.all$Year<-as.numeric(as.character(tables.all$Year))
tables.all[,7]<-as.numeric(tables.all[,7])
tables.all[,6]<-as.numeric(tables.all[,6])
tables.all[,5]<-as.numeric(tables.all[,5])
tables.all[,4]<-as.numeric(tables.all[,4])

medal.counts$Year<-2014
medal.counts$Total<-as.numeric(medal.counts$Total)
medal.counts<-medal.counts[,c(7,1:6)]

all.medals<-rbind(medal.counts, tables.all)

qplot(data=all.medals,x=Year,y=Total,geom='line',group=Country)

library(dplyr)
big.winners<-group_by(all.medals,Country)%.%
mutate(Min.medals=min(Total))%.%
  filter(Min.medals>=3)

qplot(data=big.winners,x=Year,y=Total,geom='line',group=Country,color=Country)
```

Junzhao Hu
//```{r fig.width=7, fig.height=6}
# place code here
x<-c(1:10)
```

 -->
