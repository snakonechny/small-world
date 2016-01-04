---
title: "Small World"
author: "SN"
date: "December 30, 2015"
output: html_document
---

I took a trip to Ukraine recently. All the usual pleasures of transatlantic travel aside (delays, more delays, and lost luggage), I noticed something that seemed quite interesting at my  ["home" airport](https://en.wikipedia.org/wiki/Lviv_Danylo_Halytskyi_International_Airport). LWO seems like a relatively insignificant regional gateway in western Ukraine; yet, several major *global* airlines come to Lviv with varying degree of regularity and thus drive most of the traffic through this airport. On Lufthansa (my preferred method of getting home), I can hop over to Munich every day; Austrian will take me to Vienna, Turkish - to Istanbul (painfully long connections to North America, unfortunately), and, finally, LOT (might) get me to Warsaw and then to Chicago with (hopefully) minimal delays and layovers.

Lviv is thus taking full advantage of the [hub-and-spook paradigm](https://en.wikipedia.org/wiki/Spoke%E2%80%93hub_distribution_paradigm) of modern aviation. One layover can get a willing paying passenger virtually anywhere in the world.

So, first, if money isn't a factor, where in the world can I go from Lviv? Put differently, how small is the world for someone who lives here?

We'll the [OpenFlights dataset](http://openflights.org/data.html) to tackle this and a few other questions. This data source isn't ideal, but it should do the job: I couldn't find any evidence of the routes database update after 2014, so we will be missing out on some aggressive expansion of Turkish Airlines, for instance.

Second, there are actually two separate files for the routes and for the airports. At some point, we will need to combine the two, but more about that later.

First, import the routes data set. Conviniently, the columns aren't named - we'll fix this also.


```r
data <- read.csv("routes.csv", header = F)
colnames(data) <- c("airline", "airline.id", "origin", "origin.id", "dest", "dest.id", "codeshare", "stops", "type")
```

Just validate, let's see how many places we can get to directly from Lviv. This number should be quite low.


```r
library(dplyr)
```

```r
data %>% group_by(origin) %>% filter(origin == "LWO") %>% summarise(outbound.count = n())
```

```
## Error in n(): This function should not be called directly
```

Even 17 destinations seems a bit inflated; during summer navigation, yes, with charter flights, maybe. After a closer look (I simply listed the destinations), indeed, the outdated dataset captures the [canned flights to Russia](http://www.bbc.com/news/world-europe-34920207) and the destinations operated by Wizzair, a lowcoster that pulled out of LWO last year. Let's clean this up a bit.

We can actually get rid of all Vnukovo and Wizzair-operated flights. A hypothetical passenger can't even use these travel options out of Lviv.


```r
data <- data %>% filter(airline != 'WU', dest != 'VKO')
```

We can trim this data set even further by including only flights were a connection is *logically and logistically* possible. Yes, there is a flight to Laranca, but Ukraine International Airlines probably won't be able to transfer you on to another destination there. I doubt that will make too much difference, so we'll leave the data as are.

Now we ought to think about the question a bit further and operationalize it, finally. A passenger can fly from Lviv to another airport and, in theory, can transfer to another flight operated out of that airport. The same could obviously happen at this second destination airport, and so on, until the final destination is reached.

How many destinations, then, are available to our hypothetical PAX with one transfer?


```r
#create a vector of unique destinations out of Lviv
dest.nostop <- data %>% group_by(origin) %>% filter(origin == "LWO")
unique.dest <- data %>% filter(origin == 'LWO') %>% distinct(dest) %>% select(dest) %>% as.matrix() %>% c()

#append LWO to this list of airports
unique.dest <- append(unique.dest, 'LWO')

#now subset all the routes that originate from either LWO or any of the airports to which a connection is possible
dest.onestop <- data %>% filter(origin %in% unique.dest)
```

By the looks of it there are nearly 1500 flights available to a passenger who is willing to transfer only once when flying from Lviv. How many unique airports (not necessarily cities) is that?


```r
dest.onestop %>% group_by(dest) %>% distinct(dest)
```

```
## Source: local data frame [357 x 9]
## Groups: dest [357]
## 
##    airline airline.id origin origin.id   dest dest.id codeshare stops
##     (fctr)     (fctr) (fctr)    (fctr) (fctr)  (fctr)    (fctr) (int)
## 1       2L       2750    LCA      1197    ZRH    1678               0
## 2       3L       2916    MUC       346    EBA    1558               0
## 3       3O       9818    SAW      4317    CMN    1074               0
## 4       4U       2548    MUC       346    DTM     373               0
## 5       4U       2548    SAW      4317    CGN     344               0
## 6       4U       2548    SAW      4317    HAJ     352               0
## 7       4U       2548    SAW      4317    HAM     342               0
## 8       4U       2548    SAW      4317    STR     350               0
## 9       4U       2548    SAW      4317    TXL     351               0
## 10      4U       2548    WAW       679    DUS     345               0
## ..     ...        ...    ...       ...    ...     ...       ...   ...
## Variables not shown: type (fctr)
```

There are nearly 360 airports our hypothetical passenger can go to with only one transfer. What if they're willing to change planes twice?


```r
#now let's expand our list of unique destinations by including flights originating from our previous destinations (termini of a single layover)

unique.two.layovers <- dest.onestop %>% distinct(dest) %>% select(dest) %>% as.matrix() %>% c()

dest.twostops <- data %>% filter(origin %in% unique.two.layovers)

dest.twostops %>% group_by(dest) %>% distinct(dest)
```

```
## Source: local data frame [1,859 x 9]
## Groups: dest [1859]
## 
##    airline airline.id origin origin.id   dest dest.id codeshare stops
##     (fctr)     (fctr) (fctr)    (fctr) (fctr)  (fctr)    (fctr) (int)
## 1       2B        410    AER      2965    KZN    2990               0
## 2       2B        410    ASF      2966    MRV    2962               0
## 3       2B        410    DME      4029    NBC    6969               0
## 4       2B        410    DME      4029    TGK     \\N               0
## 5       2B        410    DME      4029    UUA    6160               0
## 6       2B        410    KGD      2952    EGO    6156               0
## 7       2B        410    KZN      2990    AER    2965               0
## 8       2B        410    KZN      2990    ASF    2966               0
## 9       2B        410    KZN      2990    CEK    2968               0
## 10      2B        410    KZN      2990    DME    4029               0
## ..     ...        ...    ...       ...    ...     ...       ...   ...
## Variables not shown: type (fctr)
```

The number of unique airports one can reach with two layovers grows to some 1,850. I'd imagine the growth is exponential here. 

Let's visualize this small world from Lviv. I added the code for processing/arranging the data below (commented, explained), but for now, let's look at the result.



```r
#make sure there are no missing coordinates
dest.nostop <- dest.nostop[!with(dest.nostop, is.na(dest.lat) | is.na(dest.long) | is.na(origin.lat) | is.na(origin.long)),]
```

```
## Error in eval(expr, envir, enclos): object 'dest.lat' not found
```

```r
dest.onestop <- dest.onestop[!with(dest.onestop, is.na(dest.lat) | is.na(dest.long) | is.na(origin.lat) | is.na(origin.long)),]
```

```
## Error in eval(expr, envir, enclos): object 'dest.lat' not found
```

```r
#We'll consolidate these pairs of coordinates into line routes that will later be used to plot

nostop <- gcIntermediate(dest.nostop[, c('origin.long', 'origin.lat')], 
                          dest.nostop[,c('dest.long', 'dest.lat')], 
                          breakAtDateLine=FALSE, addStartEnd=TRUE, sp=TRUE)
```

```
## Error in .check_names_df(x, j): undefined columns: origin.long, origin.lat
```

```r
onestop <- gcIntermediate(dest.onestop[, c('origin.long', 'origin.lat')], 
                                             dest.onestop[,c('dest.long', 'dest.lat')], 
                                             breakAtDateLine=FALSE, addStartEnd=TRUE, sp=TRUE)
```

```
## Error in `[.data.frame`(dest.onestop, , c("origin.long", "origin.lat")): undefined columns selected
```

```r
#need to define the fortify function - found here https://github.com/hadley/ggplot2/blob/master/R/fortify-spatial.r
fortify.SpatialLinesDataFrame <- function(model, data, ...) {
  plyr::ldply(model@lines, fortify)
}

routes.nostop <- fortify.SpatialLinesDataFrame(nostop)
routes.onestop <- fortify.SpatialLinesDataFrame(onestop)


#now follow the approach here outlined here http://web.stanford.edu/~cengel/cgi-bin/anthrospace/great-circles-on-a-recentered-worldmap-in-ggplot

#shift the coordinates to recenter

gg <- ggplot() + geom_map(data=map.world, map=map.world,
                          aes(x=long, y=lat, map_id=region),
                          color="white", fill="#7f7f7f", size=0.05, alpha=1/4) +
  geom_line(aes(long,lat,group=group), color = "blue", size=0.5, data= routes.nostop) + geom_line(aes(long,lat,group=group), size=0.2, alpha = .5, color = "orange", data= routes.onestop)
  
gg <- gg + scale_color_tableau()
gg <- gg + theme_map()
gg <- gg + theme(strip.background=element_blank())
gg <- gg + theme(legend.position="none")
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
