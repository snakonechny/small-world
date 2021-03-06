---
title: "Small World"
author: "SN"
date: "December 30, 2015"
output: html_document
---

##Prelude

I took a trip to Ukraine recently. All the usual pleasures of transatlantic travel aside (delays, more delays, and lost luggage), I noticed something that seemed quite interesting at my  ["home" airport](https://en.wikipedia.org/wiki/Lviv_Danylo_Halytskyi_International_Airport). LWO seems like a relatively insignificant regional gateway in western Ukraine; yet, several major *global* airlines come to Lviv with varying degree of regularity and thus drive most of the traffic through this airport. On Lufthansa (my preferred method of getting home), I can hop over to Munich every day; Austrian will take me to Vienna, Turkish - to Istanbul (painfully long connections to North America, unfortunately), and, finally, LOT (might) get me to Warsaw and then to Chicago with (hopefully) minimal delays and layovers.

Lviv is thus taking full advantage of the [hub-and-spook paradigm](https://en.wikipedia.org/wiki/Spoke%E2%80%93hub_distribution_paradigm) of modern aviation. One layover can get a willing paying passenger virtually anywhere in the world.

So, first, if money isn't a factor, where in the world can I go from Lviv? Put differently, how small is the world for someone who lives here?

##Data

We'll use the [OpenFlights dataset](http://openflights.org/data.html) to tackle this and a few other questions. This data source isn't ideal, but it should do the job: I couldn't find any evidence of the routes database update after 2014, so we will be missing out on some aggressive expansion of Turkish Airlines, for instance.

Second, there are actually two separate files for the routes and for the airports. At some point, we will need to combine the two, but more about that later.

##Analysis

First, import the routes data set. Conviniently, the columns aren't named - we'll fix this also.

```{r}
data <- read.csv("routes.csv", header = F)
colnames(data) <- c("airline", "airline.id", "origin", "origin.id", "dest", "dest.id", "codeshare", "stops", "type")
```

Just to validate, let's see how many places we can get to directly from Lviv. This number should be quite low.

```{r message=FALSE}

library(plyr)
library(dplyr)

data %>% group_by(origin) %>% filter(origin == "LWO") %>% summarise(outbound.count = n())
```

Even 17 destinations seems a bit inflated; during summer navigation, yes, with charter flights, maybe. After a closer look (I simply listed the destinations), indeed, the outdated dataset captures the [canned flights to Russia](http://www.bbc.com/news/world-europe-34920207) and the destinations operated by Wizzair, a lowcoster that pulled out of LWO last year. Let's clean this up a bit.

We can actually get rid of all Vnukovo and Wizzair-operated flights. A hypothetical passenger can't even use these travel options out of Lviv.

```{r}
data <- data %>% filter(airline != 'WU', dest != 'VKO')
```

We can trim this data set even further by including only flights where a connection is *logically and logistically* possible. Yes, there is a flight to Laranca, but Ukraine International Airlines probably won't be able to transfer you on to another destination there. I doubt that will make too much difference, so we'll leave the data as are.

Now we ought to think about the question a bit further and operationalize it, finally. A passenger can fly from Lviv to another airport and, in theory, can transfer to another flight operated out of that airport. The same could obviously happen at this second destination airport, and so on, until the final destination is reached.

How many destinations, then, are available to our hypothetical PAX with one transfer?

```{r}
#create a vector of unique destinations out of Lviv
dest.nostop <- data %>% group_by(origin) %>% filter(origin == "LWO")
unique.dest <- data %>% filter(origin == 'LWO') %>% distinct(dest) %>% select(dest) %>% as.matrix() %>% c()

#append LWO to this list of airports
unique.dest <- append(unique.dest, 'LWO')

#now subset all the routes that originate from either LWO or any of the airports to which a connection is possible
dest.onestop <- data %>% filter(origin %in% unique.dest)

```

By the looks of it there are nearly 1500 flights available to a passenger who is willing to transfer only once when flying from Lviv. How many unique airports (not necessarily cities) is that?

```{r warning=FALSE}
dest.onestop %>% group_by(dest) %>% distinct(dest)
```

There are nearly 360 airports our hypothetical passenger can go to with only one transfer. What if they're willing to change planes twice?

```{r warning=FALSE}
#now let's expand our list of unique destinations by including flights originating from our previous destinations (termini of a single layover)

unique.two.layovers <- dest.onestop %>% distinct(dest) %>% select(dest) %>% as.matrix() %>% c()

dest.twostops <- data %>% filter(origin %in% unique.two.layovers)

dest.twostops %>% group_by(dest) %>% distinct(dest)

```

The number of unique airports one can reach with two layovers grows to some 1,850. I'd imagine the growth is exponential here. 

Let's visualize this small world from Lviv. I added the code for processing/arranging the data below (commented, explained), but for now, let's look at the result.


```{r warning=FALSE, message=FALSE}
library(geosphere) #will plot the actual geo circles data, not absolute point-to-point distances

#we'll need these for graphing
#devtools::install_github("hrbrmstr/ggalt")
library(ggplot2)
library(ggalt)
library(ggthemes)

#as mentioned above, let's match destination with airport data
airports <- read.csv("airports.csv", header = F)
colnames(airports) <- c('airport.id', 'name', 'city', 'country', 'iata', 'icao', 'lat', 'long', 'alt', 'tmz', 'dst', 'tzd')
airports$airport.id <- as.factor(airports$airport.id)
airports.matching <- airports %>% select(5, 7, 8)

#match origin lat/long
colnames(airports.matching) <- c('origin', 'lat', 'long')
dest.nostop <- left_join(dest.nostop, airports.matching, by = 'origin')
colnames(dest.nostop)[10:11] <- c('origin.lat', 'origin.long')

dest.onestop <- left_join(dest.onestop, airports.matching, by = 'origin')
colnames(dest.onestop)[10:11] <- c('origin.lat', 'origin.long')

dest.twostops <- left_join(dest.twostops, airports.matching, by = 'origin')
colnames(dest.twostops)[10:11] <- c('origin.lat', 'origin.long')

#now match destination lat/long
colnames(airports.matching) <- c('dest', 'lat', 'long')
dest.nostop <- left_join(dest.nostop, airports.matching, by = 'dest')
dest.nostop$type <- 'nonstop'
colnames(dest.nostop)[12:13] <- c('dest.lat', 'dest.long')

dest.onestop <- left_join(dest.onestop, airports.matching, by = 'dest')
dest.onestop$type <- 'onestop'
colnames(dest.onestop)[12:13] <- c('dest.lat', 'dest.long')

dest.twostops <- left_join(dest.twostops, airports.matching, by = 'dest')
dest.twostops$type <- 'twostops'
colnames(dest.twostops)[12:13] <- c('dest.lat', 'dest.long')


#make sure there are no missing coordinates
dest.nostop <- dest.nostop[!with(dest.nostop, is.na(dest.lat) | is.na(dest.long) | is.na(origin.lat) | is.na(origin.long)),]
dest.onestop <- dest.onestop[!with(dest.onestop, is.na(dest.lat) | is.na(dest.long) | is.na(origin.lat) | is.na(origin.long)),]


#We'll consolidate these pairs of coordinates into line routes that will later be used to plot

nostop <- gcIntermediate(dest.nostop[, c('origin.long', 'origin.lat')], 
                          dest.nostop[,c('dest.long', 'dest.lat')], 
                          breakAtDateLine=FALSE, addStartEnd=TRUE, sp=TRUE)


onestop <- gcIntermediate(dest.onestop[, c('origin.long', 'origin.lat')], 
                                             dest.onestop[,c('dest.long', 'dest.lat')], 
                                             breakAtDateLine=FALSE, addStartEnd=TRUE, sp=TRUE)


#need to define the fortify function - found here https://github.com/hadley/ggplot2/blob/master/R/fortify-spatial.r
#this function is needed to convert the product of gcIntermediate into something ggplot understands

fortify.SpatialLinesDataFrame <- function(model, data, ...) {
  plyr::ldply(model@lines, fortify)
}

routes.nostop <- fortify.SpatialLinesDataFrame(nostop)
routes.onestop <- fortify.SpatialLinesDataFrame(onestop)


#now follow the approach here outlined here http://web.stanford.edu/~cengel/cgi-bin/anthrospace/great-circles-on-a-recentered-worldmap-in-ggplot

#Finally, create a world map
map.world <- map_data ("world")
map.world <- map.world %>% filter(region != 'Antarctica')

#and plot
map.onestop <- ggplot() + geom_map(data=map.world, map=map.world,
                 aes(x=long, y=lat, map_id=region),
                 color="white", fill="#7f7f7f", size=0.05, alpha=1/4) +
                 geom_line(aes(long,lat,group=group), color = "blue", size=0.5, data= routes.nostop)+                  geom_line(aes(long,lat,group=group), size=0.2, alpha = .5, color = "orange", data= routes.onestop) + 
                labs(title = 'Direct and one-stop routes out of Lviv International') +
                scale_color_tableau() +
                theme_map() +
                theme(strip.background=element_blank()) +
                theme(legend.position="none")

plot(map.onestop)
```

While this visualization poses questionable analytical value (there are too many routes over Europe to truly figure out what's happening), it drives a more general point quite clearly: all sorts of exciting destinations open up as soon as our hypothetical passenger from Lviv makes a stop somewhere. Thanks mostly to Lufthansa and Turkish Airlines, every continent except for Australia is accessible.

Now let's try to provide a more adequate visual answer to our original question. We will use the same canvas to plot all airports accessible directly, with one, two, or three layovers.


```{r warning=FALSE, message=FALSE}
#First, we'll generate lists of unique airports that fit each criteria.
#recycling the wranglings we used earlier

unique.dest <- data %>% filter(origin == 'LWO') %>% distinct(dest) %>% select(dest) %>% as.matrix()

unique.onestop <- data %>% filter(origin %in% unique.dest) %>% group_by(dest) %>% distinct(dest) %>% select(dest) %>% as.matrix()

unique.twostops <- data %>% filter (origin %in% unique.onestop) %>% group_by(dest) %>% distinct(dest) %>% select(dest) %>% as.matrix()

#we will now identify each row as direct/one/two stopover airport

unique.dest <- unique.dest %>% as.data.frame() %>% mutate(type = 'direct')
unique.onestop <- unique.onestop %>% as.data.frame() %>% mutate(type = 'one layover')
unique.twostops <- unique.twostops %>% as.data.frame() %>% mutate(type = 'two layovers')

#combine all three and pick out the first occurrence of each airport and its 'type'
master.destinations <- bind_rows(unique.dest, unique.onestop, unique.twostops) %>% distinct(dest)

#let's match all of this goodness with geographical coordinates once more
master.destinations <- left_join(master.destinations, airports.matching)

#some 30 airports don't have matching coordinates somehow :( Unfortunately this requires manual fixing

missing.dest <- master.destinations %>% filter(is.na(lat)==TRUE) %>% select(dest)

lat <- c('42.0175', '36.3075', '47.243378', '17.534443','33.717804','33.5354', '36.212223', '29.016666', '-0.286845', '46.746986', '27.589672', '26.26005', '30.766115', '25.452477', '41.103523', '37.865623', '27.369392', '31.087694', '22.639444', '44.904232', '43.426674', '22.733334', '34.135277', '-8.916634', '5.156111', '-0.309444', '25.902504', '20.498444', '40.735027', '53.25036', '6.203055', '4.876463', '44.40972', '26.708332')

long <- c('35.06889', '43.149445', '38.864017', '-91.98893', '1.094105', '-0.242177', '1.331667', '-10.066667', '20.883972', '125.13399', '106.999825', '105.87281', '117.73568', '107.961914', '123.857834', '68.86513', '53.195354', '61.542244', '113.81084', '82.024185', '112.09845', '-12.35', '132.235550', '33.45886', '73.13028', '73.43278', '54.546616', '45.20445', '30.083336', '-4.529017', '6.667222', '8.085693', '18.709167', '85.92389')

missing.dest <- data.frame(missing.dest, lat, long)

#yes, I know this isn't ideal, but I figured this would be a quick and dirty way to complete the missing coordinates
for (i in 1:nrow(master.destinations)) {
  for (j in 1:nrow(missing.dest)) {
    if (master.destinations$dest[i] == missing.dest$dest[j]) {
      master.destinations$lat[i] <- missing.dest$lat[j]
      master.destinations$long[i] <- missing.dest$long[j]
    }
  }
}

#Now, finally, off to graph

map.destinations <- ggplot() +
  geom_map(data=map.world, map=map.world, aes(x=long, y=lat, map_id=region), color="white", fill="#7f7f7f", size=0.05, alpha=1/4) +
  geom_point(data = master.destinations, aes(x=long, y=lat, color = type), size = .2) +
                labs(title = "Destinations available directly, after one or two layovers, out of Lviv International") +
                theme_map() +
                theme(strip.background=element_blank())

plot(map.destinations)
```

Long story short, the world does indeed seems relatively small from Lviv. With two layovers, I can reach practically all major North American cities and make it to the most remote (albeit still somewhat *major*) airports of Africa or Asia.

I reckon Lviv is in a somewhat unique position here. Two truly global airlines, Lufthansa and Turkish, fly here regularly, and a single stop in either Munich or Istanbul expands the list of destinations incrementally. Thus surfaces my second question: how common is it for a regional airport to have this much access to destinations around the world?

##Analysis 1.1

I think answering this question demands rethinking our approach a bit. First, our units of analysis (forgive my social science lingo) have grown from one (Lviv) to any other airport like Lviv, on an aggregate. Second, we will be comparing these "other airports" to each other and to Lviv.

My original idea was to employ [igraph](http://igraph.org/redirect.html) and analyze explicit networks of flights instead of some abstract connections between X and Y. However, I decided to stick to the dplyr ways; the process seemed a bit more straightforward and the product of analysis - similar to what I've already shown.

We will use the same dataset and start from the very beginning.

```{r}

data.network <- read.csv('routes.csv', header = FALSE)
colnames(data.network) <- c("airline", "airline.id", "origin", "origin.id", "dest", "dest.id", "codeshare", "stops", "type")

```

First, what constitutes an airport that is "like" Lviv International? While the number of passengers served would be a logical attribute, I think the number of outbound flights is more interesting: one could imagine a situation where there is a serious amount of PAX traffic flowing out of an airport but hardly any flights (a high-capacity, low-frequency route). That, and we don't have passenger data in this dataset :)

So let's figure out how many flights originate out of every airport in this dataset.

```{r}
outbound.flights <- data.network %>% group_by(origin) %>% summarize(outbound.count = n()) %>% arrange(outbound.count)
```

Just for curiosity's sake, let's draw up a quick histogram/frequency distribution outbound flights per airport across the globe:

```{r}
flights.hist <- ggplot(outbound.flights, aes(x=outbound.count)) + 
                geom_histogram(binwidth = 5) +
                theme_bw() +
                labs(title = 'Distribution of outbound flights, globally, bin size = 5')

plot(flights.hist)
```

Somewhat unexpectedly, vast majority of airports hardly have any flights originating from them. Also observe the enormous tail - somewhere to the right of the graph are the O'Hares, Hartsfields, Dubais, and Heathrows of this world.

Recall that Lviv has around eight non-stop destinations. *Let's define "similar to Lviv" an airport that has between six and ten non-stop outbounds*.

```{r}
#we will compute the list of airports similar to LWO
like.lwo <- outbound.flights %>% filter(outbound.count <= 10 & outbound.count >= 6) %>% select(origin) %>% as.data.frame() %>% c()
```

There are 443 airports that fit these criteria. We will now try to analyze the secondary and tertiary connections out of these airports.

```{r warning=FALSE, message=FALSE}
#back to the original goal, create an empty data frame to fill with airport stats
like.lwostats <- data.frame(origin = character(0), direct = numeric(0), one.stop = numeric(0), two.stops = numeric(0))

#now loop through each element in the list of airports that fit the critera and determine the number of each connection type       
for (i in like.lwo$origin) {
  
  primary <- data.network %>% filter(origin == i) %>% distinct(dest) %>% select(dest) %>% as.data.frame()
  secondary <- data.network %>% filter(origin %in% primary$dest) %>% distinct(dest) %>% select(dest) %>% as.data.frame()
  tertiary <- data.network %>% filter(origin %in% secondary$dest) %>% distinct(dest) %>% select(dest) %>% as.data.frame()
  
  like.lwostats <- bind_rows(like.lwostats, data.frame(origin = i, direct = nrow(primary), one.stop = nrow(secondary), two.stops = nrow(tertiary)))
  
}
```

Let's take a step back: out of Lviv International, one can get to eight airports directly, 357 after one transfer, and 1,859 after two. How does this compare to other similar starting points?

```{r}
summary(like.lwostats)
```

Indeed, it does appear significantly more "connected" than other airports with similar number of direct outbounds. The mean/median number of destinations with one layover is around 172, compared to Lviv's 357. That puts it well clear of the 3rd quartile of all similar airports. Same holds true for destinations available after two layovers.

##Why does this all matter and what's next?

For one, I'm a bit astonished by how easy it is to get from a remote South American strip to an equally remote Central Asian airfield, let alone to a major international transfer point. Yes, Lviv is relatively centrally located, hence, probably, its attractiveness to several major airlines; this obviously gives it serious adventage over the said regional airports on other continents, but still, the degree of connectivity of this city to other places around the world is astonishing.

More importantly, I'd like to one day take this analysis a bit further. It would be interesting, for instance, to study the impact of distance to a major transfer point (think Frankfurt, Dubai, Singapore, etc.) on accessibility to other destinations. Or I'd like to study how competition or presence of different airlines impact this accessibility. Lviv is catered by four members of Star Alliance that compete for transit traffic (this is the reason why I'm dreadfully committed to United Airlines in the US). Is this at all common? I don't know, but I'd love to find out.

There's a theme here, obviously: outbound traffic could be an interesting guage for how major international carriers try to achieve competitive advantage or strategically open/maintain/shut down destinations.
