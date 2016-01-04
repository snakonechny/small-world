data <- read.csv("routes.csv", header = F)
colnames(data) <- c("airline", "airline.id", "origin", "origin.id", "dest", "dest.id", "codeshare", "stops", "type")

library(plyr)
library(dplyr)

data %>% group_by(origin) %>% filter(origin == "LWO") %>% summarise(outbound.count = n())

data <- data %>% filter(airline != 'WU', dest != 'VKO')

#create a vector of unique destinations out of Lviv
dest.nostop <- data %>% group_by(origin) %>% filter(origin == "LWO")
unique.dest <- data %>% filter(origin == 'LWO') %>% distinct(dest) %>% select(dest) %>% as.matrix() %>% c()

#append LWO to this list of airports
unique.dest <- append(unique.dest, 'LWO')

#now subset all the routes that originate from either LWO or any of the airports to which a connection is possible
dest.onestop <- data %>% filter(origin %in% unique.dest)

dest.onestop %>% group_by(dest) %>% distinct(dest)

#now let's expand our list of unique destinations by including flights originating from our previous destinations (termini of a single layover)

unique.two.layovers <- dest.onestop %>% distinct(dest) %>% select(dest) %>% as.matrix() %>% c()

dest.twostops <- data %>% filter(origin %in% unique.two.layovers)

dest.twostops %>% group_by(dest) %>% distinct(dest)

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

                                                              ##Analysis 1.1

outbound.flights <- data.network %>% group_by(origin) %>% summarize(outbound.count = n()) %>% arrange(outbound.count)

#Just for curiosity's sake, let's draw up a quick histogram/frequency distribution outbound flights per airport across the globe:

flights.hist <- ggplot(outbound.flights, aes(x=outbound.count)) + 
geom_histogram(binwidth = 5) +
theme_bw() +
labs(title = 'Distribution of outbound flights, globally, bin size = 5')

plot(flights.hist)

#we will compute the list of airports similar to LWO
like.lwo <- outbound.flights %>% filter(outbound.count <= 10 & outbound.count >= 6) %>% select(origin) %>% as.data.frame() %>% c()

#back to the original goal, create an empty data frame to fill with airport stats
like.lwostats <- data.frame(origin = character(0), direct = numeric(0), one.stop = numeric(0), two.stops = numeric(0))

#now loop through each element in the list of airports that fit the critera and determine the number of each connection type       
for (i in like.lwo$origin) {

primary <- data.network %>% filter(origin == i) %>% distinct(dest) %>% select(dest) %>% as.data.frame()
secondary <- data.network %>% filter(origin %in% primary$dest) %>% distinct(dest) %>% select(dest) %>% as.data.frame()
tertiary <- data.network %>% filter(origin %in% secondary$dest) %>% distinct(dest) %>% select(dest) %>% as.data.frame()

like.lwostats <- bind_rows(like.lwostats, data.frame(origin = i, direct = nrow(primary), one.stop = nrow(secondary), two.stops = nrow(tertiary)))

}

summary(like.lwostats)
