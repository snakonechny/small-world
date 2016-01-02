library(dplyr)
library(ggplot2)
library(geosphere) #will plot the actual geo circles data, not absolute point-to-point distances

data <- read.csv("routes.csv", header = F)
colnames(data) <- c("airline", "airline.id", "origin", "origin.id", "dest", "dest.id", "codeshare", "stops", "type")

data %>% group_by(origin) %>% filter(origin == "LWO") %>% summarise(outbound.count = n())

data <- data %>% filter(airline != 'WU', dest != 'VKO')

dest.nostop <- data %>% group_by(origin) %>% filter(origin == "LWO")

unique.dest <- data %>% filter(origin == 'LWO') %>% distinct(dest) %>% select(dest) %>% as.matrix() %>% c()

#append LWO to this list of airports
unique.dest <- append(unique.dest, 'LWO')

#now subset all the routes that originate from either LWO or any of the airports to which a connection is possible
dest.onestop <- data %>% filter(origin %in% unique.dest)

dest.onestop %>% group_by(dest) %>% distinct(dest)

unique.two.layovers <- dest.onestop %>% distinct(dest) %>% select(dest) %>% as.matrix() %>% c()

dest.twostops <- data %>% filter(origin %in% unique.two.layovers)

dest.twostops %>% group_by(dest) %>% distinct(dest)


#making the graph
devtools::install_github("hrbrmstr/ggalt")
library(ggalt)
library(ggthemes)

#first prepare the data
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

#Now let's combine everything into one data frame
#----------------------#

lviv.master <- bind_rows(dest.nostop, dest.onestop, dest.twostops)

#make sure there are no missing coordinates
lviv.master <- lviv.master[!with(lviv.master, is.na(dest.lat) | is.na(dest.long) | is.na(origin.lat) | is.na(origin.long)),]

#We'll consolidate these pairs of coordinates into line routes that will later be used to plot

for (i in 1:nrow(lviv.master)) {
  intermediate <- as.data.frame(gcIntermediate(c(lviv.master$origin.long[i], lviv.master$origin.lat[i]), 
                                        c(lviv.master$dest.long[i], lviv.master$dest.lat[i]), 
                                        n=100, addStartEnd=TRUE))
}
  
map.world <- map_data('world')
map.world <- map.world[map.world$region != "Antarctica",]

gg <- ggplot()
gg <- gg + geom_map(data=map.world, map=map.world,
                    aes(x=long, y=lat, map_id=region),
                    color="white", fill="#7f7f7f", size=0.05, alpha=1/4)

