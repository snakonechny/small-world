library(dplyr)

data <- read.csv("routes.csv", header = F)
colnames(data) <- c("airline", "airline.id", "origin", "origin.id", "dest", "dest.id", "codeshare", "stops", "type")

data %>% group_by(origin) %>% filter(origin == "LWO") %>% summarise(outbound.count = n())

data %>% group_by(origin) %>% filter(origin == "LWO")

data <- data %>% filter(airline != 'WU', origin != 'LWO')

unique.dest <- data %>% filter(origin == 'LWO') %>% distinct(dest) %>% select(dest) %>% as.matrix() %>% c()

#append LWO to this list of airports
unique.dest <- append(unique.dest, 'LWO')

#now subset all the routes that originate from either LWO or any of the airports to which a connection is possible
dest.onestop <- data %>% filter(origin %in% unique.dest)

dest.onestop %>% group_by(dest) %>% distinct(dest)

unique.two.layovers <- dest.onestop %>% distinct(dest) %>% select(dest) %>% as.matrix() %>% c()

dest.twostops <- data %>% filter(origin %in% unique.two.layovers)

dest.twostops %>% group_by(dest) %>% distinct(dest)
