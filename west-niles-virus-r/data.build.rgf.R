##############################################################
## trap data
##############################################################
rm(list = ls())
source("fn.base.R")


data.folder <- "../data/input/"
train.file <- "train.csv"
test.file <- "test.csv"
spray.file <- "spray.csv"
weather.file <- "weather.csv"
weather.station1 <- c(-87.933, 41.995)
weather.station2 <- c(-87.752, 41.786)

#################################################################
#################### TRAIN TEST DATA ############################
#################################################################

### read datasets ###
data.train <- fread(paste0(data.folder, train.file))
data.test <- fread(paste0(data.folder, test.file))

cols.tr.orig <- colnames(data.train)
data.tr.info <- unique(data.test[, list(Address, Block, Street, Trap,
                                        AddressNumberAndStreet, 
                                        Latitude, Longitude,
                                        AddressAccuracy)])
data.tr.aug <- data.table(expand.grid(Date = unique(data.train$Date),
                                      Address = unique(data.test$Address),
                                      Species = unique(data.test$Species)))
data.tr.aug <- merge(data.tr.aug, data.tr.info,
                     by=c("Address"))
data.tr.aug <- merge(data.tr.aug, 
                     data.train[, list(Date, Address, Species,
                                       NumMosquitos, WnvPresent)],
                     by=c("Date", "Address", "Species"),
                     all.x=T,
                     allow.cartesian=T)
data.tr.aug <- data.tr.aug[, cols.tr.orig, with=F][order(Date, Species)]
data.train <- data.tr.aug
data.train[, Id := -(.N:1)]

data.train.test.rgf <- rbindlist(list(data.train, data.test), use.names=T, fill=T) 
Store(data.train.test.rgf)

### combine train and test ###
data.all.rgf <- rbindlist(list(data.train, data.test), use.names=T, fill=T) 
data.all.rgf[, Id := NULL]
data.all.rgf <- data.all.rgf[Species %in% c("CULEX PIPIENS/RESTUANS", "CULEX PIPIENS", "CULEX RESTUANS")]

### correct wrong Trap ids ###
data.traps <- data.all.rgf[,list(Longitude_count = length(unique(Longitude)), 
                                 Latitude_count = length(unique(Latitude))),by="Trap"]
ix <- union(which(data.traps$Longitude_count>1), which(data.traps$Latitude_count>1))
traps.2correct <- data.traps[ix, Trap]
cat ("wrong traps", traps.2correct, "\n")

data.traps <- data.all.rgf[, list(TrapUniqueId = .GRP), by=c("Trap","Longitude","Latitude")]
data.all.rgf <- merge(data.all.rgf, data.traps, by=c("Trap", "Longitude", "Latitude"), all.x=TRUE)

### remove Address, Block, Street, AddressNumberAndStreet, AddressAccuracy
data.all.rgf[, Address := NULL]
data.all.rgf[, Block := NULL]
data.all.rgf[, Street := NULL]
data.all.rgf[, AddressNumberAndStreet := NULL]
data.all.rgf[, AddressAccuracy := NULL]

### remove duplicated traps ###
cols <- c("Trap","Longitude","Latitude","Date","Species","TrapUniqueId")
data.all.rgf <- data.all.rgf[,list(NumMosquitos = sum(NumMosquitos),
                                   NumBatches = .N,
                                   WnvPresent = sum(WnvPresent)),by=cols]

### get year, month, day, day_rank ###
dates <- strsplit(data.all.rgf$Date, "-")
data.all.rgf[, Year := as.numeric(sapply(dates, function(x) x[1]))]
data.all.rgf[, Month := as.numeric(sapply(dates, function(x) x[2]))]
data.all.rgf[, Day := as.numeric(sapply(dates, function(x) x[3]))]
data.day.rank <- data.all.rgf[, list(Year, Month, Day)]
setkey(data.day.rank, Year, Month, Day)
data.day.rank <- unique(data.day.rank)
data.day.rank <- data.day.rank[, list(Day,
                                      Month,
                                      DayRank = 31*(Month-5) + Day), by="Year"]
data.all.rgf <- merge(data.all.rgf, data.day.rank, by=c("Day","Month","Year"), all.x=TRUE)

### transform Species to dummy variables ###
data.species <- data.all.rgf[,list(Species)]
data.species[, Species := as.factor(Species)]
data.species <- model.matrix(~ . - 1, data.species)
colnames(data.species) <- gsub(" |/","",colnames(data.species))
col.species <- colnames(data.species)
Store(col.species)
data.all.rgf <- cbind(data.all.rgf, data.species)

### get coded Species ###
data.all.rgf[, SpeciesTree := 0]
data.all.rgf[Species == "CULEX RESTUANS", SpeciesTree := 1]
data.all.rgf[Species == "CULEX PIPIENS/RESTUANS", SpeciesTree := 2]
data.all.rgf[Species == "CULEX PIPIENS", SpeciesTree := 3]

### get coded Traps ###
data.traps <- data.all.rgf[, list(Trap)]
setkey(data.traps, Trap)
data.traps <- unique(data.traps)
data.traps[,TrapTree := c(1:nrow(data.traps))]
data.all.rgf <- merge(data.all.rgf, data.traps, by="Trap", all.x=T)

data.all.rgf[, WeekYear := as.numeric(format(as.Date(Date, format="%Y-%m-%d"), format = "%V"))]
data.all.rgf[, DayYear := as.numeric(format(as.Date(Date, format="%Y-%m-%d"), format = "%j"))]

### add targets
data.all.rgf[, WnvPresentBinary := WnvPresent]
data.all.rgf[WnvPresent>0, WnvPresentBinary := 1]

### add previous NumBatches ####
for (NPreviousBatches in c(30)) {
  cols <- c("TrapUniqueId", "Date", "Species")
  for (i in 1:NPreviousBatches) {
    data.all.previous <- data.all.rgf[,c(cols, "NumBatches"),with=F]
    setnames(data.all.previous, "NumBatches", paste0("NumBatches", NPreviousBatches-i))
    data.all.previous[, Date := as.character(as.Date(data.all.previous$Date)+i)]
    data.all.rgf <- merge(data.all.rgf, data.all.previous, by=cols, all.x=TRUE)
  }
  col.numbatches <- c(paste0("NumBatches",c(0:(NPreviousBatches-1))))
  data.numbatches <- as.matrix(data.all.rgf[,col.numbatches,with=F])
  data.numbatches <- t(apply(data.numbatches, 1, function(x) c(max(x[!is.na(x)]),
                                                               sum(x[!is.na(x)]))))
  data.numbatches[is.na(data.numbatches)] <- 0
  data.numbatches[data.numbatches==-Inf] <- 0
  colnames(data.numbatches) <- c(paste0("NumBatchesPrevious",NPreviousBatches,"Max"),
                                 paste0("NumBatchesPrevious",NPreviousBatches,"Sum"))
  for (col in col.numbatches) {
    setnames(data.all.rgf, col, "feature")
    data.all.rgf[, feature := NULL]
  }
  data.all.rgf <- cbind(data.all.rgf, data.numbatches)
}

### add N closest traps ###
Nclosest <- 10
data.traps <- data.all.rgf[,list(TrapUniqueId, Longitude, Latitude)]
setkey(data.traps, TrapUniqueId, Longitude, Latitude)
data.traps <- unique(data.traps)
data.traps.loc <- as.matrix(data.traps[,list(Longitude, Latitude)])
ix <- as.matrix(expand.grid(1:nrow(data.traps.loc), 1:nrow(data.traps.loc)))
data.traps.dist <- distHaversine(data.traps.loc[ix[,2],], data.traps.loc[ix[,1],], r = 6378.137)
data.traps.dist <- matrix(data.traps.dist, ncol=nrow(data.traps.loc), byrow=T)
traps <- data.traps$TrapUniqueId
data.traps.closest <- t(apply(data.traps.dist, 1, function(x) traps[sort(x, index.return=T)$ix[2:(1+Nclosest)]]))
#data.traps.dist.closest <- t(apply(data.traps.dist, 1, function(x) sort(x)[2:(1+Nclosest)]))
#data.traps.closest[which(data.traps.dist.closest>3)] <- NA
data.traps <- cbind(data.traps, as.data.table(data.traps.closest))
setnames(data.traps, paste0("V",c(1:Nclosest)), paste0("TrapClosest", c(1:Nclosest)))
data.all.rgf <- merge(data.all.rgf, data.traps, by=c("TrapUniqueId","Longitude","Latitude"), all.x=T)

#################################################################
###################### WEATHER DATA #############################
#################################################################

data.weather <- fread(paste0(data.folder, weather.file))
data.weather[, Depth := NULL]
data.weather[, Water1 := NULL]
data.weather[grepl("T", SnowFall), SnowFall := "0.05"]
data.weather[grepl("T", PrecipTotal), PrecipTotal := "0.005"]

### fill data from one weather station to another one ###
cols <- setdiff(colnames(data.weather), c("Station", "Date"))
data.weather.station1 <- data.weather[Station == 1]
data.weather.station1[, Station := NULL]
setnames(data.weather.station1, cols, paste0(cols, "_station1"))
data.weather.station2 <- data.weather[Station == 2]
setnames(data.weather.station2, cols, paste0(cols, "_station2"))
data.weather.station2[, Station := NULL]
data.weather.stations <- merge(data.weather.station1, data.weather.station2, by="Date")
for (col in cols) {
  setnames(data.weather.stations, paste0(col,"_station1"), "feature_station1")
  setnames(data.weather.stations, paste0(col,"_station2"), "feature_station2")
  data.weather.stations[feature_station1 == "M" 
                        | feature_station1 == "-" 
                        | feature_station1 == "", feature_station1 := feature_station2]
  data.weather.stations[feature_station2 == "M" 
                        | feature_station2 == "-" 
                        | feature_station2 == "", feature_station2 := feature_station1]
  setnames(data.weather.stations, "feature_station1", paste0(col,"_station1"))
  setnames(data.weather.stations, "feature_station2", paste0(col,"_station2"))
}

data.weather.station1 <- data.weather.stations[,c("Date", paste0(cols,"_station1")), with=F]
setnames(data.weather.station1, paste0(cols, "_station1"), cols)
data.weather.station1[, Station := 1]
data.weather.station2 <- data.weather.stations[,c("Date", paste0(cols,"_station2")), with=F]
setnames(data.weather.station2, paste0(cols, "_station2"), cols)
data.weather.station2[, Station := 2]

data.weather <- rbindlist(list(data.weather.station1, data.weather.station2), use.names=T, fill=T) 
setkey(data.weather, Date)

cols <- setdiff(colnames(data.weather), c("Station", "Date", "CodeSum"))
for (col in cols) {
  setnames(data.weather, col, "feature")
  ix <- which(data.weather$feature=="M" | data.weather$feature == "" | data.weather$feature == "-" | is.na(data.weather$feature))
  if (length(ix) > 0) {
    data.weather[ix, feature := "-1"]
  }
  data.weather[, feature := as.numeric(feature)]
  setnames(data.weather, "feature", col)  
}

data.weather[, Id := as.integer(c(1:nrow(data.weather)))]
weather.codes <- strsplit(data.weather$CodeSum, " ")
data.weather.codes <- data.table(Id = unlist(sapply(c(1:nrow(data.weather)), function(x) rep(data.weather$Id[x], length(weather.codes[[x]])))),
                                 CodeSum = unlist(weather.codes),
                                 Count = 1)
data.weather.codes <- dcast.data.table(data.weather.codes, 
                                       Id~CodeSum,
                                       fill = 0,
                                       value.var = "Count")
setnames(data.weather.codes, 
         colnames(data.weather.codes), 
         gsub("\\+", "PLUS", colnames(data.weather.codes)))
data.weather.codes[, V1 := NULL]
data.weather <- merge(data.weather, data.weather.codes, by="Id")
data.weather[, CodeSum := NULL]
data.weather[, Id := NULL]

### closest weather station for each trap ###
data.traps <- data.all.rgf[,list(Trap, Longitude, Latitude)]
setkey(data.traps, Trap, Longitude, Latitude)
data.traps <- unique(data.traps)
dists1 <- distHaversine(as.matrix(data.traps[,list(Longitude, Latitude)]), weather.station1, r = 6378.137)
dists2 <- distHaversine(as.matrix(data.traps[,list(Longitude, Latitude)]), weather.station2, r = 6378.137)
data.traps[, Station := as.numeric(dists1>dists2)+1]
data.all.rgf <- merge(data.all.rgf, data.traps, by=c("Trap","Longitude","Latitude"), all.x=TRUE)
### add weather information to the main data ###
data.all.rgf <- merge(data.all.rgf, data.weather, by=c("Date", "Station"), all.x=TRUE)
Store(data.all.rgf)
