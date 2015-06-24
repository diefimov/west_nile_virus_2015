##############################################################
## trap data
##############################################################
rm(list = ls())
source("fn.base.R")

tic()
cat("Loading trap data... \n")

data.tr <- fread(fn.in.file("train.csv"))
data.test <- fread(fn.in.file("test.csv"))

cols.tr.orig <- colnames(data.tr)
data.tr.info <- unique(data.test[, list(Address, Block, Street, Trap,
                                        AddressNumberAndStreet, 
                                        Latitude, Longitude,
                                        AddressAccuracy)])

data.tr.aug <- data.table(expand.grid(Date = unique(data.tr$Date),
                                      Address = unique(data.test$Address),
                                      Species = unique(data.test$Species)))
data.tr.aug <- merge(data.tr.aug, data.tr.info,
                     by=c("Address"))

data.tr.aug <- merge(data.tr.aug, 
                     data.tr[, list(Date, Address, Species,
                                    NumMosquitos, WnvPresent)],
                     by=c("Date", "Address", "Species"),
                     all.x=T,
                     allow.cartesian=T)
data.tr.aug <- data.tr.aug[, cols.tr.orig, with=F][order(Date, Species)]

data.tr <- data.tr.aug

data.tr[, Id := -(.N:1)]

# Used in python scripts
write.csv(
  data.tr,
  file = fn.in.file("train_aug.csv"), 
  na = '',
  row.names = F, quote = T)

data.test[, NumMosquitos := NA_integer_]
data.test[, WnvPresent := NA_integer_]

data.tr.out.max <- data.tr[
  !is.na(WnvPresent),
  list(WnvPresent=max(WnvPresent),
       WnvPresentSum=sum(WnvPresent),
       Id = Id[which.max(WnvPresent)]),
  by=c("Date", "Trap", "Species")]
data.tr.out.max <- data.tr.out.max[, list(Id, WnvPresent, WnvPresentSum)]
Store(data.tr.out.max)

data.all.species <- rbind(
  data.tr[, list(Id, Species)],
  data.test[, list(Id, Species)])
setkeyv(data.all.species, "Id")

Store(data.all.species)

cols.first <- c("Id", "WnvPresent", "NumMosquitos", "Date")
cols.last <- setdiff(colnames(data.tr), cols.first)
data.all.trap <- rbind(
  data.tr[, c(cols.first, cols.last), with=F],
  data.test[, c(cols.first, cols.last), with=F]
)
data.all.trap <- data.all.trap[order(Id)]

fn.add.date.fields(data.all.trap)

data.all.trap.fix <- data.all.trap[
  ,list(
    NeedFix = length(unique(AddressNumberAndStreet)) > 1,
    AddressNumberAndStreet = unique(AddressNumberAndStreet),
    TrapFixed = paste(Trap, 1:length(unique(AddressNumberAndStreet)), sep="_")
  )
  ,by=c("Trap")][NeedFix==T]
data.all.trap.fix[, NeedFix:= NULL]

data.all.trap <- merge(data.all.trap, data.all.trap.fix, all.x=T,
                       by=c("Trap", "AddressNumberAndStreet"))
data.all.trap[, TrapFixedDiff := TrapFixed]
data.all.trap[is.na(TrapFixedDiff), TrapFixedDiff := "NotFix"]

data.all.trap[is.na(TrapFixed), TrapFixed := Trap]

rm(data.all.trap.fix)

trap.count.key <- c("TrapFixed", "Date", "Species")

data.all.trap.count <- data.all.trap[
  ,list(TrapCount = as.numeric(.N), TrapYear=unique(Year)), 
  by=trap.count.key]
setkeyv(data.all.trap.count, trap.count.key)

cols.trap <- colnames(data.all.trap)
data.all.trap <- merge(data.all.trap, data.all.trap.count, by=trap.count.key)
data.all.trap <- data.all.trap[, c(cols.trap, "TrapCount"), with=F]
data.all.trap[, TrapCount := TrapCount - 1]

data.all.out <- data.all.trap[, list(Id, WnvPresent, NumMosquitos, Date, Year)]
setkeyv(data.all.out, "Id")
data.all.trap[, NumMosquitos := NULL]
data.all.trap[, WnvPresent := NULL]
data.all.trap[, Date := NULL]

# Redundant info
data.all.trap[, Address := NULL]
data.all.trap[, Street := NULL]
data.all.trap[, AddressNumberAndStreet := NULL]

# Factor transform
data.all.trap[, Block := factor(Block, levels=sort(unique(Block)))]
data.all.trap[, Trap := factor(Trap, levels=sort(unique(Trap)))]
data.all.trap[, TrapFixed := factor(TrapFixed, levels=sort(unique(TrapFixed)))]
data.all.trap[, TrapFixedDiff := factor(TrapFixedDiff, 
                                        levels=sort(unique(TrapFixedDiff)))]

#                   Species       Mean    N
# 1:           CULEX ZOTHER 0.00000000  315
# 2:          CULEX PIPIENS 0.08892182 2699
# 3: CULEX PIPIENS/RESTUANS 0.05513468 4752
# 4:         CULEX RESTUANS 0.01788321 2740

data.all.trap[
  !Species %chin% c("CULEX PIPIENS", "CULEX PIPIENS/RESTUANS", 
                    "CULEX RESTUANS"), 
  Species:= "CULEX ZOTHER"]
data.all.trap[, Species := factor(Species, levels=sort(unique(Species)))]

data.all.id.zero <- 
  sort(data.all.trap$Id[data.all.trap$Species == "CULEX ZOTHER"])

setkeyv(data.all.trap, "Id")
rm(data.tr, data.test, cols.first, cols.last)

Store(data.all.trap, data.all.out, data.all.id.zero)

# trap distance data

data.trap.dist <- data.table(
  expand.grid(TrapFixed1=sort(unique(data.all.trap$TrapFixed)),
              TrapFixed2=sort(unique(data.all.trap$TrapFixed))))
data.trap.dist <- data.trap.dist[TrapFixed2 != TrapFixed1]

for (ix in 2:1) {
  col.nam <- paste0("TrapFixed", ix)
  setnames(data.trap.dist, col.nam, "TrapFixed")
  data.trap.dist <- merge(
    data.trap.dist, 
    unique(data.all.trap[, list(TrapFixed, Latitude, Longitude)]),
    by="TrapFixed")
  setnames(data.trap.dist, 
           c("TrapFixed", "Latitude", "Longitude"), 
           c(col.nam, paste0(c("Lat", "Lon"), ix)))
}

data.trap.dist[, Tdist := fn.dist.coord(Lat1, Lon1, Lat2, Lon2)] 
data.trap.dist[, Lat1:= NULL]
data.trap.dist[, Lon1:= NULL]
data.trap.dist[, Lat2:= NULL]
data.trap.dist[, Lon2:= NULL]

data.trap.dist <- data.trap.dist[order(TrapFixed2, Tdist)]
setkeyv(data.trap.dist, "TrapFixed1")

Store(data.trap.dist)

data.trap.stats.full <- merge(data.all.out, data.all.trap[
  , list(Id, TrapFixed, TrapCount)], by="Id")
data.trap.stats.sum <- data.trap.stats.full[
  , list(
    TrapCountPos=length(unique(Year[TrapCount>0])), 
    YrsPos=length(unique(Year[which(WnvPresent>0)])), 
    TotalPos=sum(WnvPresent, na.rm=T))
  , by="TrapFixed"][order(TrapFixed)]
setkeyv(data.trap.stats.sum, c("TrapFixed"))

data.trap.stats.yr <- data.trap.stats.full[
  , list(
    YrsPos2=length(unique(Year[which(WnvPresent>0)])), 
    TotalPos2=sum(WnvPresent, na.rm=T))
  , by=c("TrapFixed", "Year")][order(TrapFixed)]

data.trap.stats <- merge(data.trap.stats.yr,
                         data.trap.stats.sum, by="TrapFixed")
data.trap.stats[,YrsPos:=YrsPos-YrsPos2]
data.trap.stats[,TotalPos:=TotalPos-TotalPos2]

data.trap.stats[,YrsPos2:=NULL]
data.trap.stats[,TotalPos2:=NULL]

setkeyv(data.trap.stats, c("TrapFixed", "Year"))
Store(data.trap.stats, data.trap.stats.sum)

toc()

##############################################################
## weather data
##############################################################
rm(list = ls())
source("fn.base.R")

tic()
cat("Loading weather data... \n")

data.weather <- fread(fn.in.file("weather.csv"))
data.weather.codesum.id <- data.weather[, list(Station, Date, CodeSum)]
data.weather[, CodeSum := NULL]
data.weather[, Water1 := NULL]
data.weather[, Depth := NULL]

for (col.nam in setdiff(colnames(data.weather), c("Station", "Date"))) {
  suppressWarnings(data.weather[, col.nam := as.numeric(get(col.nam)), with=F])
}

data.weather <- merge(
  data.weather[Station == 1], data.weather[Station == 2], 
  by="Date", suffixes = c("St1", "St2"))
data.weather[, StationSt1:=NULL]
data.weather[, StationSt2:=NULL]

data.weather <- data.weather[order(Date)]

cols.rm <- c(
  "WetBulbSt1",
  "StnPressureSt1", "SeaLevelSt1",  "ResultSpeedSt1", "AvgSpeedSt1",
  "WetBulbSt2",  "HeatSt2", "CoolSt2", "StnPressureSt2", "SeaLevelSt2",
  "ResultSpeedSt2", "AvgSpeedSt2", "SunriseSt2", "SunsetSt2", 
  "SnowFallSt2", "DepartSt2")
for (col.nam in cols.rm) {
  data.weather[, col.nam:=NULL, with=F]
}

setkeyv(data.weather, "Date")
Store(data.weather)

fn.weather.smooth <- function(...) {
  cols.weather <- colnames(data.weather)
  data.weather.smooth <- copy(data.weather)
  fn.add.date.fields(data.weather.smooth)
  for (feat in cols.weather[-1]) {
    if (!feat %in% c('SunriseSt1', 'SunsetSt1')) {
      for (year in 2007:2014) {
        year.ix <- data.weather.smooth$Year == year
        model.loess <- loess(
          as.formula(paste(feat, "DayYear", sep="~")), 
          data=data.weather.smooth[year.ix], ...)
        pred <- predict(model.loess, data.weather.smooth[year.ix])
        data.weather.smooth[year.ix, feat := pred, with=F]
      }
    }
  }
  data.weather.smooth <- data.weather.smooth[,cols.weather,with=F]
  data.weather.smooth[, SunriseSt1 := NULL]
  data.weather.smooth[, SunsetSt1 := NULL]
  setkeyv(data.weather.smooth, "Date")
  data.weather.smooth
}

data.weather.smooth.01 <- fn.weather.smooth(span = 0.1)
Store(data.weather.smooth.01)

data.weather.smooth.02 <- fn.weather.smooth(span = 0.2)
Store(data.weather.smooth.02)

toc()

##############################################################
## history data
##############################################################
rm(list = ls())
source("fn.base.R")

tic()
cat("Creating history data... \n")

data.all.trap.dup <- merge(
  data.all.trap[TrapCount > 0, list(Id, TrapFixed, Species, TrapCount)],
  data.all.out[, list(Id, Date)], by="Id")


data.dup.key <- c("Date", "TrapFixed")
data.all.trap.dup <- data.all.trap.dup[
  , list(TrapCount=max(TrapCount)), 
  by=data.dup.key]

setkeyv(data.all.trap.dup, data.dup.key)

data.all.trap.hist <- merge(
  data.all.trap[, list(Id, TrapFixed, Species)],
  data.all.out[, list(Id, Date)], by="Id")

trap.dates <- as.Date(data.all.trap.hist$Date, format="%Y-%m-%d")

cat("Previous TrapCount features... \n")
days.back <- 90
data.all.trap.mat <- Matrix(data=NA_real_, 
                            nrow=nrow(data.all.trap.hist), 
                            ncol=days.back)
for (ix in 1:days.back) {
  trap.dt <- data.all.trap.hist[,data.dup.key,with=F]
  trap.dt[, Date := format(trap.dates-ix, format = "%Y-%m-%d")]
  data.all.trap.mat[,ix] <- data.all.trap.dup[trap.dt]$TrapCount
}

data.all.trap.hist[
  , TrapCountPrevAge := apply(data.all.trap.mat, 1, 
                              function(x) 
                                c(which(!is.na(x)), NA_real_)[1])]
data.all.trap.hist[
  , TrapCountPrev := apply(data.all.trap.mat[, 1:10], 1, 
                           function(x) 
                             x[c(which(!is.na(x)), 1)[1]])]
data.all.trap.hist[is.na(TrapCountPrev), TrapCountPrev :=0]

data.all.trap.hist[
  , TrapCountPrevMean := apply(data.all.trap.mat[,1:30], 1, 
                               mean, na.rm=T)]

cat("done. \n")

data.all.trap.hist[, TrapFixed := NULL]
data.all.trap.hist[, Species := NULL]
data.all.trap.hist[, Date := NULL]

setkeyv(data.all.trap.hist, "Id")
Store(data.all.trap.hist)

toc()


##############################################################
## merge all features
##############################################################
rm(list = ls())
source("fn.base.R")
tic()
cat("Creating features data... \n")



data.all.feat <- copy(data.all.trap)
data.all.feat <- merge(data.all.feat, data.all.out[, list(Id,Date)], by="Id")

fn.merge <- function(data.add, ...)  {
  merge(data.all.feat, data.add, by=key(data.add), all.x=T, ...)
}

data.all.feat <- fn.merge(data.weather)
data.all.feat <- fn.merge(data.weather.smooth.01, suffixes=c("", "Smth01"))
data.all.feat <- fn.merge(data.weather.smooth.02, suffixes=c("", "Smth02"))

# Previous weather features
weather.dates <- as.Date(data.weather$Date, format="%Y-%m-%d")
weather.list.prev <- list(
  Smth01=data.weather.smooth.01,
  Smth02=data.weather.smooth.02)
for (ix in c(1:8)) {
  for (w.nam in names(weather.list.prev)) {
    data.weather.prev <- copy(weather.list.prev[[w.nam]])
    data.weather.prev[, Date := format(weather.dates + ix, format = "%Y-%m-%d")]
    setkeyv(data.weather.prev, key(data.weather))
    data.all.feat <- fn.merge(data.weather.prev, 
                              suffixes=c("", paste0("Prv",ix, w.nam)))
  }
}

data.all.feat <- fn.merge(data.all.trap.hist)


data.all.feat[, Date := NULL]
setkeyv(data.all.feat, "Id")
Store(data.all.feat)


data.all.feat.tree <- fn.data.2.tree(data.all.feat)

setkeyv(data.all.feat.tree, "Id")
Store(data.all.feat.tree)
toc()


##############################################################
## cross validation indexes
##############################################################
rm(list = ls())
source("fn.base.R")

tic()
cat("Building cv... ")

data.cv.folds <- fn.cv.folds(data.all.out)

Store(data.cv.folds)

toc()

##############################################################
## year multiplier calculation
##############################################################
rm(list = ls())
source("fn.base.R")

tic()
cat("year multiplier calculation... \n")

data.tmp.01.pred <- data.all.trap[, list(Id, Year, Species)]
data.tmp.01.pred[, Pred := 0.0]
Store(data.tmp.01.pred)

cat("using auc... \n")
data.tr.mult <- NULL

n.reps <- 50
n.samp <- nrow(data.tmp.01.pred)
for (ix in 1:n.reps) {
  set.seed(ix + 356782)
  x.ix <- sample(n.samp, ceiling(n.samp*0.3))
  data.tr.mult.cur <- data.all.out[
    x.ix, 
    list(
      AUC = NA_real_, 
      ActRatio = mean(WnvPresent, na.rm=T),
      N = sum(!is.na(WnvPresent))), 
    by="Year"][!is.na(ActRatio)][order(Year)]
  data.tr.mult.cur[, ActRatio := ActRatio/max(ActRatio)]
  for (yr in seq(2007, 2013, 2)) {
    data.tmp.01.pred[, Pred := as.numeric(Year == yr & Species != "CULEX ZOTHER")]
    res <- fn.print.err(data.tmp.01.pred[x.ix], do.print=F)
    data.tr.mult.cur[Year == yr, AUC := res$AUC]
  }
  data.tr.mult <- rbind(data.tr.mult, data.tr.mult.cur)
}
set.seed(Sys.time())
model.mult <- lm(ActRatio ~ AUC + 1, data=data.tr.mult)
data.tr.mult.ratio <- data.tr.mult[, list(Year, ActRatio)] 
setnames(data.tr.mult.ratio, "ActRatio", "PredRatio")
Store(data.tr.mult, data.tr.mult.ratio, model.mult)

data.mult.01 <- data.all.out[
  , list(AUC = NA_real_, ActRatio = mean(WnvPresent, na.rm=T)), by="Year"]
data.mult.01 <- data.mult.01[order(Year + (Year %% 2 == 0)*100)]
data.mult.01[Year %% 2 == 1, ActRatio := ActRatio/max(ActRatio)]
data.mult.01[Year %% 2 == 0, ActRatio := NA_real_]
for (yr in seq(2007, 2013, 2)) {
  data.tmp.01.pred[, Pred := as.numeric(Year == yr & Species != "CULEX ZOTHER")]
  res <- fn.print.err(data.tmp.01.pred, do.print=F)
  data.mult.01[Year == yr, AUC := res$AUC]
}
data.mult.01[Year == 2008, AUC := 0.44616]
data.mult.01[Year == 2010, AUC := 0.44130]
data.mult.01[Year == 2012, AUC := 0.64752]
data.mult.01[Year == 2014, AUC := 0.49158]

data.mult.01[, PredRatio := predict(model.mult, data.mult.01)]
for (ix in 0:1) {
  data.mult.01[Year %% 2 == ix, PredRatio := PredRatio/max(PredRatio)]
}

# print(data.mult.01[, list(Year, ActRatio, PredRatio)])
#    Year   ActRatio PredRatio
# 1: 2007 0.61977825 0.6914741
# 2: 2009 0.08455269 0.0720780
# 3: 2011 0.27773953 0.3013745
# 4: 2013 1.00000000 1.0000000
# 5: 2008         NA 0.2135175
# 6: 2010         NA 0.1945351
# 7: 2012         NA 1.0000000
# 8: 2014         NA 0.3909214

Store(data.mult.01)


toc()

##############################################################
## month multiplier calculation 
##############################################################
rm(list = ls())
source("fn.base.R")

tic()
cat("month multiplier calculation... \n")

data.tmp.m.01.pred <- data.all.trap[, list(Id, Year, Month, Species)]
data.tmp.m.01.pred[, Pred := 0.0]
Store(data.tmp.m.01.pred)

cat("using auc... \n")
data.all.out.cur <- merge(
  data.all.out,
  data.all.trap[, list(Id, Month, WeekYear, Species, TrapCount)],
  by="Id")

n.reps <- 40
n.samp <- nrow(data.tmp.m.01.pred)

data.tr.m.mult <- NULL
mult.m.group <- c("Year", "Month")
mult.m.expr <- quote(
  list(
    AUC = NA_real_, 
    ActRatio = mean(c(WnvPresent, data.smoother), na.rm=T),
    Count=sum(length(unique(Date))),
    CountReps=sum(TrapCount[!duplicated(Date)])
  )
)

fn.data.pred <- function(data, year, month) 
  as.numeric(with(data, Year == year & Month == month 
                  & Species != "CULEX ZOTHER"))

# data.smoother <- rep(mean(data.all.out$WnvPresent, na.rm=T), 1)
data.smoother <- rep(sum(data.all.out$WnvPresent, na.rm=T)/
                       nrow(data.all.out[Year %% 2 == 1]), 0)
for (ix in 1:n.reps) {
  set.seed(ix + 234579)
  samp.rate <- 0.3
  x.ix <- sample(n.samp, ceiling(n.samp*samp.rate))
  data.tr.m.mult.cur <- data.all.out.cur[
    x.ix, eval(mult.m.expr), 
    by=mult.m.group][!is.na(ActRatio)][order(Year, Month)]
  data.tr.m.mult.cur[, Count := Count/samp.rate]
  data.tr.m.mult.cur[, CountReps := CountReps/samp.rate]
  
  for (yr in seq(2007, 2013, 2)) {
    for (mnth in unique(data.tmp.m.01.pred[Year == yr]$Month)) {
      data.tmp.m.01.pred[, Pred := fn.data.pred(.SD, yr, mnth)]
      auc.df <- fn.print.err(data.tmp.m.01.pred[x.ix], do.print=F)
      data.tr.m.mult.cur[Year == yr & Month == mnth, 
                         AUC := auc.df$AUC]
    }
  }
  data.tr.m.mult <- rbind(data.tr.m.mult, data.tr.m.mult.cur)
}
set.seed(Sys.time())
model.m.mult <- lm(ActRatio ~ 1+ AUC, 
                   data=data.tr.m.mult)
data.tr.m.mult.ratio <- data.tr.m.mult[, list(Year, Month, ActRatio)] 
setnames(data.tr.m.mult.ratio, "ActRatio", "PredRatio")
Store(data.tr.m.mult, data.tr.m.mult.ratio, model.m.mult)

data.mult.m.01 <- data.all.out.cur[, eval(mult.m.expr), by=mult.m.group]
data.mult.m.01 <- data.mult.m.01[order(Year + (Year %% 2 == 0)*100, Month)]
data.mult.m.01[Year %% 2 == 0, ActRatio := NA_real_]
for (yr in seq(2007, 2013, 2)) {
  for (mnth in unique(data.mult.m.01[Year == yr]$Month)) {
    data.tmp.m.01.pred[ , Pred := fn.data.pred(.SD, yr, mnth)]
    auc.df <- fn.print.err(data.tmp.m.01.pred, do.print=F)
    data.mult.m.01[Year == yr & Month == mnth, AUC := auc.df$AUC]
  }
}

data.mult.m.01[Year == 2008 & Month == 6, AUC := 0.48879]
data.mult.m.01[Year == 2008 & Month == 7, AUC := 0.46413]
data.mult.m.01[Year == 2008 & Month == 8, AUC := 0.48532]
data.mult.m.01[Year == 2008 & Month == 9, AUC := 0.50793]
data.mult.m.01[Year == 2010 & Month == 6, AUC := 0.48748]
data.mult.m.01[Year == 2010 & Month == 7, AUC := 0.47064]
data.mult.m.01[Year == 2010 & Month == 8, AUC := 0.49407]
data.mult.m.01[Year == 2010 & Month == 9, AUC := 0.49266]
data.mult.m.01[Year == 2010 & Month == 10, AUC := 0.49645]
data.mult.m.01[Year == 2012 & Month == 6, AUC := 0.48169]
data.mult.m.01[Year == 2012 & Month == 7, AUC := 0.59749]
data.mult.m.01[Year == 2012 & Month == 8, AUC := 0.58453]
data.mult.m.01[Year == 2012 & Month == 9, AUC := 0.48381]
data.mult.m.01[Year == 2014 & Month == 6, AUC := 0.47627]
data.mult.m.01[Year == 2014 & Month == 7, AUC := 0.47351]
data.mult.m.01[Year == 2014 & Month == 8, AUC := 0.53614]
data.mult.m.01[Year == 2014 & Month == 9, AUC := 0.50958]
data.mult.m.01[Year == 2014 & Month == 10, AUC := 0.49608]


data.mult.m.01 <- data.mult.m.01[!is.na(AUC)]
data.mult.m.01[, PredRatio := predict(model.m.mult, data.mult.m.01)]


for (ix in 0:1) {
  data.mult.m.01[Year %% 2 == ix, PredRatio := PredRatio/max(PredRatio)]
}
data.mult.m.01[PredRatio < 0, PredRatio := 0]

# print(data.mult.m.01[, list(Year, Month, AUC, ActRatio, PredRatio)])
#     Year Month       AUC    ActRatio  PredRatio
#  1: 2007     5 0.4987443 0.000000000 0.21165616
#  2: 2007     6 0.4912104 0.000000000 0.14806056
#  3: 2007     7 0.4778203 0.010434783 0.03503121
#  4: 2007     8 0.5921361 0.097560976 1.00000000
#  5: 2007     9 0.4903506 0.036175711 0.14080227
#  6: 2007    10 0.4918701 0.009478673 0.15362912
#  7: 2009     5 0.4970367 0.000000000 1.00000000
#  8: 2009     6 0.4713712 0.000000000 0.00000000
#  9: 2009     7 0.4677215 0.006622517 0.00000000
# 10: 2009     8 0.4901358 0.024064171 0.70466750
# 11: 2009     9 0.4840952 0.011961722 0.44614965
# 12: 2009    10 0.4968358 0.000000000 0.99140198
# 13: 2011     6 0.4824209 0.000000000 0.33579755
# 14: 2011     7 0.4802480 0.017187500 0.25241588
# 15: 2011     8 0.4997294 0.048681542 1.00000000
# 16: 2011     9 0.4947502 0.040740741 0.80892912
# 17: 2013     6 0.4791093 0.002293578 0.04438339
# 18: 2013     7 0.4913416 0.037735849 0.14420106
# 19: 2013     8 0.5962165 0.172661871 1.00000000
# 20: 2013     9 0.5426771 0.144032922 0.56310829
# 21: 2008     6 0.4887900          NA 0.44132550
# 22: 2008     7 0.4641300          NA 0.00000000
# 23: 2008     8 0.4853200          NA 0.34004021
# 24: 2008     9 0.5079300          NA 1.00000000
# 25: 2010     6 0.4874800          NA 0.60622771
# 26: 2010     7 0.4706400          NA 0.00000000
# 27: 2010     8 0.4940700          NA 0.89552084
# 28: 2010     9 0.4926600          NA 0.83362353
# 29: 2010    10 0.4964500          NA 1.00000000
# 30: 2012     6 0.4816900          NA 0.06476890
# 31: 2012     7 0.5974900          NA 1.00000000
# 32: 2012     8 0.5845300          NA 0.89533165
# 33: 2012     9 0.4838100          NA 0.08189057
# 34: 2014     6 0.4762700          NA 0.04161481
# 35: 2014     7 0.4735100          NA 0.00000000
# 36: 2014     8 0.5361400          NA 1.00000000
# 37: 2014     9 0.5095800          NA 0.57483363
# 38: 2014    10 0.4960800          NA 0.35872873

data.mult.my <- list(data.mult.m.01, data.mult.01)
Store(data.mult.m.01, data.mult.my)

toc()
