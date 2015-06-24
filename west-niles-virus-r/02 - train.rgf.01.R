source("data.build.rgf.R")

#############################################################
# train rgf model
#############################################################
rm(list = ls())
source("fn.base.R")

flist <- c("Longitude",
           "Latitude",
           "NumBatches",
           "NumBatchesPrevious30Sum",
           "NumBatchesPrevious30Max",
           "DayYear",
           "TS", "FG", "DewPoint", "Tmin",
           col.species)
target <- "WnvPresentBinary"

predicted.years <- list(2007,
                        2009,
                        2011,
                        2013,
                        c(2008,2010,2012,2014))
train.years <- list(c(2009, 2011, 2013),
                    c(2007, 2011, 2013),
                    c(2007, 2009, 2013),
                    c(2007, 2009, 2011),
                    c(2007,2009,2011,2013))

prefix.model <- paste0("rgf.model")
rgf.data.folder <- "../data/output-rgf/"
train.x.file <- paste0(rgf.data.folder, "train.x.csv")
train.y.file <- paste0(rgf.data.folder, "train.y.csv")
test.x.file <- paste0(rgf.data.folder, "test.x.csv")
model.file <- paste0(rgf.data.folder, "rgf.model")
test.y.file <- paste0(rgf.data.folder, "test.y.csv")
train.settings.file <- paste0(rgf.data.folder,"train.rgf.settings")
test.settings.file <- paste0(rgf.data.folder,"test.rgf.settings")

trainSettings <- file(paste0(train.settings.file, ".inp"))
pars <- paste0("train_x_fn=",train.x.file,"\n",
               "train_y_fn=",train.y.file,"\n",
               "model_fn_prefix=",model.file,"\n",
               "reg_L2=", 0.2, "\n",
               "reg_sL2=", 0.07, "\n",
               #"reg_depth=", 1.01, "\n",
               "algorithm=","RGF","\n",
               "loss=","Log","\n",
               "num_iteration_opt=", 7, "\n",
               "num_tree_search=", 1, "\n",
               "min_pop=", 8, "\n",
               "opt_stepsize=", 0.7, "\n",
               #"opt_interval=", 100, "\n",
               "test_interval=",1400,"\n",
               "max_leaf_forest=",1400,"\n",
               "Verbose","\n")
writeLines(pars, trainSettings)
close(trainSettings)

count <- 1
for (i in 1:length(predicted.years)) {
  ix <- which(!is.na(data.all.rgf[[target]]))
  train.x <- as.data.frame(data.all.rgf[ix][Year %in% train.years[[i]]][,flist,with=F])
  train.y <- data.all.rgf[ix][Year %in% train.years[[i]]][[target]]
  test.x <- as.data.frame(data.all.rgf[Year %in% predicted.years[[i]]][,flist,with=F])
  test.y <- data.all.rgf[Year %in% predicted.years[[i]]][["WnvPresentBinary"]]
  
  write.table(
    train.x,
    file=train.x.file,
    row.names = F, quote = F, na = "", sep = " ",
    append = F, col.names = F
  )
  
  write.table(
    test.x,
    file=test.x.file,
    row.names = F, quote = F, na = "", sep = " ",
    append = F, col.names = F
  )
  
  if (length(unique(train.y))<3) {
    write.table((train.y-0.5)*2,
                file = train.y.file,
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
  } else {
    write.table(train.y,
                file = train.y.file,
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
  }

  system(paste("perl ../rgf1.2/test/call_exe.pl",
               "../rgf1.2/bin/rgf train",
               train.settings.file,
               ">>", paste0(rgf.data.folder, "rgf.log"), "2>&1"))
  
  models <- list.files(rgf.data.folder, pattern=paste0("^",prefix.model))
  ix <- which.max(sapply(strsplit(models, "-"), function(x) as.numeric(x[2])))
  model <- models[ix]
  
  testSettings<-file(paste0(test.settings.file, ".inp"))
  pars <- paste0("test_x_fn=",test.x.file,"\n",
                 "model_fn=",rgf.data.folder, model,"\n",
                 "prediction_fn=", test.y.file,"\n")
  writeLines(pars, testSettings)
  close(testSettings)
  
  system(paste("perl ../rgf1.2/test/call_exe.pl",
               "../rgf1.2/bin/rgf predict",
               test.settings.file,
               ">>", paste0(rgf.data.folder, "rgf.log"), "2>&1"))
  if (length(unique(train.y)) < 3) {
    pred <- 1/(1+exp(-scan(test.y.file)))
  } else {
    pred <- scan(test.y.file)
    min_pred <- min(pred)
    max_pred <- max(pred)
    pred <- (pred - min_pred)/(max_pred - min_pred)
  }
  #cat ("average auc:", auc(test.y, pred), "\n")
  
  cols <- c("Trap", "TrapUniqueId", "Longitude", "Latitude", "Date", "Species", "NumBatches", paste0("TrapClosest", c(1:10)))
  data.pred.epoch <- data.all.rgf[Year %in% predicted.years[[i]]][,cols,with=F]
  data.pred.epoch[, Pred := pred]
  
  if (count == 1) {
    data.pred <- copy(data.pred.epoch)
  } else {
    data.pred <- rbind(data.pred, data.pred.epoch)
  }
  count <- count + 1 
  tatam <- file.remove(dir(
    rgf.data.folder, 
    pattern = paste0("^",prefix.model), 
    full.names = T))
}
data.pred <- merge(data.train.test.rgf, data.pred, by=c("Trap","Longitude","Latitude","Date","Species"), all.x=T)
data.pred[is.na(Pred), Pred := 0.0]
data.pred[,Year := year(as.Date(Date))]
data.pred[,Month := month(as.Date(Date))]
err1 <- auc(data.pred[!is.na(WnvPresent), WnvPresent], data.pred[!is.na(WnvPresent), Pred])

data.avg <- data.pred[,list(YearAvgActual = mean(WnvPresent[!is.na(WnvPresent)]),
                            YearAvgPred = mean(Pred)), by="Year"]
data.avg[Year==2008, YearAvgActual := 0.21311596]
data.avg[Year==2010, YearAvgActual := 0.19412383]
data.avg[Year==2012, YearAvgActual := 1.00000000]
data.avg[Year==2014, YearAvgActual := 0.39061037]
data.pred <- merge(data.pred, data.avg, by="Year", all.x=T)

err2 <- auc(data.pred[!is.na(WnvPresent), WnvPresent], 
            data.pred[!is.na(WnvPresent), Pred]*data.pred[!is.na(WnvPresent),YearAvgActual]/data.pred[!is.na(WnvPresent),YearAvgPred])
err3 <- auc(data.pred[!is.na(WnvPresent), WnvPresent], 
            rank(data.pred[!is.na(WnvPresent), Pred])*data.pred[!is.na(WnvPresent),YearAvgActual]/data.pred[!is.na(WnvPresent),YearAvgPred])
data.pred[, YearAvgPred := NULL]

### closest by location
data.pred.closest.loc <- data.pred[,c("TrapUniqueId","Date","Species","Pred"),with=F]
setkey(data.pred.closest.loc, TrapUniqueId, Date, Species, Pred)
data.pred.closest.loc <- unique(data.pred.closest.loc)
setnames(data.pred.closest.loc, c("TrapUniqueId", "Pred"), c("TrapClosest", "PredClosest"))

### closest by time
data.pred.closest.time <- data.pred[,c("TrapUniqueId","Date","Species","Pred"),with=F]
setkey(data.pred.closest.time, TrapUniqueId, Date, Species, Pred)
data.pred.closest.time <- unique(data.pred.closest.time)
setnames(data.pred.closest.time, c("Date", "Pred"), c("DateClosest", "PredClosest"))

#data.pred[, PredLocTime := Pred]
#data.pred[, NumTrapsLocTime := 1]
data.pred[, PredLocTime := 0]
data.pred[, NumTrapsLocTime := 0]

### take an average by closest stations ###
for (i in 1:6) { #5
  setnames(data.pred, paste0("TrapClosest",i), "TrapClosest")
  data.pred <- merge(data.pred, data.pred.closest.loc, by=c("TrapClosest", "Date", "Species"), all.x=T)
  data.pred[!is.na(PredClosest), PredLocTime := PredLocTime + PredClosest]
  data.pred[!is.na(PredClosest), NumTrapsLocTime := NumTrapsLocTime + 1]
  data.pred[, PredClosest := NULL]
  setnames(data.pred, "TrapClosest", paste0("TrapClosest",i))
}

### take an average by closest dates ###
Nprevious <- 11
Nfuture <- 7
for (i in (-Nprevious):Nfuture) {
  data.pred.closest.time[, Date := as.character(as.Date(data.pred.closest.time$DateClosest)-i)]
  data.pred <- merge(data.pred, data.pred.closest.time[, c("TrapUniqueId","Date","Species","PredClosest"),with=F], by=c("TrapUniqueId", "Date", "Species"), all.x=T)
  data.pred[!is.na(PredClosest), PredLocTime := PredLocTime + PredClosest]
  data.pred[!is.na(PredClosest), NumTrapsLocTime := NumTrapsLocTime + 1]
  data.pred[, PredClosest := NULL]
}

data.pred[, PredLocTime := PredLocTime/NumTrapsLocTime]
err4 <- auc(data.pred[!is.na(WnvPresent), WnvPresent], 
            data.pred[!is.na(WnvPresent), PredLocTime])
data.avg <- data.pred[,list(YearAvgPred = mean(PredLocTime)), by="Year"]
data.pred <- merge(data.pred, data.avg, by="Year", all.x=T)
err5 <- auc(data.pred[!is.na(WnvPresent), WnvPresent], 
            data.pred[!is.na(WnvPresent), PredLocTime]*data.pred[!is.na(WnvPresent),YearAvgActual]/data.pred[!is.na(WnvPresent),YearAvgPred])
err6 <- auc(data.pred[!is.na(WnvPresent), WnvPresent], 
            rank(data.pred[!is.na(WnvPresent), PredLocTime])*data.pred[!is.na(WnvPresent),YearAvgActual]/data.pred[!is.na(WnvPresent),YearAvgPred])
if (TRUE) {
  data.rgf.01.pred.smth <- data.pred[,list(Id,PredLocTime,YearAvgActual,YearAvgPred)]
  data.rgf.01.pred.smth[, Pred := PredLocTime*YearAvgActual/YearAvgPred]
  data.rgf.01.pred.smth[, PredLocTime := NULL]
  data.rgf.01.pred.smth[, YearAvgActual := NULL]
  data.rgf.01.pred.smth[, YearAvgPred := NULL]
  minWnvPresent <- min(data.rgf.01.pred.smth[, Pred])
  maxWnvPresent <- max(data.rgf.01.pred.smth[, Pred])
  data.rgf.01.pred.smth[, Pred := (Pred - minWnvPresent)/(maxWnvPresent - minWnvPresent)]
  Store(data.rgf.01.pred.smth)
}
data.pred[, YearAvgPred := NULL]

# submission for LB
if (FALSE) {
  data.avg <- data.pred[,list(YearAvgPred = mean(PredLocTime)), by="Year"]
  data.pred <- merge(data.pred, data.avg, by="Year", all.x=T)
  data.submission <- data.pred[Year %in% c(2008,2010,2012,2014), list(Id, PredLocTime, YearAvgPred, YearAvgActual)]
  setnames(data.submission, "PredLocTime", "WnvPresent")
  data.submission[, WnvPresent := WnvPresent*YearAvgActual/YearAvgPred]
  minWnvPresent <- min(data.submission[, WnvPresent])
  maxWnvPresent <- max(data.submission[, WnvPresent])
  data.submission[, WnvPresent := (WnvPresent - minWnvPresent)/(maxWnvPresent - minWnvPresent)]
  setkey(data.submission, Id)
  write.table(as.data.frame(data.submission[,list(Id, WnvPresent)]),
              file = "../data/output-rgf/train.rgf.01.csv",
              quote = F, sep=",", row.names=F, col.names=T)
}

cat ("auc for augmented train:", err1, "\n")
cat ("auc for augmented train with multipliers:", err2, "\n")
cat ("auc for augmented train with multipliers by rank:", err3, "\n")
cat ("auc after smoothing by location and time:", err4, "\n")
cat ("auc after smoothing by location and time with multipliers:", err5, "\n")
cat ("auc after smoothing by location and time with multipliers by rank:", err6, "\n")
