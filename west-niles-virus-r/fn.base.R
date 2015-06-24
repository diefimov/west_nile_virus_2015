suppressWarnings(library("data.table"))
suppressWarnings(library("compiler"))

enableJIT(3) 
setCompilerOptions(suppressUndefined = T)
options(stringsAsFactors = FALSE)
options(max.print = 1000)
options(scipen=999)

path.wd <- getwd()
Sys.setenv("R_LOCAL_CACHE"="../data/output-r/.R_Cache")

suppressWarnings(library("SOAR"))
suppressWarnings(library("SparseM"))
suppressWarnings(library("Matrix"))
suppressWarnings(library("geosphere"))
Objects()

all.noexport <- c("fn_auc", "fn_opt_auc")

#############################################################
# tic toc
#############################################################
tic <- function(gcFirst = TRUE, type=c("elapsed", "user.self", "sys.self")) {
  type <- match.arg(type)
  assign(".type", type, envir=baseenv())
  if(gcFirst) gc(FALSE)
  tic <- proc.time()[type]         
  assign(".tic", tic, envir=baseenv())
  invisible(tic)
}

toc <- function() {
  type <- get(".type", envir=baseenv())
  toc <- proc.time()[type]
  tic <- get(".tic", envir=baseenv())
  print(toc - tic)
  invisible(toc)
}

##############################################################
## Registers parallel workers
##############################################################
fn.register.wk <- function(n.proc = NULL) {
  if (file.exists("../data/cluster.csv")) {
    cluster.conf <- read.csv(fn.in.file("cluster.csv"), 
                             stringsAsFactors = F,
                             comment.char = "#")
    n.proc <- NULL
    for (i in 1:nrow(cluster.conf)) {
      n.proc <- c(n.proc, 
                  rep(cluster.conf$host[i], 
                      cluster.conf$cores[i]))
    }
  }
  if (is.null(n.proc)) {
    n.proc = as.integer(Sys.getenv("NUMBER_OF_PROCESSORS"))
    if (is.na(n.proc)) {
      library(parallel)
      n.proc <-detectCores()
    }
    n.proc <- max(c(floor(n.proc*0.5), 1))
  }
  workers <- mget(".pworkers", envir=baseenv(), ifnotfound=list(NULL));
  if (!exists(".pworkers", envir=baseenv()) || length(workers$.pworkers) == 0) {
    
    library(doSNOW)
    library(foreach)
    workers<-suppressWarnings(makeSOCKcluster(n.proc));
    suppressWarnings(registerDoSNOW(workers))
    clusterSetupRNG(workers, seed=5478557)
    assign(".pworkers", workers, envir=baseenv());
    
    tic()
    cat("Workers start time: ", format(Sys.time(), 
                                       format = "%Y-%m-%d %H:%M:%S"), "\n")
  }
  invisible(workers);
}

##############################################################
## Kill parallel workers
##############################################################
fn.kill.wk <- function() {
  library("doSNOW")
  library("foreach")
  workers <- mget(".pworkers", envir=baseenv(), ifnotfound=list(NULL));
  if (exists(".pworkers", envir=baseenv()) && length(workers$.pworkers) != 0) {
    stopCluster(workers$.pworkers);
    assign(".pworkers", NULL, envir=baseenv());
    cat("Workers finish time: ", format(Sys.time(), 
                                        format = "%Y-%m-%d %H:%M:%S"), "\n")
    toc()
  }
  invisible(workers);
}

##############################################################
## init worker setting work dir and doing path redirect
##############################################################
fn.init.worker <- function(log = NULL, add.date = F) {
  setwd(path.wd)
  
  if (!is.null(log)) {
    date.str <- format(Sys.time(), format = "%Y-%m-%d_%H-%M-%S")
    
    if (add.date) {
      output.file <- fn.log.file(paste(log, "_",date.str,
                                       ".log", sep=""))
    } else {
      output.file <- fn.log.file(paste(log,".log", sep=""))
    }
    
    dir.create(dirname(output.file), showWarnings = F, recursive = T)
    output.file <- file(output.file, open = "wt")
    sink(output.file)
    sink(output.file, type = "message")
    
    cat("Start:", date.str, "\n")
  }
  
  tic()
}

##############################################################
## clean worker resources
##############################################################
fn.clean.worker <- function() {
  gc()
  
  try(toc(), silent=T)
  suppressWarnings(sink())
  suppressWarnings(sink(type = "message"))
}

##############################################################
## wait clean
##############################################################
fn.gc.wait <- function() {
  invisible(gc())
  Sys.sleep(1)
  invisible(gc())
  Sys.sleep(1)
  invisible(gc())
}

#############################################################
# log file path
#############################################################
fn.base.dir <- function(extra) {
  paste0(path.wd, "/../data/", extra)
}

#############################################################
# log file path
#############################################################
fn.log.file <- function(name) {
  fn.base.dir(paste0("log/", name))
}

#############################################################
# input file path
#############################################################
fn.in.file <- function(name) {
  fn.base.dir(paste0("input/", name))
}

#############################################################
# R output file path
#############################################################
fn.out.file <- function(name) {
  fn.base.dir(paste0("output-r/", name))
}

#############################################################
# python output file path
#############################################################
fn.py.file <- function(name) {
  fn.base.dir(paste0("output-py/", name))
}

#############################################################
# rgf output file path
#############################################################
fn.rgf.file <- function(name) {
  fn.base.dir(paste0("output-rgf/", name))
}


#############################################################
# submission file path
#############################################################
fn.submission.file <- function(name, suffix=".csv") {
  fn.base.dir(paste0("submission/", name, suffix))
}

#############################################################
# data file path
#############################################################
fn.data.file <- function(name) {
  fn.out.file(name)
}

#############################################################
# save data file
#############################################################
fn.save.data <- function(dt.name, envir = parent.frame()) {
  save(list = dt.name, 
       file = fn.data.file(paste0(dt.name, ".RData")), envir = envir)
}

#############################################################
# load saved file
#############################################################
fn.load.data <- function(dt.name, envir = parent.frame()) {
  load(fn.data.file(paste0(dt.name, ".RData")), envir = envir)
}

##############################################################
## auc
##############################################################
fn.auc <- function (actual, predicted) 
{
  library("Metrics")
  auc(actual=actual, predicted=predicted)
}

##############################################################
## print error
##############################################################

# data.all.pred <- data.all.out[, list(Id)]
# for (ix in 1:9) data.all.pred[, paste0("Pred_", ix) := 1/9, with=F]

fn.print.err <- function(pred, actual = data.all.out, 
                         pred.col = "Pred",
                         years.avg=F,
                         do.print=T,
                         digits=4) {
  
  pred.all <- merge(
    pred[, c("Id", pred.col), with=F], 
    actual[, c("Id", "WnvPresent", "Year"), with=F], by="Id")
  
  pred.all <- pred.all[!is.na(WnvPresent)]
  df.err <- data.frame(
    Size=nrow(pred.all),
    AUC = NA_real_
  )
  
  
  if (df.err$Size > 0) {
    if (years.avg) {
      df.err$AUC <- 0
      yrs <- sort(unique(pred.all$Year))
      n <- length(yrs)
      for (yr in yrs) {
        auc.cur <- fn.auc(actual = pred.all[Year==yr]$WnvPresent,
                      predicted = pred.all[Year==yr][[pred.col]])
        df.err$AUC <- df.err$AUC + auc.cur/n
        df.err[[paste(yr)]] <- auc.cur
      }
    } else {
      df.err$AUC <- fn.auc(actual = pred.all$WnvPresent,
                      predicted = pred.all[[pred.col]])
    }
  }
  
  if (do.print) {
    print(df.err)
  }
  
  invisible(df.err)
}

##############################################################
## cross val folds
##############################################################
fn.cv.folds <- function(data.all,  
                        type="sequential", seed = 34234,
                        col.id = "Year", col.out = "WnvPresent") {
  
  K = length(unique(data.all[!is.na(WnvPresent)]$Year))
  ids <- sort(unique(
    data.all[!is.na(data.all[[col.out]]), col.id, with=F][[1]]))
  n <- length(ids)
  library("cvTools")
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  data.cv.folds <- cvFolds(n, K = K, type = type)
  if (!is.null(seed)) {
    set.seed(Sys.time())
  }
  
  id.ord <- order(ids)
  data.cv.folds <- list(
    n = data.cv.folds$n,
    K = data.cv.folds$K,
    which = data.cv.folds$which[data.cv.folds$subsets][id.ord],
    ids = ids[id.ord],
    col.id = col.id,
    col.out = col.out
  )
  data.cv.folds
}
# debug(fn.cv.folds)

##############################################################
## cross val selection
##############################################################
fn.cv.which <- function(data.all, cv.data, k, type) {
  if (type == "tr") {
    sel.ids <- cv.data$ids[!cv.data$which %in% k]
  } else {
    if (k %in% 1:cv.data$K) {
      sel.ids <- cv.data$ids[cv.data$which %in% k]
    } else {
      sel.ids <- data.all[[cv.data$col.id]][
        !data.all[[cv.data$col.id]] %in% cv.data$ids]
    }
  }
  ix.sel <- (data.all[[cv.data$col.id]] %in% sel.ids)
  if (type == "tr") {
    ix.sel <- ix.sel & !is.na(data.all[[cv.data$col.out]])
  }
  ix.sel
}


##############################################################
## write submission file
##############################################################
fn.write.submission <- function(data.pred, file.name, 
                                data.test = data.all.out,
                                fix.species = F) {
  
  id.sub <- data.test[is.na(WnvPresent) & Id > 0, Id]
  data.sub <- data.pred[Id %in% id.sub, c("Id", "Pred"), with=F]
  setnames(data.sub, "Pred", "WnvPresent")
  
#   if (fix.species) {
#     data.sub <- merge(data.sub, data.all.species, by="Id")
#     data.sub[
#       !Species %chin% c("CULEX PIPIENS", "CULEX PIPIENS/RESTUANS", 
#                         "CULEX RESTUANS", "UNSPECIFIED CULEX"), 
#       WnvPresent := 0.0]
#   }
  
  data.sub <- data.sub[order(Id)]
  if (any(data.sub$WnvPresent > 1)) {
    data.sub[, WnvPresent := WnvPresent/max(WnvPresent)]
  }
  
  write.csv(
    data.sub[, list(Id, WnvPresent)],
    file = xzfile(paste0(fn.submission.file(file.name), ".7z")), 
    row.names = F, quote = F)
  invisible(data.sub)
}

#############################################################
# print rf importance
#############################################################
fn.rf.print.imp <- function(rf) {
 imp <- try(print(data.frame(importance=rf$importance[order(-rf$importance[,1]),]), 
      silent = T))
}

#############################################################
# removes overfit from similary table
#############################################################
fn.rm.sparse.similarity <- function(data.x, K = 20) {
  data.all.cv <- data.table(Id = colnames(data.x), Class = -1)
  data.cv.fold.rm <- fn.cv.folds(data.all.cv, K = 20)
  for (k.rm in 1:data.cv.fold.rm$K) {
    rm.idx <- which(fn.cv.which(data.all.cv, data.cv.fold.rm, k.rm, "test"))
    rm.col <- data.all.cv$Id[rm.idx]
    data.x[rm.idx, rm.col] <- 0
  }
  return (data.x)
}

#############################################################
# creates data.fold for models
#############################################################
fn.create.data.fold <- function(model.name, model.k) {
  data.fold <- list()
  data.fold$basename <- model.name
  data.fold$name <- paste0(data.fold$basename, "_k_", model.k)
  data.fold$logname <- paste(data.fold$basename, data.fold$name, sep="/")
  data.fold$fname <- fn.out.file(paste0(data.fold$basename, "/", 
                                        data.fold$name, ".RData"))
  data.fold
}


fn.load.data.fold <- function(model.name, model.k) {
  load(fn.out.file(paste0(model.name, "/", model.name,
                          "_k_", model.k, ".RData")),
       envir = parent.frame())
}

#############################################################
# saves data from models for later inspection
#############################################################
fn.save.data.fold <- function(data.fold) {
  dir.create(dirname(data.fold$fname), showWarnings = F, recursive = T)
  save(data.fold, file=data.fold$fname)
}

##############################################################
## creates data to tree methods
##############################################################
fn.data.2.tree <- function(data.dt, cols.in.dt=colnames(data.dt)) {
  data.tree.dt <- copy(data.dt)
  cols.char <- sapply(data.tree.dt[,cols.in.dt,with=F], class)
  cols.char <- sapply(cols.char, 
                      function(x) any(x %chin% c("character", "factor")))
  cols.char <- names(cols.char)[cols.char]
  for (col.nam in cols.char) {
    cat("Converting ", col.nam, " to ordinals ... \n")
    setnames(data.tree.dt, col.nam, "col_cur")
    if (is.factor(data.tree.dt$col_cur)) {
      dt.unique <- data.table(id_val=levels(data.tree.dt$col_cur))
      data.tree.dt[, col_cur := as.character(col_cur)]
    } else {
      dt.unique <- data.table(id_val=unique(data.tree.dt$col_cur))
    }
    dt.unique[, map_val := as.numeric(1:.N)]
    setkeyv(dt.unique, "id_val")
    data.tree.dt[, col_cur := dt.unique[J(col_cur)]$map_val ]
    setnames(data.tree.dt, "col_cur", col.nam)
  }
  
  for (col.nam in cols.in.dt) {
    sel.na <- is.na(data.tree.dt[[col.nam]])
    if (any(sel.na)) {
      setnames(data.tree.dt, col.nam, "col_cur")
      na.val = min(data.tree.dt$col_cur[!sel.na])-10
      na.val = min(c(-1000, na.val))
      cat("Setting ", col.nam, " Nas to value ", na.val, "... \n")
      data.tree.dt[sel.na, col_cur := na.val ]
      setnames(data.tree.dt, "col_cur", col.nam)
    }
  }
  return (data.tree.dt)
}

##############################################################
## load ensemble data
##############################################################
fn.load.ens <- function(ens.cols, transf=identity,
                        print.err = T) {
  
  data.ens.pred <- NULL
  
  for (pred.nam in ens.cols) {
    data.cur <- get(pred.nam)
    data.cur <- transf(data.cur)
    if (is.null(data.ens.pred)) {
      data.ens.pred <- data.table(data.cur)
    } else {
      data.ens.pred <- merge(data.ens.pred, data.cur, by="Id")
    }
    if (print.err) {
      cat(pred.nam, ":\n", sep="")
      fn.print.err(data.ens.pred)
    }
    pred.nam <- gsub("^data\\.", "", pred.nam)
    pred.nam <- gsub("\\.smth$", "", pred.nam)
    pred.nam <- gsub("\\.pred$", "", pred.nam)
    setnames(data.ens.pred, "Pred", pred.nam)
  }
  data.ens.pred[order(Id)]
}

##############################################################
## print correlations
##############################################################
fn.ens.cor <- function(data.ens.pred, first.col = NULL, do.print=T) {
  data.ens.cols <- setdiff(colnames(data.ens.pred), c("Id", "Year"))
  data.ens.cor <- data.frame(t(combn(length(data.ens.cols), 2)))
  colnames(data.ens.cor) <- c("col1", "col2")
  data.ens.cor$linear_cor <- NA_real_ 
  data.ens.cor$rank_cor <- NA_real_ 
  data.ens.cor$col1 <- data.ens.cols[data.ens.cor$col1]
  data.ens.cor$col2 <- data.ens.cols[data.ens.cor$col2]
  if (!is.null(first.col)) {
    fn.sel.col <- function(cols) {
      as.logical(apply(sapply(first.col,  function(x) cols %like% x), 1, max))
    }
    data.ens.cor <- data.ens.cor[
      fn.sel.col(data.ens.cor$col1) | fn.sel.col(data.ens.cor$col2),]
  }
  for (ix in 1:nrow(data.ens.cor)) {
    p1 <- data.ens.pred[[data.ens.cor$col1[ix]]]
    p2 <- data.ens.pred[[data.ens.cor$col2[ix]]]
    pred.ix <- which((p1 != 0 | p2 != 0) & data.ens.pred$Id > 0)
    p1 <- p1[pred.ix]
    p2 <- p2[pred.ix]
    data.ens.cor$linear_cor[ix] <- cor(p1, p2)
     data.ens.cor$rank_cor[ix] <- cor(rank(p1), rank(p2))
  }
  if (do.print) {
    print(data.ens.cor)
  }
  invisible(data.ens.cor)
}

##############################################################
## index filtering
##############################################################
fn.filter.ix.species.in <- function(ix, data.all) {
  ix[data.all$Species[ix] != 'CULEX ZOTHER']
}

fn.filter.ix.species.out <- function(ix, data.all) {
  ix[data.all$Species[ix] == 'CULEX ZOTHER']
}

fn.filter.ix.tr.dups <- function(ix, data.all) {
  ix[data.all$Id[ix] %in% data.tr.out.max$Id]
}

##############################################################
## filter cols by unique values
##############################################################
fn.filter.cols <- function(data.all, tr.ix) {
  cols.vals <- apply(data.all[tr.ix, ], 2, function (x) length(unique(x)))
  cols.vals <- names(cols.vals)[cols.vals > 1]
  if (is.data.table(data.all)) {
    data.all[,cols.vals, with=F]
  } else {
    data.all[,cols.vals]
  }
}

##############################################################
## calculates normalized pred average
##############################################################
fn.calc.pred.mean <- function(data.pred, rm.zeroes=T) {
  sel.ix <- rep(T, nrow(data.pred))
  if (rm.zeroes) {
    sel.ix <- data.pred$Pred > 0
  }
  data.pred.year <- merge(data.pred[sel.ix], data.all.out[, list(Id, Year)], by="Id")
  data.pred.year <- data.pred.year[
    ,list(Mean=mean(Pred)),by="Year"][order(Year + (Year %% 2 == 0)*100)]
  for (ix in 0:1) {
    data.pred.year[Year %% 2 == ix, Mean := Mean/max(Mean)]
  }
  data.pred.year
}

fn.calc.pred.month.mean <- function(data.pred, rm.zeroes=T) {
  sel.ix <- rep(T, nrow(data.pred))
  if (rm.zeroes) {
    sel.ix <- data.pred$Pred > 0
  }
  data.pred.year <- merge(
    data.pred[sel.ix], 
    fn.add.date.fields(data.all.out)[, list(Id, Year, Month)],
    by="Id")
  data.pred.year <- data.pred.year[
    ,list(Mean=mean(Pred)),by=c("Year", "Month")][order(Year + (Year %% 2 == 0)*100, Month)]
  for (ix in 0:1) {
    data.pred.year[Year %% 2 == ix, Mean := Mean/max(Mean)]
  }
  data.pred.year
}

##############################################################
## applies multiplier to predictions
##############################################################
fn.apply.mult <- function(data.pred, data.mult=data.mult.01, 
                          rm.zeroes=T) {
  
  if (!is.data.table(data.mult)) {
    for (ix in 1:length(data.mult)) {
      data.pred = fn.apply.mult(data.pred, data.mult=data.mult[[ix]],
                                rm.zeroes=rm.zeroes)
    }
    return(data.pred)
  }
  
  data.pred.new <- merge(data.pred,
                         data.all.trap[, list(Id, Year, Month, WeekYear)],
                         by="Id")
  sel.ix <- rep(T, nrow(data.pred.new))
  if (rm.zeroes) {
    sel.ix <- data.pred.new$Pred > 0
  }
  col.by <- intersect(c("Year", "Month", "WeekYear"), colnames(data.mult))
  data.pred.new.vals <- data.pred.new[
    sel.ix, list(PredAvg = mean(Pred)), by=col.by][
    order(Year)]
  for (ix in 0:1) {
    data.pred.new.vals[Year %% 2 == ix, PredAvg := PredAvg/max(PredAvg)]
  }
  data.pred.new.vals <- merge(data.pred.new.vals, data.mult, by=col.by)
  data.pred.new.vals[, Factor := PredRatio/PredAvg]
  data.pred.new <- merge(data.pred.new, data.pred.new.vals, by=col.by)
  data.pred.new[,Pred:=Pred*Factor]
  data.pred.new <- data.pred.new[order(Id)]
  data.pred.new <- data.pred.new[, colnames(data.pred), with=F]
  data.pred.new
}


##############################################################
## likelihood - needs fix
##############################################################
#   fn.calc.likelihood <- function(data.all, by, tr.idx, test.idx, smooth=10) {
#     tr.years <- sort(unique(data.all$Year[tr.idx]))
#     test.years <- sort(unique(data.all$Year[test.idx]))
#     gavg <- mean(data.all$WnvPresent[tr.idx])
#     data.calc.year <- NULL
#     for (cur.year in c(tr.years, test.years)) {
#       data.calc.year.cur <- data.all[tr.idx,][Year != cur.year]
#       data.calc.year.cur <- data.calc.year.cur[
#         ,list(WnvPresent=sum(WnvPresent), n=.N),by=by]
#       data.calc.year.cur[, Val := (WnvPresent + smooth*gavg)/(n+smooth)]
#       data.calc.year.cur[, Year := cur.year]
#       data.calc.year.cur <- data.calc.year.cur[, c("Year", by, "Val"), with=F]
#       data.calc.year <- rbind(data.calc.year, data.calc.year.cur)
#     }
#     setkeyv(data.calc.year, c("Year", by))
#     data.all.key <- data.all[c(tr.idx, test.idx), c("Year", by), with=F]
#     prob.val <- rep(NA_real_, nrow(data.all))
#     prova.val.sel <- data.calc.year[data.all.key]$Val
#     #prova.val.sel[is.na(prova.val.sel)] <- gavg
#     prob.val[c(tr.idx, test.idx)] <- prova.val.sel
#     prob.val
#   }
#   
#   fn.add.prob <- function(data.all, col.nam, ...) {
#     col.prob <- paste0(paste(col.nam, collapse=""), "Prob")
#     data.all[, col.prob := fn.calc.likelihood(
#       data.all.cur, by=col.nam, tr.idx=data.fold$tr.idx.in,
#       test.idx = data.fold$test.idx.in, ...), with=F]
#     data.all[is.na(get(col.prob)), col.prob := -1000, with=F]
#   }
#   fn.add.prob("Trap")

##############################################################
## add date fields
##############################################################
fn.add.date.fields <- function(data.all) {
  
  if (!"Date" %in% colnames(data.all)) {
    data.all <- merge(
      data.all,
      data.all.out[, list(Id, Date)],
      by="Id"
    )
  }
  data.all[, Year := as.numeric(format(as.Date(Date, format="%Y-%m-%d"), 
                                            format = "%Y"))]
  data.all[, Month := as.numeric(format(as.Date(Date, format="%Y-%m-%d"), 
                                             format = "%m"))]
  data.all[, WeekYear := as.numeric(format(as.Date(Date, format="%Y-%m-%d"), 
                                                format = "%V"))]
  data.all[, DayYear := as.numeric(format(as.Date(Date, format="%Y-%m-%d"), 
                                               format = "%j"))]
  invisible(data.all)
}

##############################################################
## moving average
##############################################################
fn.ma <- function(x, n=1) {
    sapply(1:length(x), function (ix) {
    ix <- seq.int(ix, ix-n+1)
    ix <- ix[ix > 0]
    mean(x[ix], na.rm=T)
  })  
}

##############################################################
## geometric average
##############################################################
fn.gm.mean = function(x, na.rm=TRUE, zero.propagate = FALSE){
  if(any(x < 0, na.rm = TRUE)){
    return(NaN)
  }
  if(zero.propagate){
    if(any(x == 0, na.rm = TRUE)){
      return(0)
    }
    exp(mean(log(x), na.rm = na.rm))
  } else {
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
}

##############################################################
## geometric average
##############################################################
fn.gma <- function(x, n=1) {
  sapply(1:length(x), function (ix) {
    ix <- seq.int(ix, ix-n+1)
    ix <- ix[ix > 0 & ix <= length(x)]
    fn.gm.mean(x[ix], na.rm=T)
  })    
}

##############################################################
## weather plot function
##############################################################
fn.plot.weather <- function(feat) {
  data.weather.cur <- fn.add.date.fields(copy(data.weather))
  par(mfrow=c(4,2))
  for (year in 2007:2014) {
    data.plot <- data.weather.cur[Year == year]
    model.loess <- loess(
      as.formula(paste(feat, "DayYear", sep="~")), 
      data=data.plot, span = 0.4)
    pred <- predict(model.loess, data.plot)
    plot(y=pred, x=data.plot$DayYear,
         type="l", ylab=feat, xlab="Day",
         ylim=c(min(data.weather.cur[[feat]], na.rm=T), 
                max(data.weather.cur[[feat]], na.rm=T)), 
         xlim=c(min(data.weather.cur$DayYear, na.rm=T), 
                max(data.weather.cur$DayYear, na.rm=T)),
         main=paste(year),
         col = "blue")
    model.loess <- loess(
      as.formula(paste(feat, "DayYear", sep="~")), 
      data=data.plot, span = 0.1)
    pred <- predict(model.loess, data.plot)
    lines(y=pred, x=data.plot$DayYear, col="red")
    lines(y=data.plot[[feat]], x=data.plot$DayYear)
  }
}

##############################################################
## year infection plot function
##############################################################
fn.plot.year.evolution <- function(data.pred.names) {
  data.out.cur <- fn.add.date.fields(copy(data.all.out))
  for (data.pred.name in data.pred.names)  {
    data.pred <- copy(get(data.pred.name))[Pred > 0]
    setnames(data.pred, "Pred", data.pred.name)
    data.out.cur <- merge(data.out.cur, data.pred, by="Id")
  }

  par(mfrow=c(4,2))
  for (year in 2007:2014) {
    data.plot <- data.out.cur[
      Year == year,
      list(
        WnvPresent = as.numeric(sum(WnvPresent, na.rm=T)),
        NumMosquitos = as.numeric(sum(NumMosquitos, na.rm=T)),
        InfectRate = as.numeric(sum(NumMosquitos[WnvPresent %in% 1], na.rm=T)/
                                  sum(NumMosquitos, na.rm=T))
      ),
      by=c("WeekYear")]
    data.plot.mult <- data.mult.weekly.01[Year == year]
    data.plot.mult[, PredRatio := PredRatio/max(PredRatio)]
    data.plot <- merge(data.plot, data.plot.mult,
                       all.x=T, by=c("WeekYear"))
    data.plot[is.na(PredRatio), PredRatio := 0]
    data.plot[!is.na(WnvPresent), 
              WnvPresent := WnvPresent/max(WnvPresent)]
    data.plot[!is.na(NumMosquitos), 
              NumMosquitos := NumMosquitos/max(NumMosquitos)]
    data.plot[!is.na(InfectRate), 
              InfectRate := InfectRate/max(InfectRate)]
    for (data.pred.name in data.pred.names)  {
      data.pred <- copy(data.out.cur)[Year == year]
      setnames(data.pred, data.pred.name, "Pred")
      data.plot <- merge(data.plot, 
                         data.pred[,list(Pred = mean(Pred)),by="WeekYear"],
                         by="WeekYear")
      data.plot[, Pred := Pred/max(Pred)]
      setnames(data.plot, "Pred", data.pred.name)
    } 
    data.plot <- data.plot[order(WeekYear)]

    plot(y=data.plot[[data.pred.names[1]]], x=data.plot$WeekYear,
         type="l", ylab="Norm values", xlab="Week",
         ylim=c(0, 1), 
         xlim=c(min(data.out.cur$WeekYear-5, na.rm=T), 
                max(data.out.cur$WeekYear, na.rm=T)),
         main=paste(year),
         lty=3,
         col = "blue")
    legends <- list(txt=data.pred.names[1],
                    col="blue",
                    lty=3)
    cols.sel <- c("brown", "blueviolet", "bisque3", "orange3", "darkgreen",
                  "gold", "darkviolet", "deeppink", "gray")
    for (ix in 2:length(data.pred.names))  {
      lines(y=data.plot[[data.pred.names[ix]]], 
            x=data.plot$WeekYear, col=cols.sel[ix-1],
            lty=3)
      legends$txt <- c(legends$txt, data.pred.names[ix])
      legends$col <- c(legends$col, cols.sel[ix-1])
      legends$lty <- c(legends$lty, 3)
    }
    if (any(!is.na(data.plot$WnvPresent))) {
      lines(y=data.plot$WnvPresent, x=data.plot$WeekYear, col="green")
      legends$txt <- c(legends$txt, "Actual")
      legends$col <- c(legends$col, "green")
      legends$lty <- c(legends$lty, 1)
      
    }
    if (any(!is.na(data.plot$WnvPresent))) {
      lines(y=data.plot$NumMosquitos, x=data.plot$WeekYear, col="black", lty=2)
      legends$txt <- c(legends$txt, "Num. Mosq.")
      legends$col <- c(legends$col, "black")
      legends$lty <- c(legends$lty, 2)
    }
    if (any(!is.na(data.plot$PredRatio))) {
      lines(y=data.plot$PredRatio, x=data.plot$WeekYear, col="red", lty=2)
      legends$txt <- c(legends$txt, "Pred Ratio")
      legends$col <- c(legends$col, "red")
      legends$lty <- c(legends$lty, 2)
    }
    legend("topleft", legend=legends$txt, col=legends$col, lty=legends$lty,
           cex=0.5)
  }
}

#############################################################
# fixing zero value predictions
#############################################################
fn.fix.zero.pred <- function(data.pred) {
  data.pred.cur <- merge(data.pred, 
                         data.all.trap[, list(Id, Species)], 
                         by="Id")
  data.pred.cur[Species == "CULEX ZOTHER", Pred := 0]
  data.pred.cur[, Species := NULL]
  data.pred.cur
}

#############################################################
# coordinates distances
#############################################################
fn.dist.coord <- function(tlat, tlong, slat, slong) {
  library("geosphere")
  n <- length(tlat)
  if (length(slat) == 1) {
    slat <- rep(slat, n)
    slong <- rep(slong, n)
  }
  fn.harv.apply <- function(ix) {
    distHaversine(c(tlat[ix],tlong[ix]),c(slat[ix],slong[ix]))/1000
  }
  mapply(fn.harv.apply,ix=1:n)
}

# #############################################################
# # smoothing predictions - linear regression
# #############################################################
# fn.smooth.lm.pred <- function(data.pred, wnd.n=1, wnd.days=15) {
#   
#   data.pred.cur <- merge(fn.apply.mult(data.pred),
#                          data.all.out, by="Id")
#   data.pred.cur <- merge(data.pred.cur,
#                          data.all.trap[, list(Id, Species, TrapFixed)], by="Id")
#   data.pred.cur <- fn.add.date.fields(data.pred.cur)
#   data.pred.cur.ids <- data.pred.cur[, list(Id, Date, TrapFixed, Species)]
#   
#   data.pred.key <- c("Date", "TrapFixed", "Species")
#   data.pred.cur <- data.pred.cur[
#     ,list(
#       WnvPresent = as.numeric(mean(WnvPresent)),
#       Pred = mean(Pred),
#       Year = unique(Year)
#     )
#     ,by=data.pred.key
#     ]
#   setkeyv(data.pred.cur, data.pred.key)
#   data.pred.dates <- as.Date(data.pred.cur$Date, format="%Y-%m-%d")
#   
#   data.wnd.mat.prev <- Matrix(data=NA_real_, 
#                      nrow=nrow(data.pred.cur), 
#                      ncol=wnd.days)
#   data.wnd.mat.next <- Matrix(data=NA_real_, 
#                      nrow=nrow(data.pred.cur), 
#                      ncol=wnd.days)
#   for (ix in 1:wnd.days) {
#     pred.key.dt <- data.pred.cur[,data.pred.key,with=F]
#     
#     pred.key.dt[, Date := format(data.pred.dates-ix, format = "%Y-%m-%d")]
#     data.wnd.mat.prev[,ix] <- data.pred.cur[pred.key.dt]$Pred
#     
#     pred.key.dt[, Date := format(data.pred.dates+ix, format = "%Y-%m-%d")]
#     data.wnd.mat.next[,ix] <- data.pred.cur[pred.key.dt]$Pred
#   }
# 
#   pred.fill.prev <- data.pred.cur$Pred
#   pred.fill.next <- data.pred.cur$Pred
#   for (ix in 1:wnd.n) {
#     fn.calc.wnd <- function(x) x[c(which(!is.na(x)), rep(1,wnd.n))][ix]
#     col.nam <- paste0("PredPrev", ix)
#     data.pred.cur[
#     , col.nam := apply(data.wnd.mat.prev, 1, fn.calc.wnd) , with=F]
#     ix.na <- is.na(data.pred.cur[[col.nam]])
#     data.pred.cur[ix.na, col.nam:= pred.fill.prev[ix.na], with=F]
#     pred.fill.prev <- data.pred.cur[[col.nam]]
#     col.nam <- paste0("PredNext", ix)
#     data.pred.cur[
#     , col.nam := apply(data.wnd.mat.next, 1, fn.calc.wnd) , with=F]
#     ix.na <- is.na(data.pred.cur[[col.nam]])
#     data.pred.cur[ix.na, col.nam:= pred.fill.next[ix.na], with=F]
#     pred.fill.next <- data.pred.cur[[col.nam]]
#   }
#   
#   cols.feat <- c("Pred", 
#                  paste0("PredPrev", 1:wnd.n), 
#                  paste0("PredNext", 1:wnd.n))
#   data.pred.cur.sum <- rowSums(data.pred.cur[,cols.feat,with=F])
#   data.pred.cur.unique <- apply(data.pred.cur[,cols.feat,with=F], 1,
#                                 function (x) length(unique(x)))
#   
#   tr.years <- seq(2007, 2013, 2)
#   test.years <- seq(2008, 2014, 2)
#   
#   pred.smooth <- data.pred.cur[, data.pred.key, with=F]
#   pred.smooth[, Pred := 0.0]
#   
#   fn.nnls <- function(data.tr, data.test, do.print=F) {
#     library(nnls)
#     model.smooth <- nnls(as.matrix(data.tr[,cols.feat,with=F]), 
#                       data.tr$WnvPresent)
#     if (do.print) {
#       print(model.smooth)
#     }
#     model.smooth.coef <- coef(model.smooth)
#     model.smooth.coef <- model.smooth.coef/sum(model.smooth.coef)
#     as.vector(model.smooth.coef%*%t(as.matrix(data.test[,cols.feat,with=F])))
#   }
#   for (yr in tr.years) {
#     ix.tr <- which(with(data.pred.cur, Year != yr 
#                         & !is.na(WnvPresent) 
#                         & data.pred.cur.sum != 0
#                         & data.pred.cur.unique > 1))
#     ix.test <- which(with(data.pred.cur, Year == yr))
#     pred.smooth[ix.test, "Pred"] <- fn.nnls(data.pred.cur[ix.tr],
#                                             data.pred.cur[ix.test])
#   }
#   ix.tr <- which(with(data.pred.cur, Year %in% tr.years 
#                       & !is.na(WnvPresent) 
#                       & data.pred.cur.sum != 0
#                       & data.pred.cur.unique > 1))
#   ix.test <- which(with(data.pred.cur, Year %in% test.years))
#   pred.smooth[ix.test, "Pred"] <- fn.nnls(data.pred.cur[ix.tr],
#                                           data.pred.cur[ix.test],
#                                           do.print=T)
#   pred.smooth <- merge(data.pred.cur.ids, pred.smooth, all.x=T,
#                        by=data.pred.key)[order(Id), list(Id, Pred)]
#   
#   pred.smooth
# }

#############################################################
# smoothing predictions - moving avg
#############################################################
fn.smooth.time.ma.pred <- function(data.pred, n.prev, n.nxt, 
                                   pred.w, ma.func) {
  
  
  
  data.pred.cur <- merge(data.pred,
                         data.all.out[, list(Id, Date)], by="Id")
  data.pred.cur <- merge(data.pred.cur,
                         data.all.trap[, list(Id, Species, TrapFixed)], by="Id")
  data.pred.cur <- fn.add.date.fields(data.pred.cur)
  data.pred.cur.ids <- data.pred.cur[, list(Id, Date, TrapFixed, Species)]
  
  data.pred.key <- c("Date", "TrapFixed", "Species")
  data.pred.cur <- data.pred.cur[order(Date)][
    ,list(
      Pred = mean(Pred),
      DayYear = unique(DayYear),
      Year = unique(Year)
    )
    ,by=data.pred.key
    ][order(Date)]
  
  fn.ma.smooth <- function(pred) {
    if (length(unique(pred)) == 1) {
      return (pred)
    }
    n.nxt.ma <- n.nxt+1
    n.prev.ma <- n.prev+1
    return ( (
      ma.func(pred, n=n.prev.ma)*n.prev.ma 
      + rev(ma.func(rev(pred), n=n.nxt.ma))*n.nxt.ma 
      - 2*pred
      + pred.w*pred
      )/(n.nxt.ma+n.prev.ma+pred.w-2) )
  }
  
  data.pred.cur <- data.pred.cur[
    ,list(
      Date = Date,
      Pred = fn.ma.smooth(Pred)
    ), by=c("Year", "TrapFixed", "Species")]

  pred.smooth <- merge(data.pred.cur.ids, data.pred.cur, all.x=T,
                       by=data.pred.key)[order(Id), list(Id, Pred)]
  
  pred.smooth
}

#############################################################
# smoothing predictions by location - linear regression
#############################################################
fn.smooth.loc.ma.pred <- function(data.pred, n, pred.w, ma.func=mean) {
  
  data.pred.cur <- merge(data.pred,
                         data.all.out[, list(Id, WnvPresent, Date)], by="Id")
  data.pred.cur <- merge(data.pred.cur,
                         data.all.trap[, list(Id, Species, TrapFixed)], by="Id")
  data.pred.cur <- fn.add.date.fields(data.pred.cur)
  data.pred.cur.ids <- data.pred.cur[, list(Id, Date, TrapFixed, Species)]
  
  data.pred.key <- c("Date", "TrapFixed", "Species")
  data.pred.cur <- data.pred.cur[
    ,list(
      WnvPresent = as.numeric(mean(WnvPresent)),
      Pred = mean(Pred),
      Year = unique(Year)
    )
    ,by=data.pred.key
    ]
  setkeyv(data.pred.cur, data.pred.key)
  
  data.trap.dist.near <- data.trap.dist[
    ,list(
      TrapFixedNear = head(TrapFixed2[order(Tdist)], n=n)
    ), by="TrapFixed1"]
  
  for (trap.nam in sort(unique(data.pred.cur$TrapFixed))) {
    trap.near <- data.trap.dist.near[J(trap.nam)]$TrapFixedNear
    trap.ix <- which(data.pred.cur$TrapFixed==trap.nam)
    for (ix in 1:n) {
      pred.key.dt <- data.pred.cur[trap.ix,data.pred.key,with=F]
      pred.key.dt[, TrapFixed := trap.near[ix]]
      pred.vals <- data.pred.cur[pred.key.dt]$Pred
      data.pred.cur[trap.ix, paste0("PredTN", ix) := pred.vals, with=F]
    }
  }
  
  cols.feat <- paste0("PredTN", 1:n)
  fn.ma.smooth <- function(pred) {
    return ( ma.func( c(rep(pred[1], pred.w), pred[-1]) ) )
  }
  
  cols.feat <- c("Pred", cols.feat)
  pred.smooth <- data.pred.cur[, c(data.pred.key, cols.feat), with=F]
  pred.smooth[
    , Pred := apply(data.pred.cur[, cols.feat, with=F], 1, fn.ma.smooth)]
  
  pred.smooth <- pred.smooth[, c(data.pred.key, "Pred"), with=F]
  pred.smooth <- merge(data.pred.cur.ids, pred.smooth, all.x=T,
                       by=data.pred.key)[order(Id), list(Id, Pred)]
  pred.smooth
}

#############################################################
# time and location smoothing
#############################################################
fn.smooth.pred <- function(data.pred, trap.n=4, 
                           time.n.prev=3, time.n.nxt=time.n.prev,
                           pred.w=4, time.ma.fn=fn.ma,
                           data.mult = data.mult.01) {
  
  data.pred.cur <- copy(data.pred)
  data.pred.cur <- fn.smooth.loc.ma.pred(data.pred.cur, n=trap.n,
                                         pred.w=pred.w)
  data.pred.cur <- fn.smooth.time.ma.pred(data.pred.cur, 
                                          n.prev=time.n.prev, n.nxt=time.n.nxt, 
                                          pred.w=pred.w,
                                          ma.func=time.ma.fn)
  if (!is.null(data.mult)) {
    data.pred.cur <- fn.apply.mult(data.pred.cur, data.mult = data.mult)
  }
  data.pred.cur
}

#############################################################
# write rgf files
#############################################################
fn.rgf.write <- function(data, file) {
  write.table(
    data,
    file=file,
    row.names = F, quote = F, na = "", sep = " ",
    append = F, col.names = F
  )
}

#############################################################
# rgf file names
#############################################################
fn.rgf.tr.file <- function(data.fold, suffix) {
  paste0(data.fold$writedir, "/", data.fold$name,"_tr.", suffix)
}

fn.rgf.test.file <- function(data.fold, suffix) {
  paste0(data.fold$writedir, "/", data.fold$name,"_test.", suffix)
}

#############################################################
# rgf train config file
#############################################################
fn.rgf.write.cfg <- function(data.fold, params) {
  # train
  lines <- c(
    paste0("train_x_fn=", data.fold$tr.x.file),
    paste0("train_y_fn=", data.fold$tr.y.file),
    paste0("model_fn_prefix=", data.fold$tr.model.file),
    paste0("test_x_fn=", data.fold$test.x.file),
    #paste0("prediction_fn=", data.fold$test.pred.file),
    paste0("SaveLastModelOnly")
  )
  for (nam in names(params)) {
    line <- nam
    if (!is.character(params[[nam]]) ||  params[[nam]] != "") {
      line = paste(line,params[[nam]], sep="=")
    }
    lines <- c(lines, line)
  }
  writeLines(lines, con = paste0(data.fold$tr.cfg.file, ".inp"))
}

#############################################################
# convert log odds to probabilities
#############################################################
fn.from.logit <- function(x) {
  1/(1+exp(-x))
}

fn.to.logit <- function(x) {
  (x-0.5)*2
}

#############################################################
# nnls regression
#############################################################
fn.nnls.train <- function(x, y, intercept=T, coef.norm=F) {
  library(nnls)
  coef.names <- colnames(x)
  x <- as.matrix(x)
  if (intercept) {
    x <- cbind(Matrix(data=1, nrow=nrow(x)), x)
    coef.names <- c("Intercept", coef.names)
  }
  model <- nnls(x, y)
  model.coefs <- coef(model)
  if (coef.norm) {
    model.coefs <- model.coefs/sum(model.coefs)
  }
  model <- (list(model=model, intercept=intercept, 
               coef.names=coef.names, model.coefs=model.coefs))
  names(model$model.coefs) <- coef.names
  model
}

fn.nnls.predict <- function(model, x) {
  library(nnls)
  x <- as.matrix(x)
  if (model$intercept) {
    x <- cbind(Matrix(data=1, nrow=nrow(x)), x)
  }
  as.vector(model$model.coefs%*%t(x))
}


#############################################################
# opt auc regression
#############################################################
fn.opt.pred <- function(pars, x) {
  if ("par" %in% names(pars)) {
   pars <- pars$par 
  }
	pars.m <- matrix(rep(pars,each=nrow(x)),nrow=nrow(x))
	rowSums(data.x*pars.m)
}

fn.opt <- function(x, y) {
  return (function(pars) {
    #-auc(y, fn.opt.pred(pars, c))
    -fn_opt_auc(y, x, pars)
  })
}

fn.opt.run <- function(x, y, pars=rep(1/ncol(x),ncol(x)), norm.pars=T) {
  res <- optim(pars, fn.opt(x, y), 
               control = list(trace = T))
  if (norm.pars) {
    res$par <- res$par/sum(res$par)
  }
  res
}
