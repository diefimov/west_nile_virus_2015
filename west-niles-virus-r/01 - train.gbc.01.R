source("data.build.gbc.R")
#############################################################
# save csv to disk data
#############################################################
rm(list = ls())
source("fn.base.R")

fn.register.wk(1)
tmp <- foreach(
  k=0:data.cv.folds$K,
  .combine=rbind) %dopar% {
  
  source("fn.base.R")
  data.fold <- fn.create.data.fold("gbc_01", k)
  data.fold$writedir <- fn.py.file(data.fold$basename)
  dir.create(data.fold$writedir, showWarnings = F, recursive = T)
  
  data.all.cur <- merge(data.all.out[, list(Id, WnvPresent)], 
                        data.all.feat.tree, by="Id")
  data.all.cur <- merge(data.all.cur, 
                        data.all.trap[, list(Id, Species)], 
                        by="Id", suffixes = c("Tree", ""))
  data.all.cur <- data.all.cur[order(Id)]
  
  col.out <- "WnvPresent"
  cols.weather <- c('TmaxSt1', 'TminSt1', 
                    'TavgSt1', 'DepartSt1', 'DewPointSt1', 'HeatSt1', 'CoolSt1', 
                    'PrecipTotalSt1', 'SnowFallSt1', 
                    'ResultDirSt1', 'TmaxSt2',  'TminSt2', 'TavgSt2', 
                    'DewPointSt2', 'PrecipTotalSt2', 'ResultDirSt2')
  
  cols.weather <- c(
    cols.weather
  )
  cols.weather <- c(paste0(cols.weather, "Smth02"))

  cols.in <- c('SpeciesTree', 'Block', 'Trap', 'Latitude', 'Longitude', 
               'AddressAccuracy', 'Month', 'TrapCount', 'TrapCountPrevAge', 
               'TrapCountPrev', 'SunriseSt1', 'SunsetSt1', "Year",
               cols.weather)

  cols.in <- intersect(cols.in, colnames(data.all.cur))
  cols.write <- c(col.out, cols.in)
    
  data.fold$tr.idx <- which(fn.cv.which(data.all.cur, data.cv.folds, k, "tr"))
  data.fold$tr.idx.in <- 
    fn.filter.ix.species.in(data.fold$tr.idx, data.all.cur)

  data.fold$tr.file <- paste0(data.fold$writedir, "/", data.fold$name,"_tr.csv")
  write.csv(
    data.all.cur[data.fold$tr.idx.in][, cols.write, with=F],
    file=data.fold$tr.file, row.names = F
  )

  
  data.fold$test.idx <- which(fn.cv.which(
    data.all.cur, data.cv.folds, k, "test"))
  data.fold$test.idx.in <- 
    fn.filter.ix.species.in(data.fold$test.idx, data.all.cur)
  data.fold$test.idx.out <- 
    fn.filter.ix.species.out(data.fold$test.idx, data.all.cur)
  
  data.fold$test.ids.in <- data.all.cur[data.fold$test.idx.in, Id]
  data.fold$test.ids.out <- data.all.cur[data.fold$test.idx.out, Id]
  data.fold$test.file <- paste0(data.fold$writedir, "/", 
                                data.fold$name,"_test.csv")
  data.fold$test.pred.file <- paste0(data.fold$writedir, "/", 
                                     data.fold$name,"_test_pred.csv")
  write.csv(
    data.all.cur[data.fold$test.idx.in, cols.write, with=F],
    file=data.fold$test.file, row.names = F
  )
  
  fn.save.data.fold(data.fold)
  
  NULL
  
}
fn.kill.wk()

#############################################################
# train using scikit-learn
#############################################################
rm(list = ls())
source("fn.base.R")

fn.register.wk()
data.gbc.01.pred.tmp <- foreach(
  k=0:data.cv.folds$K, 
  .combine=rbind) %dopar% {
  
  source("fn.base.R")
  
  fn.load.data.fold("gbc_01", k)
    
  fn.init.worker(data.fold$logname)
    
  for (ix in 1) {
    fit.args <- paste("'{\"n_estimators\": 1000, \"learning_rate\": 0.0035, ",
                      "\"loss\": \"deviance\", ",
                      "\"max_features\": 8, \"max_depth\": 7, ",
                      "\"random_state\": 788942, ",
                      "\"subsample\": 1, \"verbose\": 50}'")

    fit.more <- "" 
    if (ix > 1) fit.more <- "-load_model"
    
    system(
      paste(
        "cd ../west-niles-virus-py && python -u sci_learn_train.py",
        fit.more,
        "-train_data_file", data.fold$tr.file,
        "-test_data_file", data.fold$test.file,
        "-test_pred_file", data.fold$test.pred.file,
        "-test_metric auc",
        "-skip_mapping",
        "-target_col WnvPresent",
        "-model_type GradientBoostingClassifier",
        "-model_file ", paste0(data.fold$tr.file, ".pkl"),
        "-fit_args ", fit.args,
        " >> ", paste0("../data/log/", data.fold$logname, ".log"), " 2>&1"
      )
    )
  }
  
  data.fold$test.pred <- rbind(
    data.table(
      Id = data.fold$test.ids.in,
      Pred = fread(data.fold$test.pred.file)$pred
    ),
    data.table(
      Id = data.fold$test.ids.out,
      Pred = 0.0
    )
  )
    
  fn.print.err(data.fold$test.pred)

  fn.clean.worker()
  
  data.fold$test.pred

}
fn.kill.wk()

Store(data.gbc.01.pred.tmp)

data.gbc.01.pred <- data.gbc.01.pred.tmp[order(Id)]

#############################################################
# Saving data
#############################################################
# rm(list = ls())
source("fn.base.R")

fn.print.err(fn.apply.mult(data.gbc.01.pred))
#    Size      AUC
# 1 10506 0.8119684 

print(fn.calc.pred.mean(data.gbc.01.pred))
#    Year      Mean
# 1: 2007 0.5640557
# 2: 2009 0.5957073
# 3: 2011 1.0000000
# 4: 2013 0.3845166
# 5: 2008 0.5835099
# 6: 2010 0.4542783
# 7: 2012 0.5790881
# 8: 2014 1.0000000

Store(data.gbc.01.pred)

data.gbc.01.pred.smth <- fn.smooth.pred(data.gbc.01.pred)
fn.print.err(data.gbc.01.pred.smth)
#    Size       AUC
# 1 10506 0.8215739

Store(data.gbc.01.pred.smth) # 0.81513
