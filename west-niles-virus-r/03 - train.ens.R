##############################################################
## load data
##############################################################
rm(list = ls())
source("fn.base.R")

tic()
cat("Loading ensenble data... \n")

data.ens.cols <- c(
  "data.rgf.01.pred.smth",
  "data.gbc.01.pred.smth"
)

data.ens.pred <- fn.load.ens(ens.cols = data.ens.cols, print.err = F)

Store(data.ens.pred)


fn.data.mult.m.pred <- function(x) {
  fn.apply.mult(data.pred=x, data.mult = data.mult.m.01)
}

data.ens.my.pred <- fn.load.ens(ens.cols = data.ens.cols,
                                transf = fn.data.mult.m.pred,
                                print.err = F)

Store(data.ens.my.pred)

cols.ens.all <- setdiff(colnames(data.ens.my.pred), "Id")
data.ens.all.pred <- merge(
  data.ens.my.pred,
  data.ens.pred, 
  by="Id", suffixes = c(".y", ".my"))

data.ens.all.pred <- fn.add.date.fields(data.ens.all.pred)
data.ens.all.pred <- merge(
  data.ens.all.pred,
  data.all.trap[, list(Id, Species)],
  by="Id"
)

## add some hand crafted multipliers

for (col.nam in cols.ens.all) { 
  col.nam.y <- paste0(col.nam, ".y")
  col.nam.my <- paste0(col.nam, ".my")
  data.ens.all.pred[, col.nam := (1*get(col.nam.y) + 
                                        6*get(col.nam.my))/7, with=F]
  
  fn.multiplier <- function(yr, mnth, mult) {
    data.ens.all.pred[get("Year") == yr & get("Month") == mnth, 
                    col.nam := get(col.nam)*mult, with=F]
  }
  
  fn.multiplier(yr=2012, mnth=7, mult=1.6)
  
  fn.multiplier(yr=2012, mnth=9, mult=0.3)
  
  fn.multiplier(yr=2012, mnth=8, mult=0.8)
  
  fn.multiplier(yr=2008, mnth=9, mult=1.2)

  fn.multiplier(yr=2008, mnth=7, mult=0.85)
  
  
  data.ens.all.pred[
    get("Year") == 2012 & get("Month")== 8 & get("WeekYear") == 35, 
    col.nam := get(col.nam)*0.3, with=F]
  
  data.ens.all.pred[
    get("Year") == 2012 & get("Month")==8 & get("WeekYear") == 34, 
    col.nam := get(col.nam)*0.3, with=F]
  
  data.ens.all.pred[
    get("Year") == 2012 & get("WeekYear") == 32, 
    col.nam := get(col.nam)*0.7, with=F]
  
  data.ens.all.pred[
    get("Year") == 2012 &  get("WeekYear") == 31, 
    col.nam := get(col.nam)*1.6, with=F]
  
  data.ens.all.pred[
    get("Year") == 2012 & get("WeekYear") == 30, 
    col.nam := get(col.nam)*1.6, with=F]
  
  data.ens.all.pred[
    get("Year") == 2012 & get("WeekYear") == 29, 
    col.nam := get(col.nam)*1.1, with=F]
  
  data.ens.all.pred[
    get("Year") == 2012 & get("Month")== 7 & get("WeekYear") == 28, 
    col.nam := get(col.nam)*0.9, with=F]
  
  data.ens.all.pred[
    get("Species") == "CULEX PIPIENS", 
    col.nam := get(col.nam)*0.5, with=F]
  
  data.ens.all.pred[
    get("Species") == "CULEX RESTUANS", 
    col.nam := get(col.nam)*0.9, with=F]
  
  data.ens.all.pred[, col.nam.y := NULL, with=F]
  data.ens.all.pred[, col.nam.my := NULL, with=F]
}

data.ens.all.pred <- data.ens.all.pred[, c("Id", cols.ens.all), with=F]

Store(data.ens.all.pred)

toc()

##############################################################
## create ensenble
##############################################################
tic()
cat("Creating ensenble... \n")

expr.ens <- quote(
  3*rank(rgf.01) + 1*rank(gbc.01)
)

data.ens.all.pred.sub <- copy(data.ens.all.pred)
data.ens.all.pred.sub[, Pred := 0.0]
data.ens.all.pred.sub[rgf.01 > 0, Pred := eval(expr.ens)]

Store(data.ens.all.pred.sub)

fn.write.submission(data.ens.all.pred.sub, "data.ens.all.pred.sub") 

# data.ens.all.ord.list <- list()
# for (rem in c(0:1)) {
#   data.ens.all.ord <- fn.calc.pred.month.mean(data.ens.all.pred.sub)[Year %% 2 == rem]
#   data.ens.all.ord <- merge(
#     data.ens.all.ord, data.mult.m.01[,list(Year, Month, AUC)], 
#     by=c("Year", "Month")
#   )
#   data.ens.all.ord[,RankMean := rank(-Mean)]
#   data.ens.all.ord[,RankAUC := rank(-AUC)]
#   data.ens.all.ord[,RankDiff := rank(-Mean) - rank(-AUC)]
#   data.ens.all.ord <- data.ens.all.ord[order(-AUC)]
#   
#   data.ens.all.ord.list[[rem+1]] <- data.ens.all.ord
# }
# print(data.ens.all.ord.list[[1]])
# print(data.ens.all.ord.list[[1]][order(-Mean)])



toc()


