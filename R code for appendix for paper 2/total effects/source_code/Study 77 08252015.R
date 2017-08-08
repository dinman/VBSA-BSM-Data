#D.Inman August 28
rm(list=ls())
getwd()
setwd("C:/Users/dinman/Documents/GitHub/bsm-studies/analysis/Bio Economy  FY15 Q4 September 2015/src/ee/Study 77")
resultspath <- "C:/Users/dinman/Documents/GitHub/bsm-studies/analysis/Bio Economy  FY15 Q4 September 2015/src/ee/Study 77/results/"
source ("load.R")
source ("sens.functions.R")
options(stringsAsFactors = FALSE)
options(warn=-1)

temp <- loadWorkbook ("testdesign.xlsx", create = FALSE)

vars <- readWorksheet(temp, sheet="NewSetNumbers")
save(vars, file="vars.RData")

load("design.RData")
N <- nrow(vars)
r <- 25
s <- 25

drv <- dbDriver("PostgreSQL")

con <- src_postgres(host='dnpdb001.bigde.nrel.gov', port=5432, dbname='bsm', user='bsmclient', password='dogsled')

q3 <- ('select * FROM "Monkey"."summary_77_prod" natural join "Monkey"."Runs" where "Study" = 77')#Need to change study number to 77??####
outputs <- tbl(con, sql(q3))#Need to point this to variable of interest - MaxProd?
out <- collect (outputs)
out<-out [,-c(2,10:15)]
heading <- colnames(out[,-c(1)])
out.melt <- melt (out, id.vars = "Run")
colnames(out.melt)<- c("run", "var","val")
run.number <- as.data.table (out$Run)
colnames (run.number) <- "run"
split.num <- length(heading)
out.split <- split (out.melt, out.melt$var)

split.num <- as.data.frame (heading)
split.num$heading <- sub("^", "EE", split.num$heading)
split.num <- t (split.num)
new.names <- c(split.num)
new.names  = gsub("[ -]", "", new.names)

for (i in 1:(length(split.num))) {
  (assign(new.names[i], as.data.table (out.split[[i]])))
}



df.list = lapply(ls(pattern = "EE"), get)


batch.ee <- function (x,y,n,r){
  result <- ee(x,y,n,r)
  return (result)
}

#vars <- cbind (run.number, vars)

LIST1 <- list ()
for(i in 1:length (df.list)) { 
  LIST1[[i]] <- batch.ee(SA.design, df.list[[i]]$val, N, r)
  LIST1[[i]] <- join(vars, LIST1[[i]], by="Set")
  LIST1[[i]] <- arrange (LIST1[[i]], desc(std.errors))
  study.77.ee <- LIST1
}

for (i in 1:(length(study.77.ee))) {
  (assign(new.names[i], as.data.table (study.77.ee[[i]])))
}

for (i in 1:(length(study.77.ee))) {
  study.77.ee [[i]] <- mutate (study.77.ee[[i]], metric = new.names[i])
  
}

for (i in 1:(length(study.77.ee))) {
  (assign(new.names[i], as.data.table (study.77.ee[[i]])))
}

study.77.results <- do.call(rbind, study.77.ee)


save(study.77.ee, study.77.results, file= paste(resultspath, "study77EEresults.RData"))

write.csv (study.77.results, file = "study77results.csv")

sapply(names(new.names), 
       function (x) write.csv(study.77.ee[[x]], file=paste(x, "csv", sep=".") )   )


for (i in 1:length(study.77.ee)){
write.csv (study.77.ee[[i]], file = paste (resultspath, new.names[i], "csv", sep = "."))
}

