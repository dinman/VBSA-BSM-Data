#
# Elementary Effects Analysis - Study 77
#
#   Decription: This code takes the outputs from the BSM and the statistical design and runs the elementary effects analysis
#
#       Source Code: This script utilizes the following source code:
#                 1. "~/source_code/sens.functions.R"
#                 2. "~/source_code/load.R"
#       Inputs: There are three inputs:
#                 1. "~/inputs/testdesign.xlsx" -- DESCRIPTION HERE ********
#                 2. "~/outputs/design.xlsx" -- This is the morris study design generated in 1.studydesign.R
#                 3. "~/inputs/study77out" -- This is the output from the BSM Model run (ran indepently from R) ****
#       Outputs:  Statistical outputs for the Elementary Effects Analysis
#                 All outputs are saved to "~/elementary effects/outputs" directory
#                 Output files generated:
#                     1. vars.RData -- list of variables from "/inputs/testdesign.xlsx"
#                     2. study77results.RData -- DESCRIPTION HERE ***
#                     3. EE2035EtOHMax.csv -- DESCRIPTION HERE ***
#                     4. EE2035RRMax.csv -- DESCRIPTION HERE ***
#                     5. EE2035TotalMax.csv -- DESCRIPTION HERE ***
#                     6. EE2051EtOHMax.csv -- DESCRIPTION HERE ***
#                     7. EE2051RRMax.csv -- DESCRIPTION HERE ***
#                     8. EE2051TotalMax.csv -- DESCRIPTION HERE ***
#                     9. EEEtOHbeta.csv -- DESCRIPTION HERE ***
#                 Examples of Outputs Generated are located in "~/outputs/output_examples" directory
#
#   Note: In order to reproduce the same results, adjust the paths of the working directory and R library
#
#   Danny Inman, August 28, 2015
#   Supplementary materials for Inman (***) et al. 2016, TITLE OF THE PAPER (***), System Dynamic Review


## Set Up Environments
rm(list=ls())
# Set Paths
setwd("~/elementary effects/")
resultspath <- ("./outputs")
# Load Source Files
source ("./source_code/load.R")
source ("./source_code/sens.functions.R")
options(stringsAsFactors = FALSE)
options(warn=-1)


## 1. Load Inputs

# Load Excel Input
temp <- loadWorkbook ("./inputs/testdesign.xlsx", create = FALSE)
vars <- readWorksheet(temp, sheet="NewSetNumbers")
# Save variables in Rdata table
save(vars, file="./outputs/vars.RData")

# Load Morris Model Design Generated in "/elementary effects/1.studydesign.R"
load("./outputs/design.RData")
N <- nrow(vars)
r <- 25
s <- 25

# Load the Output from the BSM Model Run
load("./inputs/study77out.RData")
heading <- colnames(out[,-c(1)])
out.melt <- melt (out, id.vars = "Run")
colnames(out.melt)<- c("run", "var","val")
run.number <- as.data.table (out$Run)
colnames (run.number) <- "run"
split.num <- length(heading)
out.split <- split (out.melt, out.melt$var)
# Ceate DataFrame
split.num <- as.data.frame (heading)
split.num$heading <- sub("^", "EE", split.num$heading)
split.num <- t (split.num)
new.names <- c(split.num)
new.names  = gsub("[ -]", "", new.names)


## 2. Run the Elementary Effects Analysis
#     # source code: "/elementary effects/source_code/sens.functions.R"
for (i in 1:(length(split.num))) {
  (assign(new.names[i], as.data.table (out.split[[i]])))
}

df.list = lapply(ls(pattern = "EE"), get)

batch.ee <- function (x,y,n,r){
  result <- ee(x,y,n,r)
  return (result)
}

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


## 3. Save Outputs to File
save(study.77.ee, study.77.results, file= "./outputs/study77EEresults.RData")
setwd(resultspath)
write.csv (study.77.results, file = "study77results.csv")
sapply(names(new.names),
       function (x) write.csv(study.77.ee[[x]], file=paste(x, "csv", sep=".") )   )
for (i in 1:length(study.77.ee)){
  write.csv (study.77.ee[[i]], file = paste (new.names[i], "csv", sep = "."))
}