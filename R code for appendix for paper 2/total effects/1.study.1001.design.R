#
# Total Effects Study Design -- Study 1001
#
#   Decription: This code generates the model design and the BSM inputs used for *****
#
#       Inputs: There are two inputs:
#               1. factors071715.xlsx -- DESCRIPTION HERE ***
#               2. modelsettings.xlsx -- DESCRIPTION HERE ***
#               Note: To replicate this study with different input data, the desired input data must
#                      be formated in the same way as the factors071715.xlsx file and the modelsettings.xlsx file
#       Outputs: There two outputs generated:
#               1. study.1001.design.RDA -- DESCRIPTION HERE ****
#               2. study.1001.design.ends.RDA -- DESCRIPTION HERE ****
#               All outputs are saved to the "~/outputs/" directory
#
#   Note: In order to reproduce the same results, adjust the paths of the working directory and R library
#   Danny Inman, 08/25/2015
#   Supplementary materials for Inman (***) et al. 2016, TITLE OF THE PAPER (***), System Dynamic Review


# Setup Environments
rm(list=ls())
R_LIBS= ("/home/R/library")
getwd()
setwd("~/total effects")

# Load pertinent libraries
library (XLConnect)
library(randtoolbox)
library (dplyr)
library (reshape2)

#load factors to be assessd, these should be in an excel workbook named "factors.xlsx" within a sheet named "factors"; this is a result of EE
wb <- loadWorkbook ("./inputs/factors071715.xlsx")
var.names <- readWorksheet(wb, sheet="factors")
factor.names <- as.list (var.names[,1])

# load the factor settings from BSM. The number of factors in this workbook must match the number of factors from EE.
#note the filename and sheet number below - check that these are correct
wb <- loadWorkbook("./inputs/modelsettings.xlsx")
bsm.vars <- readWorksheet(wb, sheet="NewSetNumbers")
bsm.vars <- mutate(bsm.vars, factor = paste(Variable.Name, Subscript.1,Subscript.2, sep=" "))
bsm.vars$delta <- bsm.vars$Study.Max - bsm.vars$Study.Min

factors <- merge (var.names, bsm.vars, by = "factor")
factors$Set <- seq (from = 1, to = nrow(factors))

# create the sampling space for setting the factors
seed <- 31001

#our sensitivity study in 2014 showed that N of 2000 - 4000 produced the narrowest BCI for a 19-factor model. 3000 is a conservative estiamte
#study size = N(k+2)
N <- 4000  #number of observations
k <- nrow(factors)   #number of factors

#generate quasi-random sequence
qrs <- sobol(N, dim=2*k, seed=seed)

#Separate sequence into A and B matrices, each (N x factors)
A <- qrs[,1:k]
B <- qrs[,(k+1):(2*k)]

#Create C matrices
#For each factor, C = matrix B with column i taken from matrix A
C.runs <- matrix(nrow=N*k,ncol=k)

for(i in 1:k){
  C <- B
  C[,i] <- A[,i]
  C.runs[((i-1)*N+1):(i*N),] <- C
}

#Create D matrices
#For each factor, D = matrix A with column i taken from matrix B
D.runs <- matrix(nrow=N*k,ncol=k)

for(i in 1:k){
  D <- A
  D[,i] <- B[,i]
  D.runs[((i-1)*N+1):(i*N),] <- D
}

#Combine matrices
runs <- rbind( A, B, C.runs, D.runs)
#save (runs, file = "te07172015.design.RData")

# 3. Transform data; each column represents a unique Variable + Subscript 1 + Subscript 2 + Index
#combination.  Rows represent the individual runs.
a <- t(factors$Set)
b <- 1 : dim(runs)[2]
z <- matrix(a, nrow=length(b), ncol=length(a), byrow=TRUE)
zz <- apply(b == z, c(1,2), function(x) {if (x) 1 else 0})
w <- runs %*% zz

#take result and put into BSM format
new.design <- w * matrix(factors$delta, nrow=dim(runs)[1], ncol=length(a), byrow=TRUE) + matrix(factors$Study.Minimum, nrow=dim(runs)[1], ncol=length(a), byrow=TRUE)
#colnames(new.design) <- t(var.names)

colnames(new.design) <- t(factors$factor)
design.in.bsm.format <- melt(t(new.design))
colnames(design.in.bsm.format) <- c("variable", "run", "value")
#write.csv(design.in.bsm.format, file = "te.design.bsm.csv")
save (design.in.bsm.format, new.design, file = "./outputs/study.1001.design.RDA")#This is the file to bring into SA

#add end years to the input sheet
df.1 <- design.in.bsm.format
starts <- df.1[grep("_Start", df.1$variable), ]
starts <- arrange(starts, variable)
duration <- df.1[grep("_Dur", df.1$variable), ]
duration <- arrange(duration, variable)
ends <- cbind(starts, duration$value)
ends <- mutate(ends, end.year = value + duration$value)
ends[] <- lapply(ends, function(x) gsub("_Start", "_End", x))
ends <- ends [, -c(3:4)]
colnames (ends) <- c("variable", "run", "value")
df.1 <- df.1 [!(df.1$variable %in% duration$variable),]

design.in.bsm.format.2 <- (rbind (df.1, ends))
design.in.bsm.format.2 <-transform(design.in.bsm.format.2, run = as.numeric(run))
design.in.bsm.format.2 <-transform(design.in.bsm.format.2, value = as.numeric(value))
design.in.bsm.format.2 <- arrange(design.in.bsm.format.2, run)

#write.csv (design.in.bsm.format.2, file = "te.design.end.year.csv")
save (design.in.bsm.format.2, file = "./outputs/study.1001.design.ends.RDA")# this is the file to pass to Stella

#sanity checks on design
unique (design.in.bsm.format.2$variable)
max (design.in.bsm.format$run)

temp <- dcast(design.in.bsm.format.2, run ~ variable)
summary (temp)






