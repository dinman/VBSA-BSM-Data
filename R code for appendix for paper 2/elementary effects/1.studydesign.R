#
# Elementary Effects Study Design - Study 77
#
#   Decription: This code generates the model design and the BSM inputs used for Study 75, Q4 FY 15 sensitivity study
#
#       Inputs: There is only 1 input:
#               1. "~/inputs/testdesign.xlsx"
#               Note: To replicate this study with different input data, the desired input data must
#                      be formated in the same way as the testdesign.xlsx file
#       Outputs: There are four file outputs, but only two general types of outputs--1) the model design, and 2) BSM inputs:
#                 1) the model design
#                     1. design.Rdata -- The statistical model design
#                 2) the BSM inputs - Saved to 3 different file formats
#                     2. SS_inputs.txt -- BSM intputs, txt format
#                     3. SS_inputs.csv -- BSM inputs, csv format
#                     4. Study77design.RData -- BSM inputs, RData format
#               All outputs are saved to the "~/outputs/" directory
#
#   Note: In order to reproduce the same results, adjust the paths of the working directory and R library
#
#   Danny Inman, 06/02/2015
#   Supplementary materials for Inman (***) et al. 2016, TITLE OF THE PAPER (***), System Dynamic Review


## Setup Environments
setwd("~/elementary effects/")
rm(list=ls())
R_LIBS= ("/home/R/library")
library(sensitivity)
library (XLConnect)
library (dplyr)
library (plyr)
library (reshape2)
options(scipen = 999)
options(digits = 2)

## 1. Load Excel Input
wb <- loadWorkbook("./inputs/testdesign.xlsx")
vars <- readWorksheet(wb, sheet="NewSetNumbers")
vars <- mutate(vars, new.var.names = paste(Variable.Name, Subscript.1,
               Subscript.2, Index, sep=":"))
vars$delta <- vars$Study.Max - vars$Study.Min


## 2. Generate a Study Design
set.seed(12340)

# Morris 2r sample points per input:
#     total = r(k + 1) samples, where
#        k = the number of inputs
#        r = the number of trajectories (reps)
#        N = number of factors
N <- nrow(vars)
r <- 25

myGenerate <- function (X){
  rep(1,dim(X)[2])
}
# Generate the Morris Study Design
SA.design <- morris(model = myGenerate, factors = N, r = r,
                    design = list(type = "oat", levels = 6, grid.jump=1))$X
# Save the Morris Study Design
save(SA.design, file="./outputs/design.Rdata")



# 3. Transform Data
#         Each column represents a unique Variable + Subscript 1 + Subscript 2 + Index combination
#         Rows represent the individual runs.
a <- t(vars$Set)
b <- 1 : dim(SA.design)[2]
z <- matrix(a, nrow=length(b), ncol=length(a), byrow=TRUE)
zz <- apply(b == z, c(1,2), function(x) {if (x) 1 else 0})
w <- SA.design %*% zz

new.design <- w * matrix(vars$delta, nrow=dim(SA.design)[1], ncol=length(a), byrow=TRUE) + matrix(vars$Study.Minimum, nrow=dim(SA.design)[1], ncol=length(a), byrow=TRUE)
colnames(new.design) <- vars$new.var.names
design.in.bsm.format.1 <- melt(t(new.design))
colnames(design.in.bsm.format.1) <- c("variable", "run", "value")
df.1 <- design.in.bsm.format.1

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

design.in.bsm.format <- (rbind (df.1, ends))
design.in.bsm.format <-transform(design.in.bsm.format, run = as.numeric(run))
design.in.bsm.format <-transform(design.in.bsm.format, value = as.numeric(value))
design.in.bsm.format <- arrange(design.in.bsm.format, run)


## 4. Save Transformed Outputs
#       # These outputs are inputs for the BSM model
write.table(design.in.bsm.format, file="./outputs/SS_Inputs.txt", sep="\t")
write.csv(design.in.bsm.format, file="./outputs/SS_Inputs.csv")
save (design.in.bsm.format.1, design.in.bsm.format, duration, file = "./outputs/study77design.RData" )
