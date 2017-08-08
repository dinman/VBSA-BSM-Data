#
# Total Effects Analysis - Study 1001
#
#   Decription: *****************
#               Based on Saltelli, A. (2002). Making best use of model evaluations to compute sensitivity indices. Computer Physics Communications,
#               (2), 280-297.
#       Inputs: There are two inputs file:
#                 1. "/inputs/study.1001.total.2035.Rdata"
#                 2. "/outputs/study.1001.design.RDA"
#       Outputs: There are 9 files and two types of outputs generated:
#                 A. Tabular Outputs:
#                     1. "~/outputs/total.2035.sensitivity.results.RDA"
#                 B. Figure Outputs:
#                     1. Total.2035.histogram.png -- Total Effects histogram for study 1001, year 2035
#                     2. Total.2035.Si.boot.png -- Total Effects bootstrap sentitivity indicies for study 1001, year 2035
#                     3. Total.2035.Si.png -- Total Effects sentitivity indicies for study 1001, year 2035
#                     4. Total.2035.Sij.boot.png -- Total Effects bootstrap second order sentitivity indicies for study 1001, year 2035
#                     5. Total.2035.Sij.png -- Total Effects second order sentitivity indicies for study 1001, year 2035
#                     6. Total.2035.Sijc.boot.png -- Total Effects bootsrap closed second order sensitivity indicies for study 1001, year 2035
#                     7. total.2035.Sti.boot.png -- Total Effects png total sentitivity indicies for study 1001, year 2035
#                     8. Total.2035.Sti.png -- Total Effects total sentitivity indicies for study 1001, year 2035
#                 All figures are saved in the "~/outputs/figures" directory
#       Source Code:
#               1."/source_code/Total_Effects_Function.R" -- ****
#               2. "/source_code/load.R" -- ***
#
#   Note: In order to reproduce the same results, adjust the paths of the working directory and R library
#
#   Danny Inman, 08/26/2015
#   Supplementary materials for Inman (***) et al. 2016, TITLE OF THE PAPER (***), System Dynamic Review

# Set up Environments
rm(list=ls())
R_LIBS= ("/home/R/library")
setwd("~/total effects")
options(scipen=999)
resultspath <- "./outputs//"
figurepath <- "./outputs/figures//"
source("./source_code/Total_Effects_Function.R")
source ("./source_code/load.R")

# Load data and set following parameters
#drv <- dbDriver("PostgreSQL")
#con <- src_postgres(host='dnpdb001.bigde.nrel.gov', port=5432, dbname='bsm', user='bsmclient', password='dogsled')

#load the TE study design
load ("./outputs/study.1001.design.RDA")

#load the BSM output data
#load ("study.1001.etoh.2051.Rdata")
#load ("study.1001.rr.2051.Rdata")
#load ("study.1001.total.2051.Rdata")

#load ("study.1001.etoh.2035.Rdata")
#load ("study.1001.rr.2035.Rdata")
load ("study.1001.total.2035.Rdata")

#name the dataset to correspond to the BSM output loaded
dataset <- "total.2035."

dataset <- "te.output.full"

# Create Histogram Output
png (file = paste (figurepath, dataset, "histogram.png"))
hist(output)
dev.off()

# Get Study Design Settings
k <- as.numeric (length (unique (design.in.bsm.format$variable)))
N <- as.numeric (length (unique (design.in.bsm.format$run)))/ (2*(k + 1))

# define variable names - use ~ factor nemes for this
var.names <- c(colnames (new.design))

#define the combinations of variables for interaction effects here - need to look at how to automate the naming of these in larger studies (i.e. k = 20)
var.names.2 <- as.character (abbreviate (var.names, 4, strict = FALSE, dot = FALSE)) #this creates abbreviations for the factors

#center output by subtracting the mean from output
output.centered <- output - mean(output)


#split total output into A and B vectors (1xN) and C and D matrices (Nxk), see Saltelli 2008 ch. 4
A.out <- output.centered[1:N] # (N x 1)
B.out <- output.centered[(N+1):(N*2)] # (N x 1)

C.out <- matrix(nrow=N, ncol=k)
for(i in 1:k){
  C.out[,i] <- output.centered[((i+1)*N+1):((i+2)*N)]
}

D.out <- matrix(nrow=N, ncol=k)
for(i in 1:k){
  D.out[,i] <- output.centered[((i+1+k)*N+1):((i+2+k)*N)]
}

ABCD <- cbind(A.out, B.out, C.out, D.out)

#calculate sens indicies
sensitivity <- get.sensitivity.new(A.out, B.out, C.out, D.out)

#add variable names to sensitivity data frame
for(i in 1:nrow(sensitivity)){
  if(is.na(sensitivity$variable.j[i])){
    sensitivity$variable.name[i] <- var.names[sensitivity$variable.i[i]]
  } else {
    sensitivity$variable.name[i] <- paste(var.names.2[sensitivity$variable.i[i]],
                                          var.names.2[sensitivity$variable.j[i]], sep="-")
  }
}


##-----------------------------------------------------##
# First Order Index (Si)

df <- sensitivity[sensitivity$estimate=="S.i", ]

png (file = paste (figurepath, dataset, "Si.png"))

plot <- ggplot(df, aes(x=reorder(df$variable.name, -df$est.value),
                       y=est.value,
                       ymin=est.value-est.PE,
                       ymax=est.value+est.PE))
plot + geom_point(size=4) +
  geom_errorbar(width=.2) +
  geom_hline(yintercept=0, linetype=2) +
  labs(title="Sensitivity Indices",
       x="Variable",
       y=expression("S"["i"])) +
  theme_bw() +
  theme(title = element_text(size=15),
        axis.title.x=element_text(size=12),
        axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=12, angle = 90))

dev.off()

# Total Effects Index (St)

df <- sensitivity[sensitivity$estimate=="St.i", ]

png (file = paste (figurepath, dataset, "Sti.png"))

plot <- ggplot(df, aes(x=reorder(df$variable.name, -df$est.value),
                       y=est.value,
                       ymin=est.value-est.PE,
                       ymax=est.value+est.PE))
plot + geom_point(size=4) +
  geom_errorbar(width=.2) +
  geom_hline(yintercept=0, linetype=2) +
  labs(title="Total Sensitivity Indices",
       x="Variable",
       y=expression("S"["Ti"])) +
  theme_bw() +
  theme(title = element_text(size=15),
        axis.title.x=element_text(size=12),
        axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=12, angle = 90))
dev.off()

# Sij
df <- sensitivity[sensitivity$estimate=="S.ij", ]

arrange (df, desc(est.value))

df.list <- split(df,rep(1:11,each = 23))

df <- df.list[[1]] #this is a temporary fix - this only plots the top 23 most influential interations

png (file = paste (figurepath, dataset, "Sij.png"))

plot <- ggplot(df, aes(x=reorder(df$variable.name, -df$est.value),
                       y=est.value))
plot + geom_point(size=4) +
  geom_hline(yintercept=0, linetype=2) +
  labs(title="Second Order Sensitivity Indices",
       x="Variable",
       y=expression("S"["ij"])) +
  theme_bw() +
  theme(title = element_text(size=15),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=8, angle=90))

dev.off()

# Bootstrapping for total effects and second order effects
# Add bootstrap confidence internvals to sensitivity matrix

k <- (ncol(ABCD) - 2)/2

##-----------------------------------------------------##
# Bootstrap Estimates - calculate mean and .025 and .975 percentiles
source("./source_code/Total_Effects_Function.R")
boots <- 2000

boot.sensi <- data.frame(matrix(nrow=nrow(sensitivity), ncol=boots))

for(b in 1:boots){
  boot.sample <- ABCD[sample(nrow(ABCD), replace=TRUE),]
  A.boot <- boot.sample[,1]
  B.boot <- boot.sample[,2]
  C.boot <- boot.sample[,3:(k+2)]
  D.boot <- boot.sample[,(k+3):(2*k+2)]

  sensi <- get.sensitivity.new(A.boot, B.boot, C.boot, D.boot)

  boot.sensi[,b] <- sensi$est.value
}

sensitivity$boot.avg <- apply(boot.sensi, 1, mean)
sensitivity$boot.025 <- apply (boot.sensi, 1, quantile, 0.025)
sensitivity$boot.975 <- apply (boot.sensi, 1, quantile, 0.975)

save (sensitivity, file = paste (resultspath, dataset, "sensitivity.results.RDA"))
##this is giving an error but is working....
#for(i in 1:nrow(boot.sensi)){
#cdf <- ecdf(boot.sensi[i,])
#sensitivity$boot.025[i] <- quantile(cdf, .025)
#sensitivity$boot.975[i] <- quantile(cdf, .975)
#}

#save(sensitivity, file= paste (resultspath, "bootstrapped.sensi.max.2035.Rda"))

# Sensi Index - bootstrap v PE

df <- sensitivity[sensitivity$estimate=="S.i", ]

png (file = paste (figurepath, dataset, "Si.boot.png"))

plot <- ggplot(df, aes(x=reorder(df$variable.name, -df$est.value)))
plot + geom_point(aes(y=est.value), size=4) +
  geom_errorbar(aes(ymin=est.value-est.PE,
                    ymax=est.value+est.PE),
                width=.5) +
  geom_point(aes(y=(boot.avg)),
             size=4,
             shape=4,
             color="blue") +
  geom_errorbar(aes(ymin=boot.025,
                    ymax=boot.975),
                width=.5,
                lty=2,
                color="blue") +
  geom_hline(yintercept=0, linetype=2) +
  labs(title="Sensitivity Indices",
       x="Variable",
       y=expression("S"["i"])) +
  theme(title = element_text(size=15),
        axis.title.x=element_text(size=12),
        axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=12, angle = 90))

dev.off()

##-----------------------------------------------------##
# Total Index - bootstrap v PE


df <- sensitivity[sensitivity$estimate=="St.i", ]

png (file = paste (figurepath, dataset, "Sti.boot.png"))

plot <- ggplot(df, aes(x=reorder(df$variable.name, -df$est.value)))
plot + geom_point(aes(y=est.value), size=4) +
  geom_errorbar(aes(ymin=est.value-est.PE,
                    ymax=est.value+est.PE),
                width=.5) +
  geom_point(aes(y=(boot.avg)),
             size=4,
             shape=4,
             color="blue") +
  geom_errorbar(aes(ymin=boot.025,
                    ymax=boot.975),
                width=.5,
                lty=2,
                color="blue") +
  geom_hline(yintercept=0, linetype=2) +
  labs(title="Total Sensitivity Indices",
       x="Variable",
       y=expression("S"["Ti"])) +
  theme(title = element_text(size=15),
        axis.title.x=element_text(size=12),
        axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=12, angle = 90))

dev.off()

##-----------------------------------------------------##
# Closed Complement - bootstrap v PE


df <- sensitivity[sensitivity$estimate=="S._ij.c", ]

png (file = paste (figurepath, dataset, "Sij.boot.png"))

plot <- ggplot(df, aes(x=reorder(df$variable.name, df$est.value)))
plot + geom_point(aes(y=est.value), size=4) +
  geom_errorbar(aes(ymin=est.value-est.PE,
                    ymax=est.value+est.PE),
                width=.5) +
  geom_point(aes(y=(boot.avg)),
             size=3,
             shape=4,
             color="blue") +
  geom_errorbar(aes(ymin=boot.025,
                    ymax=boot.975),
                width=.5,
                lty=2,
                color="blue") +
  geom_hline(yintercept=0, linetype=2) +
  geom_hline(yintercept=1, linetype=2) +
  labs(title="Closed Complement Sensitivity Indices",
       x="Variable",
       y=expression("S"["-ij"]^"c")) +
  theme(title = element_text(size=15),
        axis.title.x=element_text(size=12),
        axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=8, angle=90))
dev.off()

##-----------------------------------------------------##
# Closed Second Order Index - bootstrap v PE


df <- sensitivity[sensitivity$estimate=="S.ij.c", ]

png (file = paste (figurepath, dataset, "Sijc.boot.png"))

plot <- ggplot(df, aes(x=reorder(df$variable.name, -df$est.value)))
plot + geom_point(aes(y=est.value), size=4) +
  geom_errorbar(aes(ymin=est.value-est.PE,
                    ymax=est.value+est.PE),
                width=.5) +
  geom_point(aes(y=(boot.avg)),
             size=3,
             shape=4,
             color="blue") +
  geom_errorbar(aes(ymin=boot.025,
                    ymax=boot.975),
                width=.5,
                lty=2,
                color="blue") +
  geom_hline(yintercept=0, linetype=2) +
  #   geom_hline(yintercept=1, linetype=2) +
  labs(title="Closed Second Order Sensitivity Indices",
       x="Variable",
       y=expression("S"["ij"]^"c")) +
  theme(title = element_text(size=15),
        axis.title.x=element_text(size=12),
        axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=8, angle=90))

dev.off()

##-----------------------------------------------------##
# Second Order Index - bootstrap,
# No probable error estimate for S.ij


df <- sensitivity[sensitivity$estimate=="S.ij", ]

png (file = paste (figurepath, dataset, "Sij.boot.png"))

plot <- ggplot(df, aes(x=reorder(df$variable.name, -df$est.value)))
plot + geom_point(aes(y=est.value), size=4) +
  geom_errorbar(aes(ymin=est.value-est.PE,
                    ymax=est.value+est.PE),
                width=.5) +
  geom_point(aes(y=(boot.avg)),
             size=3,
             shape=4,
             color="blue") +
  geom_errorbar(aes(ymin=boot.025,
                    ymax=boot.975),
                width=.5,
                lty=2,
                color="blue") +
  geom_hline(yintercept=0, linetype=2) +
  #   geom_hline(yintercept=1, linetype=2) +
  labs(title="Second Order Sensitivity Indices",
       x="Variable",
       y=expression("S"["ij"])) +
  theme(title = element_text(size=15),
        axis.title.x=element_text(size=12),
        axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=8, angle=90))
dev.off()
