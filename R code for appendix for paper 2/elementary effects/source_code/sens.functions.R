#
# EE and Sensitivity Functions
#
#
#   Decription: This source code stores the Elementary Effects functions and the different sensitvity functions.
#               - Two elementary effects functions and different methods for calculating sensitivity indices from reviewed source.
#               - Sensitivity Measures for Sensi Index, Total Effects, complements of (k-2) order effects (S(-ij)^c),
#                 second order closed (S.ij.c) and second order (S.ij) effects
#               - Based on:
#                 1) Saltelli, A. (2002). Making best use of model evaluations to
#                     compute sensitivity indices. Computer Physics Communications, (2), 280-297.
#                 2) Saltelli (2010) Variance based sensitivity analysis of model output. Design and estimator
#                     for the total sensitivity index
#                 3) Saltelli (2008) Global Sensitivity Analysis: The Primer
#       Parameters:
#             1) EE Functions: (x, y, N, r)
#                     - x is *****
#                     - y is *****
#                     - N is *****
#                     - r is *****
#             2) Sensitivity Functions (A,B,C,D)
#                     - A,B are vector of length N of output from matrices A and B
#                     - C and D are matrices of size N x k, made up of the output vectors from matrices Ci..k, Di..k
#       Functions:
#             1. ee: ****DESCRIPTION HERE *****
#             2. ee.2: ****DESCRIPTION HERE *****
#             3. get.sensitivity.A:  Using just A matrix for calculation of mean squared and variance of Y
#             4. get.sensitivity.AB: Uses A and B matrices for calculation of mean squared and variance of Y for the first/second order effects
#             5. get.sensitivity.Sal10:  Uses equation in Saltelli 2010 paper for first order and total effects
#             6. get.sensitivity.new:  Uses total effects equation from Sal10, and first/second order equations from AB
#
# Paige Erspamer, 07/18/2014

# ---------------------------------------------------------------------------------------------------------


# Elementary Effects Function 1: ee
# **** DESCRIPTION HERE *****
ee <- function(x = NULL, y = NULL, N = NULL, r = NULL) {
  m <- (N + 1) * r
  xdiff <- (x[2:m, ] - x[1:(m-1), ])[-(N+1)*1:(r-1), ]
  delta <- t(xdiff)[t(xdiff) != 0]
  ydiff <- (y[2:m] - y[1:(m-1)])[-(N+1)*1:(r-1)]
  index <- rep(0:(r-1)*(N + 1), each=N) + apply(xdiff, 1, function(x) (1:N)[x != 0])
  sort.index <- arrange(data.frame(x1 = ydiff, x2 = delta, y = index), index)
  ratio <- matrix(sort.index$x1 / sort.index$x2, nr = N)
  mu <- apply(ratio, 1, mean)
  mu.star <- apply(ratio, 1, function(x) mean(abs(x)))
  sigma <- apply(ratio, 1, sd)
  sem <- sigma/(r^0.5)
  std.e <- abs(mu)/(sigma/(r^0.5))
  data.frame(Set=1:N, mu = mu, mu.star = mu.star, sigma = sigma, sem =sem, std.errors = std.e)
}

# Elementary Effects Function 2: ee.2
# **** DESCRIPTION HERE *****
ee.2 <- function(x = SA.design, y = NULL, N = N, r = r) {
  m <- (N + 1) * r
  xdiff <- (x[2:m, ] - x[1:(m-1), ])[-(N+1)*1:(r-1), ]
  delta <- t(xdiff)[t(xdiff) != 0]
  ydiff <- (y[2:m] - y[1:(m-1)])[-(N+1)*1:(r-1)]
  index <- rep(0:(r-1)*(N + 1), each=N) + apply(xdiff, 1, function(x) (1:N)[x != 0])
  sort.index <- arrange(data.frame(x1 = ydiff, x2 = delta, y = index), index)
  ratio <- matrix(sort.index$x1 / sort.index$x2, nr = N)
  mu <- apply(ratio, 1, mean)
  mu.star <- apply(ratio, 1, function(x) mean(abs(x)))
  sigma <- apply(ratio, 1, sd)
  sem <- sigma/(r^0.5)
  std.e <- abs(mu)/(sigma/(r^0.5))
  data.frame(Set=1:N, mu = mu, mu.star = mu.star, sigma = sigma, sem =sem, std.e = std.e)
}

## Sensitivity Function 1: get.sensitivity.A
# Uses just A matrix for calculation of mean squared and variance of Y
get.sensitivity.A <- function(A, B, C, D=NULL){
  N <- length(A)
  k <- ncol(C)

  #Set number of parameters to calculate
  if(is.null(D)){
    #first, total, and closed complement indices
    params <- 2*k + ((k-1)*k/2)
  } else {
    #First, total, closed complement, second order closed, and second order indices
    params <- 2*k + 3*((k-1)*k/2)
  }

  sensi <- as.data.frame(matrix(nrow=params, ncol=5))
  colnames(sensi) <- c("variable.i", "variable.j",
                       "estimate", "est.value",
                       "est.PE")
  count <- 1

  es.y <- mean(A)^2
  var.y <- (A%*%A)/N-es.y


  #First Order and Total Effects
  for(i in 1:k){
    sensi$variable.i[count] <- i
    sensi$estimate[count] <- "S.i"
    sensi$est.value[count] <- ((A%*%C[,i])/(N-1)-es.y)/var.y
    sensi$est.PE[count] <- (1.96/sqrt(N)*sqrt(sum((A*C[,i])^2)/N - (A%*%C[,i]/N)^2))/var.y
    count <- count + 1

    sensi$variable.i[count] <- i
    sensi$estimate[count] <- "St.i"
    sensi$est.value[count] <- 1 - ((B%*%C[,i])/(N-1)-es.y)/var.y
    sensi$est.PE[count] <- (1.96/sqrt(N)*sqrt(sum((B*C[,i])^2)/N - (B%*%C[,i]/N)^2))/var.y
    count <- count + 1

  }

  #Closed Complement Indices
  for(i in 1:k){
    for(j in (i+1):k){
      if(j>k){
        break
      }
      sensi$variable.i[count] <- i
      sensi$variable.j[count] <- j
      sensi$estimate[count] <- "S._ij.c"
      sensi$est.value[count] <- ((C[,i]%*%C[,j])/(N-1) - es.y)/var.y
      sensi$est.PE[count] <- (1.96/sqrt(N)*sqrt(sum((C[,i]*C[,j])^2)/N - (C[,i]%*%C[,j]/N)^2))/var.y
      count <- count + 1

    }
  }

  #Only compute Second Order if D output is given
  if(!is.null(D)){

    #Second Order Closed and Second Order Indices
    for(i in 1:k){

      Vi <- (A%*%C[,i])/(N-1)-es.y

      for(j in (i+1):k){
        if(j>k){
          break
        }

        Vj <- (A%*%C[,j])/(N-1)-es.y

        Vij.c <- ((C[,i]%*%D[,j])/(N-1) - es.y)

        sensi$variable.i[count] <- i
        sensi$variable.j[count] <- j
        sensi$estimate[count] <- "S.ij.c"
        sensi$est.value[count] <- Vij.c/var.y
        sensi$est.PE[count] <- (1.96/sqrt(N)*sqrt(sum((C[,i]*D[,j])^2)/N - (C[,i]%*%D[,j]/N)^2))/var.y
        count <- count + 1

        sensi$variable.i[count] <- i
        sensi$variable.j[count] <- j
        sensi$estimate[count] <- "S.ij"
        sensi$est.value[count] <- (Vij.c-Vi-Vj)/var.y
        sensi$est.PE[count] <- NA
        count <- count + 1

      }
    }
  }

  return(sensi)
}

## Sensitivity Function 2: get.sensitvity.AB
# Uses A and B matrices for calculation of mean squared and variance of Y for the first/second order effects
get.sensitivity.AB <- function(A, B, C, D=NULL){
  N <- length(A)
  k <- ncol(C)

  #Set number of parameters to calculate
  if(is.null(D)){
    #first, total, and closed complement indices
    params <- 2*k + ((k-1)*k/2)
  } else {
    #First, total, closed complement, second order closed, and second order indices
    params <- 2*k + 3*((k-1)*k/2)
  }

  sensi <- as.data.frame(matrix(nrow=params, ncol=5))
  colnames(sensi) <- c("variable.i", "variable.j",
                       "estimate", "est.value",
                       "est.PE")
  count <- 1

  es.y.1 <- mean(A)^2
  var.y.1 <- (A%*%A)/N-es.y.1

  es.y.2 <- (A%*%B)/N
  var.y.2 <- (A%*%A)/N-es.y.2

  #   var.y <- (A%*%A)/N-es.y.1

  #First Order and Total Effects
  for(i in 1:k){
    sensi$variable.i[count] <- i
    sensi$estimate[count] <- "S.i"
    sensi$est.value[count] <- ((A%*%C[,i])/(N-1)-es.y.2)/var.y.2
    sensi$est.PE[count] <- (1.96/sqrt(N)*sqrt(sum((A*C[,i])^2)/N - (A%*%C[,i]/N)^2))/var.y.2
    count <- count + 1

    sensi$variable.i[count] <- i
    sensi$estimate[count] <- "St.i"
    sensi$est.value[count] <- 1 - ((B%*%C[,i])/(N-1)-es.y.1)/var.y.1
    sensi$est.PE[count] <- (1.96/sqrt(N)*sqrt(sum((B*C[,i])^2)/N - (B%*%C[,i]/N)^2))/var.y.1
    count <- count + 1

  }

  #Closed Complement Indices
  for(i in 1:k){
    for(j in (i+1):k){
      if(j>k){
        break
      }
      sensi$variable.i[count] <- i
      sensi$variable.j[count] <- j
      sensi$estimate[count] <- "S._ij.c"
      sensi$est.value[count] <- ((C[,i]%*%C[,j])/(N-1) - es.y.2)/var.y.2
      sensi$est.PE[count] <- (1.96/sqrt(N)*sqrt(sum((C[,i]*C[,j])^2)/N - (C[,i]%*%C[,j]/N)^2))/var.y.2
      count <- count + 1

    }
  }

  #Only compute Second Order if D output is given
  if(!is.null(D)){

    #Second Order Closed and Second Order Indices
    for(i in 1:k){

      Vi <- (A%*%C[,i])/(N-1)-es.y.2

      for(j in (i+1):k){
        if(j>k){
          break
        }

        Vj <- (A%*%C[,j])/(N-1)-es.y.2

        Vij.c <- ((C[,i]%*%D[,j])/(N-1) - es.y.2)

        sensi$variable.i[count] <- i
        sensi$variable.j[count] <- j
        sensi$estimate[count] <- "S.ij.c"
        sensi$est.value[count] <- Vij.c/var.y.2
        sensi$est.PE[count] <- (1.96/sqrt(N)*sqrt(sum((C[,i]*D[,j])^2)/N - (C[,i]%*%D[,j]/N)^2))/var.y.2
        count <- count + 1

        sensi$variable.i[count] <- i
        sensi$variable.j[count] <- j
        sensi$estimate[count] <- "S.ij"
        sensi$est.value[count] <- (Vij.c-Vi-Vj)/var.y.2
        sensi$est.PE[count] <- NA
        count <- count + 1

      }
    }
  }

  return(sensi)
}

## Sensitivity Function 3: get.sensitivity.Sal10
# Uses equation in Saltelli 2010 paper for first order and total effects
get.sensitivity.Sal10 <- function(A, B, C, D){
  N <- length(A)
  k <- ncol(C)

  #First, total, closed complement, second order closed, and second order indices
  params <- 2*k + 3*((k-1)*k/2)

  sensi <- as.data.frame(matrix(nrow=params, ncol=5))
  colnames(sensi) <- c("variable.i", "variable.j",
                       "estimate", "est.value",
                       "est.PE")
  count <- 1

  es.y.1 <- mean(A)^2
  var.y.1 <- (A%*%A)/N-es.y.1

  es.y.2 <- (A%*%B)/N
  var.y.2 <- (A%*%A)/N-es.y.2


  #First Order and Total Effects
  for(i in 1:k){
    sensi$variable.i[count] <- i
    sensi$estimate[count] <- "S.i"
    sensi$est.value[count] <- ((B%*%(D[,i]-A))/N)/var.y.2
    sensi$est.PE[count] <- (1.96/sqrt(N)*sqrt((sum((B*(D[,i]-A))^2)/N - (sum((B*(D[,i]-A))/N))^2)))/var.y.2
    count <- count + 1

    sensi$variable.i[count] <- i
    sensi$estimate[count] <- "St.i"
    sensi$est.value[count] <- (sum((A-D[,i])^2)/(2*N))/var.y.1
    sensi$est.PE[count] <- (1.96/sqrt(N)*sqrt(sum(((A-D[,i])^2)^2)/(2*N) - (sum((A-D[,i])^2)/(2*N))^2))/var.y.1
    count <- count + 1

  }

  #Closed Complement Indices
  for(i in 1:k){
    for(j in (i+1):k){
      if(j>k){
        break
      }
      sensi$variable.i[count] <- i
      sensi$variable.j[count] <- j
      sensi$estimate[count] <- "S._ij.c"
      sensi$est.value[count] <- (sum((D[,i]-D[,j])^2)/(2*N))/var.y.2
      sensi$est.PE[count] <- (1.96/sqrt(N)*sqrt(sum(((D[,i]-D[,j])^2)^2)/(2*N) - (sum((D[,i]-D[,j])^2)/(2*N))^2))/var.y.2
      count <- count + 1

    }
  }

  #Only compute Second Order if D output is given


  #Second Order Closed and Second Order Indices
  for(i in 1:k){

    Vi <- ((B%*%(D[,i]-A))/N)

    for(j in (i+1):k){
      if(j>k){
        break
      }

      Vj <- ((B%*%(D[,j]-A))/N)

      Vij.c <- ((D[,i]%*%C[,j])/(N-1) - es.y.2)

      sensi$variable.i[count] <- i
      sensi$variable.j[count] <- j
      sensi$estimate[count] <- "S.ij.c"
      sensi$est.value[count] <- Vij.c/var.y.2
      sensi$est.PE[count] <- (1.96/sqrt(N)*sqrt(sum((D[,i]*C[,j])^2)/N - (D[,i]%*%C[,j]/N)^2))/var.y.2
      count <- count + 1

      sensi$variable.i[count] <- i
      sensi$variable.j[count] <- j
      sensi$estimate[count] <- "S.ij"
      sensi$est.value[count] <- (Vij.c-Vi-Vj)/var.y.2
      sensi$est.PE[count] <- NA
      count <- count + 1

    }
  }

  return(sensi)
}

## Sensitivity Function 4: get.sensitvity.new
# Uses total effects equation from Sal10, and first/second order equations from AB
get.sensitivity.new <- function(A, B, C, D){
  N <- length(A)
  k <- ncol(C)


  #First, total, closed complement, second order closed, and second order indices
  params <- 2*k + 3*((k-1)*k/2)

  sensi <- as.data.frame(matrix(nrow=params, ncol=5))
  colnames(sensi) <- c("variable.i", "variable.j",
                       "estimate", "est.value",
                       "est.PE")
  count <- 1

  es.y.1 <- mean(A)^2
  var.y.1 <- (A%*%A)/N-es.y.1

  es.y.2 <- (A%*%B)/N
  var.y.2 <- (A%*%A)/N-es.y.2

  #First Order and Total Effects
  for(i in 1:k){
    sensi$variable.i[count] <- i
    sensi$estimate[count] <- "S.i"
    sensi$est.value[count] <- ((A%*%C[,i])/(N-1)-es.y.2)/var.y.2
    sensi$est.PE[count] <- (1.96/sqrt(N)*sqrt(sum((A*C[,i])^2)/N - (A%*%C[,i]/N)^2))/var.y.2
    count <- count + 1

    sensi$variable.i[count] <- i
    sensi$estimate[count] <- "St.i"
    sensi$est.value[count] <- (sum((A-D[,i])^2)/(2*N))/var.y.1
    sensi$est.PE[count] <- (1.96/sqrt(N)*sqrt(sum(((A-D[,i])^2)^2)/(2*N) - (sum((A-D[,i])^2)/(2*N))^2))/var.y.1
    count <- count + 1

  }

  #Closed Complement Indices
  for(i in 1:k){
    for(j in (i+1):k){
      if(j>k){
        break
      }
      sensi$variable.i[count] <- i
      sensi$variable.j[count] <- j
      sensi$estimate[count] <- "S._ij.c"
      sensi$est.value[count] <- ((C[,i]%*%C[,j])/(N-1) - es.y.2)/var.y.2
      sensi$est.PE[count] <- (1.96/sqrt(N)*sqrt(sum((C[,i]*C[,j])^2)/N - (C[,i]%*%C[,j]/N)^2))/var.y.2
      count <- count + 1

    }
  }

  #Second Order Closed and Second Order Indices
  for(i in 1:k){

    Vi <- (A%*%C[,i])/(N-1)-es.y.2

    for(j in (i+1):k){
      if(j>k){
        break
      }

      Vj <- (A%*%C[,j])/(N-1)-es.y.2

      Vij.c <- ((C[,i]%*%D[,j])/(N-1) - es.y.2)

      sensi$variable.i[count] <- i
      sensi$variable.j[count] <- j
      sensi$estimate[count] <- "S.ij.c"
      sensi$est.value[count] <- Vij.c/var.y.2
      sensi$est.PE[count] <- (1.96/sqrt(N)*sqrt(sum((C[,i]*D[,j])^2)/N - (C[,i]%*%D[,j]/N)^2))/var.y.2
      count <- count + 1

      sensi$variable.i[count] <- i
      sensi$variable.j[count] <- j
      sensi$estimate[count] <- "S.ij"
      sensi$est.value[count] <- (Vij.c-Vi-Vj)/var.y.2
      sensi$est.PE[count] <- NA
      count <- count + 1

    }
  }

  return(sensi)
}


