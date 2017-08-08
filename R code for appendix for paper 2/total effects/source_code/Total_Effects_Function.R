# 
# Sensitivity Measures for Sensi Index, Total Effects,
# complements of (k-2) order effects (S(-ij)^c), 
# second order closed (S.ij.c) and second order (S.ij) effects
#
# Paige Erspamer 07/18/2014
#
# Based on Saltelli, A. (2002). Making best use of model evaluations to 
#         compute sensitivity indices. Computer Physics Communications, 
#         (2), 280-297.
#     Saltelli (2010) Variance based sensitivity analysis of model output. Design and estimator
#           for the total sensitivity index
#     Saltelli (2008) Global Sensitivity Analysis: The Primer



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


