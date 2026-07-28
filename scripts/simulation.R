  ################################################################################
  # Simulations
  ################################################################################
  #
  # Your Name Here
  # Your email here
  # date
  #
  # Description
  #
  ################################################################################
  
  # SET UP #######################################################################
  
  ## Load packages ---------------------------------------------------------------
  pacman::p_load(tidyverse)
  
  # PROCESSING ###################################################################
  
  ## Define Parameters -------------------------------------------------------------------
  #Social economic parameters
  p <- 8 # Price was $4 per lb, this is now $8 per kilo
  q <- 1e-3 # Make it super small
  beta <- 1.3
  c <- 0.1
  
  #Ecological Parameters
  X <- 1000 #assume thre are 800 tons of shrimp
  r <- 0.1
  K <- 1000
  
  E_star <- function(p, q, X, beta, c){
    ((p*q*X)/(beta*c))^((1)/beta*1)
  }
  
  growth <- function(r, K, S){
    S + r*(1-(S/K))*S
  }
  
  E_star(p=p, q=q, beta=beta, c=c, X=X)
  
  growth(r=r, K=K, S=500)
  
  simulate <- function(p, q, beta, c, X, r, K, shock_t=Inf, which=NULL, shock=1){
    p_original <- p
    q_original <- q
    r_original <- r

    
    #Step 0, Build one vector of length 100 fir each state variable
    E_t <- H_t <- S_t <- X_t <-  numeric(length = 100)
    X_t[1] <-  X # Make the population at time 1 be equal to X
    
    
    for(t in 1:100){
      #Identify whether this timestep has a shock
      if(t==shock){
        if(which=="q"){q <- shock*q}
        if(which=="p"){p <- shock*p}
      } else {
        q <- q_original
      }
      #Step 1, identify the level of effort at time t
      E_t[t] <- E_star(p=p, q=q, X=X_t[t], beta=beta, c=c)
      
      #Step 2, identify total harvest given level of effort
      H_t[t] <- q*E_t[t]*X_t[t]
      
      #Step 3, escapement 
      S_t[t] <-  X_t[t] - H_t[t]
      #Step 4, growth
      X_t[t+1] <- growth(r=r, K=K, S=S_t[t])
    }
    X_t <- X_t[1:100]
    time <- 1:100
    
    #Step 5, Put together a data.frame
    data <- data.frame(time, X_t, E_t, H_t, S_t) 
    
    #Step 6, return the data 
    return(data)
  }
 
data1 <- simulate(p=p, q=q, beta=beta, c=c, X=X, r=r, K=K)

ggplot(data=data1, 
       mapping= aes(x=time, y=X_t))+
  geom_line() + 
  lims(y=c(0,K))

data2 <- simulate(p=p, q=q, beta=beta, c=c, X=data1$X[100], r=r, K=K, shock_t=10,
                  which="q", shock=0.3)
  
ggplot(data=data2, 
       mapping= aes(x=time, y=X_t))+
  geom_line() 
  # EXPORT #######################################################################
  
  
  ## The final step --------------------------------------------------------------  