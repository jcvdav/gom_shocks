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
  pacman::p_load(tidyverse,
                 cowplot)
  
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
  
  simulate <- function(p, q, beta, c, X, r, K, shock_t=Inf, which="", shock=1){
    p_original <- p
    q_original <- q
    r_original <- r
    c_original <- c
    
    #Step 0, Build one vector of length 100 fir each state variable
    E_t <- H_t <- S_t <- X_t <-  numeric(length = 100)
    X_t[1] <-  X # Make the population at time 1 be equal to X
    
    
    for(t in 1:100){
      #Identify whether this timestep has a shock
      if(t==shock_t){
        if(which=="q"){q <- shock*q}
        if(which=="p"){p <- shock*p}
        if(which=="r"){r <- shock*r}
        if(which=="c"){c <- shock*c}
        if(which=="X"){X_t[t] <- shock*X_t[t]}
      } else {
        q <- q_original
        p <-p_original
        r <- r_original
        c <- c_original
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

##Shock q (Drought)
shock_q <- simulate(p=p, q=q, beta=beta, c=c, X=data1$X[100], r=r, K=K, shock_t=10,
                  which="q", shock=1 + (-58.3/100))
  
p_X <- ggplot(data=shock_q, 
       mapping= aes(x=time, y=X_t))+
  geom_line()

p_H <- ggplot(data=shock_q, 
              mapping= aes(x=time, y=H_t))+
  geom_line()

plot_grid(p_X, p_H, ncol = 1)

###Shock q (hypoxia) 
shock_q <- simulate(p=p, q=q, beta=beta, c=c, X=data1$X[100], r=r, K=K, shock_t=10,
                    which="q", shock=1 + (-20/100))

ggplot(data=shock_q, 
       mapping= aes(x=time, y=X_t))+
  geom_line() + lims(y=c(0,K))

## Shock p (Red Tide)
shock_p <- simulate(p=p, q=q, beta=beta, c=c, X=data1$X[100], r=r, K=K, shock_t=10,
                  which="p", shock=1 + (-25/100))

ggplot(data=shock_p, 
       mapping= aes(x=time, y=X_t))+
  geom_line() + 
  lims(y=c(0,K))

##Shock c (Hypoxia) 
shock_c <- simulate(p=p, q=q, beta=beta, c=c, X=data1$X[100], r=r, K=K, shock_t=10,
                    which="c", shock=1 + (11.1/100))

ggplot(data=shock_c, 
       mapping= aes(x=time, y=X_t))+
  geom_line() + 
  lims(y=c(0,K))

## Shock r (Red Tide)
shock_r <- simulate(p=p, q=q, beta=beta, c=c, X=data1$X[100], r=r, K=K, shock_t=10,
                    which="r", shock=1 + (31.5/100))

ggplot(data=shock_r, 
       mapping= aes(x=time, y=X_t))+
  geom_line() + 
  lims(y=c(0,K))

##Shock r (Hypoxia)
shock_r <- simulate(p=p, q=q, beta=beta, c=c, X=data1$X[100], r=r, K=K, shock_t=10,
                            which="r", shock=1 + (28.6/100))

ggplot(data=shock_r,
       mapping= aes(x=time, y=X_t))+
  geom_line() +
  lims(y=c(0,K))




# Red Tide X
shock_X <- simulate(p=p, q=q, beta=beta, c=c, X=data1$X[100], r=r, K=K, shock_t=10,
                              which="X", shock=1 + (-16.5/100))
  
ggplot(data=shock_X,
        mapping= aes(x=time, y=X_t))+
    geom_line() +
    lims(y=c(0,K))

# Shock p (River Discharge)
shock_p <- simulate(p=p, q=q, beta=beta, c=c, X=data1$X[100], r=r, K=K, shock_t=10,
                                   which="p", shock=1 + (-17.6/100))

ggplot(data=shock_p,
       mapping= aes(x=time, y=X_t))+
  geom_line() +
  lims(y=c(0,K))


  # EXPORT #######################################################################
  
  
  ## The final step --------------------------------------------------------------  