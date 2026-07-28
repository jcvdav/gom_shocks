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

pacman::p_load(
  tidyverse
)

# PROCESSING ###################################################################

## Define Parameters -------------------------------------------------------------------
p <- 8 # Price was $4 per lb, this is now $8 per kilo
q <- 1e-5 # Make it super small
beta <- 2
c <- 10
X <- 800 #assume thre are 800 tons of shrimp

E_star <- function(p, q, X, beta, c){
  ((p*q*X)/(beta*c))^((1)/beta*1)
}

E_star(p=p, q=q, beta=beta, c=c, X=X)
# VISUALIZE ####################################################################
XX <-  numeric(length = 100)
XX[1] <-  X
X_t <- XX[1]

for(t in 2:100){
  #Step 1, identify the level of effort at time t
  E_t <- E_star(p=p, q=q, X=X_t, beta=beta, c=c)
  
  #Step 2, identifyy total harvest given level of effort
  H_t <- q*E_t*X
    
  #Step 3, escapement 
    S_t <-  X_t - H_t
  #Step 4, growth
    X_t <- 1.1*S_t
    #Save for future use
    XX[t] <- X_t
}

plot(XX)

## Another step ----------------------------------------------------------------

# EXPORT #######################################################################


## The final step --------------------------------------------------------------  