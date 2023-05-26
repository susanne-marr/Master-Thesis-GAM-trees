#                              building blocks Marjolein

#------------------------------------
library(gamtree) # Marjolein's
library(partykit)
library(gamm4)
library(merDeriv)
library(foreign)
library(mgcv)
library(glmertree)


#------------------------------------
# gamfit function
gamfit <- function( y, x, start = NULL, weights = NULL, offset = NULL, 
                    estfun = NULL, object = NULL, ..., local_gam_form,
                    gam_ctrl = list() ) {
  
  args <- list(...)
  
  if ( is.null(x) ) 
    x <- matrix( 1, nrow = NROW(y), ncol = 1L,
                 dimnames = list( NULL, "(Intercept)") )
  
  args <- c( list( formula = local_gam_form, data = cbind(x, y),
                   weights = weights, offset = offset, control = gam_ctrl),
             args )
  
  object <- do.call( "gamm4", args ) # 1. calls function gamm4, 2. list of arguments, as defined above, for function call
  class(object) <- "gamm4"
  list(
    object = object,# model object for which further methods could be available
    coefficients = list( fixef = fixef(object$mer), ranef = VarCorr(object$mer)),
    objfun = lme4:::logLik.merMod(object$mer), # minimized objective function
    estfun = merDeriv::estfun.lmerMod(object$mer, level = 1) # empir. estimating functions / scores
  )
  
}



#------------------------------------
# functions
mob_ctrl <- mob_control( xtype = "data.frame", ytype = "data.frame", 
                         parm = c(1, 2, 4))
# as long as have ONE predictor ! --> stick to that, one predictor
gam_ctrl <- lmerControl()



#------------------------------------
# predict function 
predict.gamm4 <- function(object, ...) {
  predict(object$gam, ...)
}

