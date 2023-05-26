# gobal GAMs UK (ELSA) data

load("ELSA.Rdata")
ELSA <- as.data.frame(ELSA)

#-------------------------------------------------------------------------------
# formulas


# no random
uk_gam_f1 <- score ~ s(time, k = 8)

# random intercepts: time and idauniq
uk_gam_f2 <- score ~ s(time, k = 8) + s(idauniq, bs = "re")

# also random slope
uk_gam_f3 <- score ~ s(time, k = 8) + s(idauniq, bs = "re") + s(idauniq, time, bs = "re")

#-------------------------------------------------------------------------------
# main function

f.gamm.uk <- function( data = ELSA, rep = 10, n = 250, GAM_form = uk_gam_f1 ) {
  
  # create Matrix to save MSE's + tree sizes of each round
  Output <- matrix( nrow = rep, ncol = 1, dimnames = list( c( ), c("MSE") ) )
  for( i in 1:rep ) {
    # draw random sample of IDs & capture them in a vector
    subsample <- sample( unique( data$idauniq ), size = n )
    data$id <- 0L
    for( j in 1:n ) {
      data$id[  as.character( data$idauniq ) %in% subsample[ j ] ] <- 1L 
    }
    # define the training & test set for each CV-round
    Train_sub <- subset( data, data$id == 1L )
    Test_sub <- subset( data, data$id == 0L )
    # build model
    model <- gam( GAM_form, data = Train_sub, method = "REML" )
    # get MSE for this round
    model_pred <- predict.gam( model, type = "response", newdata = Test_sub )
    mse <- mean( ( model_pred - Test_sub$score )^2 )
    # save this round's MSE
    Output[ i, 1 ] <- mse
    # remove old sampling
    rm(subsample)
    data <- subset( data, select = -c( id ) )
    print( i )
  } # for-loop i end
  
  return( Output )
} # f.gamm.uk



#-------------------------------------------------------------------------------
# GAM v. 1
# small N

set.seed(65342234)
gamm1.uk_s <- f.gamm.uk( )
gamm1.uk_s
# saveRDS(gamm1.uk_s, file = "gamm1.uk_s.RDS")

#-------------------------------------------------------------------------------
# GAM v.1
# large N

set.seed(3256)
gamm1.uk_l <- f.gamm.uk( n = 500 )
gamm1.uk_l
# saveRDS(gamm1.uk_l, file = "gamm1.uk_l.RDS")

#-------------------------------------------------------------------------------
# GAM v. 2
# small N

set.seed(1201206)
gamm2.uk_s <- f.gamm.uk( GAM_form = uk_gam_f2 )
gamm2.uk_s
saveRDS(gamm2.uk_s, file = "gamm2.uk_s.RDS")

#-------------------------------------------------------------------------------
# GAM v. 2
# large N

set.seed(6734)
gamm2.uk_l <- f.gamm.uk( n = 500, GAM_form = uk_gam_f2 )
gamm2.uk_l
#saveRDS(gamm2.uk_l, file = "gamm2.uk_l.RDS")


#------------------------------------------------------------------------------
# GAM v. 3
# small N

set.seed(76932843)
gamm3.uk_s <- f.gamm.uk( GAM_form = uk_gam_f3 )
gamm3.uk_s
# saveRDS(gamm3.uk_s, file = "gamm3.uk_s.RDS")

#-------------------------------------------------------------------------------
# GAM v. 3
# large N

set.seed(3483)
gamm3.uk_l <- f.gamm.uk( n = 500, GAM_form = uk_gam_f3 )
gamm3.uk_l



