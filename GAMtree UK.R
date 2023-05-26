# GAM tree UK data

load("ELSA.Rdata")
ELSA <- as.data.frame(ELSA)


#-------------------------------------------------------------------------------
# formulas

tf_uk <- score ~ time | disex + dimar + hesmk + psold + edqual + scchd +
  indager + asoccls
  
lgf_uk <- score ~ s(time, k = 5)

#-------------------------------------------------------------------------------
# main function 

f.gammtree.uk <- function( data = ELSA, rep = 10, n = 250, min = 50 ) {
  
  # create Matrix to save MSE's + tree sizes of each round
  Output <- matrix( nrow = rep, ncol = 2, dimnames = list( c( ), c( "MSE", "tree.size" ) ) )
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
    # remove test observations with factor level(s) not in training data
    for (fact in c("dimar", "edqual", "scchd", "asoccls", "disex", "hesmk"
                   )) { ## supply factors with rare levels
      levs <- unique(Test_sub[ , fact]) %in% unique(Train_sub[ , fact])
      if (!all(levs)) {
        Test_sub <- Test_sub[-which(!Test_sub[ , fact] %in% unique(Test_sub[ , fact])[levs]), ] ## remove observations/rows from test data
        print(paste0("Levels of ", fact, " omitted from test data: ",
                     unique(Test_sub[ , fact])[!levs]))
      }     
    }
    # build tree
    tree <- mob( tf_uk, data = Train_sub, local_gam_form = lgf_uk, fit = gamfit,
                 cluster = idauniq, control = mob_ctrl, gam_ctrl = gam_ctrl,
                 # minsize = min, maxdepth = 5
                 )
    # get MSE for this round
    tree_pred <- predict( tree, type = "response", newdata = Test_sub )
    mse <- mean( ( tree_pred - Test_sub$score )^2 )
    # save this round's MSE
    Output[ i, 1 ] <- mse
    # save this round's tree size
    Output[ i, 2 ] <- length( tree )
    # remove old sampling
    rm(subsample)
    data <- subset( data, select = -c( id ) )
    print( i )
  } # for-loop i end
  return( Output )
  
} # f.gammtree.uk


#----------------------------
# small N
set.seed(7121212)
gamtree_uk_s <- f.gammtree.uk()
gamtree_uk_s
# saveRDS(gamtree_uk_s, file = "gamtree_uk_s.RDS")

#----------------------
# large N
set.seed(6742)
gamtree_uk_l <- f.gammtree.uk( n = 500, min = 100 )
gamtree_uk_l
