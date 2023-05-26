# GLMM tree UK data

load("ELSA.Rdata")
ELSA <- as.data.frame(ELSA)

#-------------------------------------------------------------------------------
# main function

f.GLMMtree_uk <- function( data = ELSA, rep = 10, n = 250, min = 50 ) {
  
  # create Matrix to save MSE's + tree sizes of each round
  Output <- matrix( nrow = rep, ncol = 2, dimnames = list( c(), c( "MSE", "tree.size" ) ) )
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
    tree <- lmertree( score ~ time | idauniq | disex + dimar + hesmk + psold + 
                        edqual + scchd + indager + asoccls,
                      cluster = idauniq, data = Train_sub, minsize = min, maxdepth = 5)
    # get MSE for this round
    tree_pred <- predict( tree, type = "response", newdata = Test_sub, 
                          re.form = NA  )
    mse <- mean( ( tree_pred - Test_sub$score )^2 )
    # save this round's MSE
    Output[ i, 1 ] <- mse
    # save this round's tree size
    Output[ i, 2 ] <- as.numeric(summary(tree)[1])
    # remove old sampling
    rm(subsample)
    data <- subset( data, select = -c( id ) )
    print( i )
  } # for-loop i end
  return( Output )
  
} # f.GLMMtree_uk


#--------------------
# small N
set.seed(07167645)
uk_glmmtree_s <- f.GLMMtree_uk( )
uk_glmmtree_s
# saveRDS(uk_glmmtree_s, file = "uk_glmmtree_s.RDS")

#---------------------
# large N

set.seed(7845)
uk_glmmtree_l <- f.GLMMtree_uk( n = 500, min = 100 )
uk_glmmtree_l
# saveRDS(uk_glmmtree_l, file = "uk_glmmtree_l.RDS")

#---.