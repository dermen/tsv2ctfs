
##############################
# GROWTH EXTENSION FUNCTIONS #
##############################

.growth.isOutlier <- function (points, thresh)
{
#   References:
#   ----------
#   Boris Iglewicz and David Hoaglin (1993), "Volume 16: How to Detect and
#   Handle Outliers", The ASQC Basic References in Quality Control:
#   Statistical Techniques, Edward F. Mykytka, Ph.D., Editor. 
    med               = median(points)
    diff              = sqrt( (points - med)^2)
    med_abs_deviation = median(diff)

    modified_z_score   = 0.6745 * diff / med_abs_deviation
    return ( which(modified_z_score > thresh) )
}

prepGrowth <- function( censusA, censusB, min_delta=-.4, species_filter= c(), thresh=3.5, bad_trees = c()  )
{
#   ARGUMENTS 
#   ==========       
#   censusA,censusB ( CTFS tree/full-style dataframes made with tsv2ctfs the function)
#   IMPORTANT: censusA should pre-date censusB, i.e. censusB is a recensus of censusA
#   min_delta ( minimum dbh difference between two growth years for a given tree, in same units as dbh )
#   species_filter  ( a vector or species labels (e.g. CIBMEN) )
#   bad_trees ( a integrer vector of bad tree ID numbers )
#   thresh ( a threshold for outlier removal; lower thresh means more outliers removed  ) 

#   RETURN 
#   ======       
#   a list of the growth-filtered censusA and censusB 
#   reference each with "$censusA" and "$censusB"


    # dataframes must be aligned with  treeID 


    stopifnot(  length( which( is.na(censusA$treeID) ) ) == 0 ) 
    stopifnot(  length( which( is.na(censusB$treeID) ) ) == 0 ) 

    ##########################
    # REMOVE CERTAIN SPECIES #
    ##########################
    if (length( species_filter) > 0 )
    {
        for (sp in species_filter)
        {
            bad_treesA = censusA$treeID[ which(censusA$sp==sp) ]
            bad_treesB = censusA$treeID[ which(censusA$sp==sp) ]
            bad_trees = c(bad_trees,  bad_treesA, bad_treesB )
        }
    }

    ###############################
    # SELECT ONLY THE ALIVE TREES #
    ###############################
    growth_inds = which( censusA$RawStatus=='alive' & censusB$RawStatus=='alive' )
    if (length( growth_inds ) == 0 )
    {
        cat('There are zero trees alive in censusA and censusB. Check your RawStatus column!')
    }
    stopifnot( length(growth_inds) > 0 )
    
    censusA = censusA[growth_inds, ]
    censusB = censusB[growth_inds, ]

    ######################################
    # REMOVE GROWTH OUTLIERS PER SPECIES #
    ######################################
    cat('\n    Beginning species-specific outlier detection...\n')
    cat('    =============================================== ')
    u_sp        = unique( censusA$sp )

    outlier_inds = c()
    for (sp in u_sp)
    {
        cat('\n')
        indsA = which( censusA$sp == sp )
        indsB = which( censusB$sp == sp ) # should be the same
        stopifnot( identical(indsA,indsB) )
       
        sp_inds = indsA # same as indsB

        dbhA  =  censusA[sp_inds,'dbh']
        dbhB  =  censusB[sp_inds,'dbh']
        delta = dbhB - dbhA
        
        inds  = which(!is.na(delta) ) # in case there are any NA still for whatever reason
        if ( length(inds) == 0 ) 
            next
        
        delta        = delta[ inds]
        sp_inds      = sp_inds[ inds]
        below_min    = which( delta < min_delta ) 
        
        if (length(below_min)==length(delta))
        {
            outlier_inds = c(outlier_inds, sp_inds[below_min] )
            next
        }
        
        if( length(below_min) >0 )
        {
            outlier_inds = c(outlier_inds, sp_inds[below_min] )
            delta        = delta[ -below_min]
            sp_inds      = sp_inds[ -below_min]
        }
        
        outliers = .growth.isOutlier( delta, thresh=thresh)
        cat(sprintf('    %s: removed %d outliers (%.2f %%)',
                         sp , length(outliers) ,
                         100*length(outliers)/length(delta)  ))
        
        outlier_inds = c( outlier_inds, sp_inds[outliers] )
    }
    cat('\n')

    censusA = censusA[ -outlier_inds, ]
    censusB = censusB[ -outlier_inds, ]

    ##########################
    # REMOVE CERTAIN TREEIDs #
    ##########################
    bad_trees = unique( bad_trees )
    if (length( bad_trees ) > 0 )
    {
        id_inds = c()
        for (id in bad_trees )
            id_inds = c(id_inds, which( censusA$treeID == id ) )
        censusA = censusA[ -id_inds, ] 
        censusB = censusB[ -id_inds, ] 
    } 
    
    # assert that censusA and censusB are still row-aligned  
    stopifnot( identical(censusA$treeID, censusB$treeID) )
    
    return ( list( "censusA"=censusA, "censusB"=censusB ) )
}

