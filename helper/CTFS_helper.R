# AUTHOR: DEREK MENDEZ (dermendarko@gmail.com)
##################################
# A COLLECTION OF FUNCTIONS USED #
# TO MAKE THE CTFS DATAFRAMES    #
##################################

source('biomass.R')

.helper.makeTreeData <- function(census)
{
##############################
#     MAKE THE TREE FILE     #
##############################
    treeData = census[which(census$mstem ==0 ), ]
    
    treeCols =  c('treeID' ,
                 'stemID',
                 'tag' ,
                 'StemTag' ,
                 'sp',
                 'quadrat' ,
                 'gx' ,           
                 'gy' ,           
                 'MeasureID',
                 'CensusID',
                 'dbh' ,
                 'pom'  ,
                 'hom' ,  
                 'ExactDate' ,
                 'DFstatus',
                 'codes' ,
                 'nostems' ,   
                 'status',
                 'date',
                 'agb',
                 'RawStatus') # the raw status is for us to selectively compute growth
    treeData = treeData[treeCols]
    return(treeData)
}


.helper.makeStemData <- function(census )
{
##############################
#     MAKE THE STEM FILE     #
##############################
    census$countPOM = rep( 1, nrow(census) ) # only considering one POM for these data
    
    stemCols =  c('treeID' ,
                'stemID',
                 'tag' ,
                 'StemTag' ,
                 'sp',
                 'quadrat' ,
                 'gx' ,           
                 'gy' ,           
                 'MeasureID',
                 'CensusID',
                 'dbh' ,
                 'pom'  ,
                 'hom' ,  
                 'ExactDate' ,
                 'DFstatus',
                 'codes' ,
                 'countPOM',
                 'status',
                 'date',
                 'agb')

    stemData = census[stemCols]
    return(stemData)
}


.helper.convert_types <- function( census) 
{
    integerCols  = c('treeID',   
                'mstem',  
                'CensusID')
    numericCols  = c('dist_to_nail',
                'gx',
                'gy',
                'date',
                'agb',
                'hom', 
                'nostems', 
                'dbh')

    census[integerCols] = lapply( census[integerCols], as.integer )
    census[numericCols] = lapply( census[numericCols], as.numeric )
    
    return(census)
}


.helper.make_int <- function( df, integerCols ) 
{
    df[integerCols] = lapply( df[integerCols], as.integer )
    return(df)
}


.helper.calcAGB <- function( census,forest )
{
    {
    if (forest=='wet')
        AGB_map = .biomass.Lau_AGB
    else if( forest=='dry')
        AGB_map = .biomass.Pn_AGB 
    }
    species  = census$sp
    diameter = census$dbh

    non_na_rows = which( !is.na(diameter) & !is.na(species) )
    for ( i in non_na_rows )
    {
        sp          = species[i]
        sp_info     = AGB_map[[sp]]
        density     = sp_info$"density" #g / cm^3
        dbh         = diameter[i] # cm
       
        height_coef = sp_info$"ht_coef"
        height_mdl  = sp_info$"ht_model"
        #print(sp, sp_info, dbh, density, height_coef, height_mdl)
        height      = do.call(height_mdl, c(height_coef,'diam'= dbh)  ) # meters
        
        agb_coef    = sp_info$"agb_coef"
        agb_mdl     = sp_info$"agb_model"

        agb         = do.call( agb_mdl, c(agb_coef, 'diam'=dbh, 'ht'=height, 'rho'=density) ) # kg
        census[ i, 'agb' ] = agb/1000 # we want AGB in Mega-grams
    }
    return (census)
}

# takes a dataframe with columns x,y,elev
.helper.makeElevMatrix <- function( data )
{
    ux         = unique( data$x )
    uy         = unique( data$y )
    num_x      = length ( ux)
    num_y      = length ( uy)
    mat        = matrix( 0, nrow= num_y, ncol = num_x)
    for (i in 1:num_x)
    {
        for (j in 1:num_y)
        {
            x        = ux[i]
            y        = uy[j]
            idx      = which( data$x==x & data$y==y  )
            mat[j,i] = data$elev[ idx]
        }
    }
    return(mat)
}
