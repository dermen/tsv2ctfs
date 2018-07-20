
# author: Derek Mendez (dermendarko@gmail.com)

.tsv2ctfs.baseDir = getwd()
setwd( .tsv2ctfs.baseDir )
helperFile = file.path(.tsv2ctfs.baseDir,'helper/CTFS_helper.R')
source(helperFile, chdir=TRUE)
rm(helperFile)

# get the split.data function from CTFSRPackage, then detach
if (!exists("split.data"))
    attach("CTFSRPackage.rdata")
.tsv2ctfs.split.data = split.data

tsv2ctfs <- function( plotName='Laupahoehoe',
                      master_fileName='Laupahoehoe_master.txt',
                      taxonomy_fileName = 'Laupahoehoe_taxonomy.txt', 
                      elevation_fileName=NULL,
                      forest='wet' )
{

# NOTE: when providing your own files, please place them in the data/ subfolder and only pass the basenames to this function. 

# ARGUMENTS
# ==========
# plotName: prefix that will be applied to all of the ctfs R data files
# master_fileName: tsv version of the master dataset 
#                   (use excel to open the xlsx files and save as tsv, then put in the data/ subfolder)
# taxonomy_fileName: tsv version of the taxoomy info 
#                   (see existing file data/Laupahoehoe_taxonomy.txt for template 
# elevation_fileName: tsv version of the elevation data for the plot (
#                   should be on a regular 2d grid spanning the plot (see data/Laupahoehoe_master.txt) 
# forest: must be either 'wet' or 'dry'. It is a selector for different tree height and biomass models

    stopifnot( forest %in% c("wet","dry") )

#########################
# SET UP FILE FRAMEWORK #
#########################
    fullDir    = file.path( .tsv2ctfs.baseDir, 'full')
    stemDir    = file.path( .tsv2ctfs.baseDir, 'stem')
    speciesDir = file.path( .tsv2ctfs.baseDir, 'species')
    splitDir   = file.path( .tsv2ctfs.baseDir, 'split')
    elevDir    = file.path( .tsv2ctfs.baseDir, 'elev')
    
    dataDir    = file.path( .tsv2ctfs.baseDir, 'data')
    dir.create(fullDir)
    dir.create(stemDir)
    dir.create(speciesDir)
    dir.create(splitDir)
    dir.create(elevDir)


    master_fileName    = file.path( dataDir, master_fileName)
    taxonomy_fileName  = file.path( dataDir, taxonomy_fileName)
    elevation_fileName = file.path( dataDir, elevation_fileName)

##################
# NAMING SCHEMES #
##################
    fullPrefix    = paste0( plotName,'.full')
    splitPrefix   = paste0( plotName,'.split')
    stemPrefix    = paste0( plotName,'.stem')
    sppTableName  = paste0( plotName, '.spptable')
    elevTableName = paste0( plotName, '.elev')

#####################
# READ THE TSV FILE #
#####################
    censusData           = read.delim( master_fileName, sep='\t' , na.strings=c('NULL', 'NA'), colClasses='character') 
    censusData$MeasureID = c( 1:nrow(censusData) )
    censusData           = .helper.convert_types( censusData )
    cat("    Calculating above ground biomass (if you have a weak computer this might take some time) ...\n")
    censusData           = .helper.calcAGB( censusData, forest)

################################
# MAKE THE TREE AND STEM FILES #
################################
    allCensusID = unique( censusData$CensusID )


    
    for (n in allCensusID )
    {
        sometext = sprintf("    Making full,stem, and split dataframes for census number %d...\n", n )
        cat(sometext)
        
        censusData_ = censusData[ which(censusData$CensusID == n), ]
        
        full        = .helper.makeTreeData( censusData_ )
        stem        = .helper.makeStemData( censusData_ )
        split       = .tsv2ctfs.split.data( full, splitcol='sp')
        
        fullName  = paste0( fullPrefix,  as.character(n) )
        stemName  = paste0( stemPrefix,  as.character(n) )
        splitName = paste0( splitPrefix, as.character(n) )

        assign( fullName,  full)
        assign( stemName,  stem)
        assign( splitName, split)
        
        save( list=fullName,  file=file.path(fullDir, paste0(fullName,'.rdata'))   )
        save( list=stemName,  file=file.path(stemDir, paste0(stemName,'.rdata'))   )
        save( list=splitName, file=file.path(splitDir, paste0(splitName,'.rdata')) )
    }
    rm(censusData_)
    rm(full)
    rm(stem)
    rm(split)
    rm(fullName)
    rm(stemName)
    rm(splitName)
    rm(n)
    rm(allCensusID)

###########################
# MAKE THE TAXONOMY TABLE #
###########################
    cat("    Making taxonomy dataframe...\n")
    sppTable = read.delim( taxonomy_fileName,sep='\t',na.strings='NA', colClasses='character')
    assign( sppTableName, sppTable)
    save( list=sppTableName, file=file.path( speciesDir, paste0(sppTableName ,'.rdata')) )
    rm(sppTable)

#################################
# MAKE THE PLOT ELEVATION TABLE #
#################################
# this should be data on a regular grid, with the columns "x","y" and "elev". Units should be meters
    if (length(elevation_fileName) > 0 )
    {
        cat("    Making elevation dataframe...\n")
        elevData   = read.delim( elevation_fileName, sep='\t' , colClasses='numeric') 
        elevData   = .helper.make_int( elevData, c('x', 'y') )
        elevMat    = .helper.makeElevMatrix( elevData )
        elevData   = list('col'=elevData ,'mat'=elevMat,  'xdim'=max(elevData$x), 'ydim'=max(elevData$y))
        assign( elevTableName, elevData)
        save( list=elevTableName, file=file.path(elevDir,paste0(elevTableName ,'.rdata')) )
        rm(elevData)
        rm(elevMat)
    }

# ADDITIONAL CLEAN-UP
    rm(fullPrefix)
    rm(splitPrefix)
    rm(stemPrefix)
    rm(sppTableName)
    rm(elevTableName)

    rm(fullDir)
    rm(stemDir)
    rm(speciesDir)
    rm(splitDir)
    rm(elevDir)
    
    cat("    NOTE:\n")
    cat("    If warnings are thrown because directories already exist, disregard.\n")
}




