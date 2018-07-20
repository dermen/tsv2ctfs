# LOAD THE TOOLS PACKAGE
library(tools)

#######################
# LOAD THE CTFS TOOLS #
#######################
cat("    Loading the CTFS-R package tools....\n")
{
  if ( grepl("CTFSRPackage.rdata", paste(search(),collapse='')) )
      cat("    (using previously loaded CTFS R package tools)\n")
  else
      attach('CTFSRPackage.rdata')
}
cat("\n")

##################################
# LOAD THE "prepGrowth" function #
##################################
source( 'CTFS_extensions.R' )

####################
# PRINT SOME NOTES #
####################
cat("    NOTES:\n")
cat("    Eelvation data for Palamanui is currently missing...\n")
cat("\n")

#############################################
# LOAD THE FILES THAT COME WITH THE PACKAGE #
#############################################
cat("    LOADING FILES:\n")
files =   c('full/Laupahoehoe.full1.rdata', 
            'full/Laupahoehoe.full2.rdata', 
            'full/Laupahoehoe.full3.rdata', 
            'full/Laupahoehoe.full4.rdata', 
            'full/Laupahoehoe.full5.rdata', 
            'full/Sanctuary.full1.rdata', 
            'full/Sanctuary.full2.rdata', 
            'full/Sanctuary.full3.rdata', 
            'full/Mamalahoa.full1.rdata', 
            'full/Mamalahoa.full2.rdata', 
            'full/Mamalahoa.full3.rdata', 
            'full/Mamalahoa.full4.rdata', 
            'full/Palamanui.full1.rdata', 
            'full/Palamanui.full2.rdata', 
            'full/Palamanui.full3.rdata', 
            'full/Palamanui.full4.rdata', 
            'full/Palamanui.full5.rdata', 
            'stem/Laupahoehoe.stem1.rdata', 
            'stem/Laupahoehoe.stem2.rdata', 
            'stem/Laupahoehoe.stem3.rdata', 
            'stem/Laupahoehoe.stem4.rdata', 
            'stem/Laupahoehoe.stem5.rdata', 
            'stem/Palamanui.stem1.rdata', 
            'stem/Palamanui.stem2.rdata', 
            'stem/Palamanui.stem3.rdata', 
            'stem/Palamanui.stem4.rdata', 
            'stem/Palamanui.stem5.rdata', 
            'stem/Sanctuary.stem1.rdata', 
            'stem/Sanctuary.stem2.rdata', 
            'stem/Sanctuary.stem3.rdata', 
            'stem/Mamalahoa.stem1.rdata', 
            'stem/Mamalahoa.stem2.rdata', 
            'stem/Mamalahoa.stem3.rdata', 
            'stem/Mamalahoa.stem4.rdata', 
            'split/Laupahoehoe.split1.rdata', 
            'split/Laupahoehoe.split2.rdata', 
            'split/Laupahoehoe.split3.rdata', 
            'split/Laupahoehoe.split4.rdata', 
            'split/Laupahoehoe.split5.rdata', 
            'split/Palamanui.split1.rdata', 
            'split/Palamanui.split2.rdata', 
            'split/Palamanui.split3.rdata', 
            'split/Palamanui.split4.rdata', 
            'split/Palamanui.split5.rdata', 
            'split/Sanctuary.split1.rdata', 
            'split/Sanctuary.split2.rdata', 
            'split/Sanctuary.split3.rdata', 
            'split/Mamalahoa.split1.rdata', 
            'split/Mamalahoa.split2.rdata', 
            'split/Mamalahoa.split3.rdata', 
            'split/Mamalahoa.split4.rdata', 
            'species/Sanctuary.spptable.rdata', 
            'species/Laupahoehoe.spptable.rdata', 
            'species/Palamanui.spptable.rdata',
            'species/Mamalahoa.spptable.rdata',
            'elev/Laupahoehoe.elev.rdata', 
            'elev/Palamanui.elev.rdata', 
            'elev/Sanctuary.elev.rdata', 
            'elev/Mamalahoa.elev.rdata')

files = sapply( files, function(x) file.path(getwd(), x), USE.NAMES=FALSE) 
for (file in files)
{
    load( file )
    dataName = basename( file_path_sans_ext(file) )
    someText = paste( '    loaded dataframe:',  dataName,'\n')
    cat ( someText )
}

#cleanup
rm(file,files,dataName,someText)

cat("    DONE.\n")

