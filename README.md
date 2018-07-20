###The tsv2ctfs repository

##Contents 

* starter.R
  * A function which loads all of the pre-computed data
* CTFS\_extensions.R
  * Loads the prepGrowth function
  * This filters the HIPPNET data so it can be used with the CTFSRPackage growth functions (see below)
* tsvctfsR.R
  * Sourcing this file will load the function tsv2ctfs which is used for converting TSV files into the CTFS style R dataframes described [here](http://ctfs.arnarb.harvard.edu/Public/CTFSRPackage/index.php/web/data_format)
  * This repo comes with pre-computed R dataframes which are saved in the subdirectories within, therefore tsv2ctfsR should only be executed when the databases are updated
* helper/CTFS\_helper.R
  * contains functions used by tsv2ctfsR.R
* helper/biomass.R
  * Contains functions and info for computing the above-ground biomass of each tree. 
  * If you add a tree species to any census, this file will need to be updated
* CTFSRPackage.rdata
  * contains all of the CTFS R functions
* CTFSRPackage/
  * Source code for the R functions, in case you want to modify any of them. Note, you should give new names to functions which are modified
* full/
  * Contains the main stem data frames described [here](http://ctfs.arnarb.harvard.edu/Public/CTFSRPackage/index.php/web/data_format/tree_data)
* stem/
  * Contains the all-stem dataframes described [here](http://ctfs.arnarb.harvard.edu/Public/CTFSRPackage/index.php/web/data_format/stem_data)
* split/
  * Contains the data frames wplit into lists according to species. These are used by some CTFSRPackage functions. 
  * The CTFSRPackage function used to compute them is *split.data*
* species/
  * Contains the species dataframes described [here](http://ctfs.arnarb.harvard.edu/Public/CTFSRPackage/index.php/web/data_format/species_data)
* elev/
  * Contains the elevation dataframes described [here](http://ctfs.arnarb.harvard.edu/Public/CTFSRPackage/index.php/web/data_format/elevation_data)
* data/
  * Contains all of the data files. There are three kinds
    * master TSV files
    * taxonomy TSV files
    * elevation TSV files
  * Navigate to each file within the github website to preview it. The column naming schemes here are **ESSENTIAL**. 
  * We are currenty lacking elevation data for Palamanui

##Loading the pre-computed HIPPNET data into R

1. Download th tsv2ctfs repo [here](https://github.com/treelover/tsv2ctfs/archive/master.zip), or using the **Download Zip** button to the right. 
2. Unzip and move the folder *tsv2ctfs* to a convenient location.
3. Open the R terminal and execute the following commands to source the **beststarter.R** file

```r
# on my computer I unzipped the folder in /Users/mender/CTFS/treelover/, hence
> pathToStarter = "/Users/mender/CTFS/treelover/tsv2ctfs/starter.R"
# if for some reason you do no know your path, you can use
> getwd()
[1] "/Users/mender"
# to see the basic structure onf your computer and go from there
# If you really are struggling with this, you should read up on directory paths, but in the meantime try
> pathToStarter = list.files(path = normalizePath("~"), pattern = "starter.R", recursive = TRUE)
> pathToStarter
[1] "CTFS/starter.R"                    "CTFS/treelover/tsv2ctfs/starter.R"
[3] "CTFS/tsv2ctfs/starter.R" 
> pathToStarter = sapply( pathToStarter, normalizePath, USE.NAMES=FALSE)
> pathToStarter
[1] "/Users/mender/CTFS/starter.R"                    "/Users/mender/CTFS/treelover/tsv2ctfs/starter.R"
[3] "/Users/mender/CTFS/tsv2ctfs/starter.R"  
# and select the one you just unzipped
> pathToStarter = pathToStarter[2]
```

The **list.files** command should work on Mac and Linux, but I am not sure if it will work on Windows... Anyhow, once you have the path, source it:

```r
# be sure to change directories, as the starter.R will in turn source some files
> source( pathToStarter,chdir=TRUE )
    Loading the CTFS-R package tools....

    NOTES:
    Eelvation data for Palamanui is currently missing...

    LOADING FILES:
    loaded dataframe: Laupahoehoe.full1 
    loaded dataframe: Laupahoehoe.full5 
    loaded dataframe: Palamanui.full1 
    loaded dataframe: Palamanui.full5 
    loaded dataframe: Laupahoehoe.stem1 
    loaded dataframe: Laupahoehoe.stem5 
    loaded dataframe: Palamanui.stem1 
    loaded dataframe: Palamanui.stem5 
    loaded dataframe: Laupahoehoe.split1 
    loaded dataframe: Laupahoehoe.split5 
    loaded dataframe: Palamanui.split1 
    loaded dataframe: Palamanui.split5 
    loaded dataframe: Laupahoehoe.spptable 
    loaded dataframe: Palamanui.spptable 
    loaded dataframe: Laupahoehoe.elev 
    DONE.
``` 
Now you have loaded all of the dataframes, and can start using the CTFS R Package functions.

## Converting TSV files to CTFS dataframes using tsv2ctfs

The main HIPPNET database files are stored in the **data/** subfolder. They are in tab-delimited (TSV) format. These files must be processed into R style dataframes to be used with the CTFS R package. This process has already been done, so you do not need to do this unless the files in **data/** are changed or updated. 

The first step is to source the file **tsv2ctfsR.R**:

```{r}
# adjust for your computer directories
> source( '/Users/mender/CTFS/treelover/tsv2ctfs/tsv2ctfsR.R',  chdir=TRUE)
```

Now you have loaded the tsv2ctfs function , which you can see with the **ls** function. 

It has the following usage:

###tsv2ctfs
----------
#####Arguments
* **plotName**: prefix that will be applied to all of the ctfs R data files
* **master\_fileName**: tsv version of the master dataset (use excel to open the xlsx files and save as tsv, then put in the data/ subfolder)
* **taxonomy\_fileName**: tsv version of the taxoomy info (see existing file data/Laupahoehoe\_taxonomy.txt for template)
* **elevation\_fileName**: tsv version of the elevation data for the plot (should be on a regular 2d grid spanning the plot (see data/Laupahoehoe\_master.txt) 
* **forest**: must be either 'wet' or 'dry'. It is a selector for different tree height and biomass models

For now, any fileNames you pass to **tsv2ctfs** should be the basname only (e.g. _file.txt_ and not _/path/to/file.txt_),
and the files should be stored in the **/data** sub-folder. 

Assume I have corrected the master TSV file for Palamanui and saved it in the same location with the same name. I want to now re-compute th Palamanui dataframes, so I would run 

```{r}
> tsv2ctfs(plotName='Palamanui',master_fileName="Palamanui_master.txt",taxonomy_fileName='Palamanui_taxonomy.txt',forest='dry')
    Calculating above ground biomass (if you have a weak computer this might take some time) ...
    Making full,stem, and split dataframes for census number 1...
    Making full,stem, and split dataframes for census number 5...
    Making taxonomy dataframe...
    NOTE:
    If warnings are thrown because directories already exist, disregard.
Warning messages:
1: In dir.create(fullDir) :
  '/Users/mender/CTFS/treelover/tsv2ctfs/full' already exists
2: In dir.create(stemDir) :
  '/Users/mender/CTFS/treelover/tsv2ctfs/stem' already exists
3: In dir.create(speciesDir) :
  '/Users/mender/CTFS/treelover/tsv2ctfs/species' already exists
4: In dir.create(splitDir) :
  '/Users/mender/CTFS/treelover/tsv2ctfs/split' already exists
5: In dir.create(elevDir) :
  '/Users/mender/CTFS/treelover/tsv2ctfs/elev' already exists
```

If you want you want to instead make a separate dataframe,  simply change the **plotName** parameter. For example, 

```{r}
> tsv2ctfs(plotName='PalaTest',master_fileName="Palamanui_master.txt",taxonomy_fileName='Palamanui_taxonomy.txt',forest='dry')
    Calculating above ground biomass (if you have a weak computer this might take some time) ...
    Making full,stem, and split dataframes for census number 1...
    Making full,stem, and split dataframes for census number 5...
    Making taxonomy dataframe...
    NOTE:
    If warnings are thrown because directories already exist, disregard.
Warning messages:
1: In dir.create(fullDir) :
  '/Users/mender/CTFS/treelover/tsv2ctfs/full' already exists
2: In dir.create(stemDir) :
  '/Users/mender/CTFS/treelover/tsv2ctfs/stem' already exists
3: In dir.create(speciesDir) :
  '/Users/mender/CTFS/treelover/tsv2ctfs/species' already exists
4: In dir.create(splitDir) :
  '/Users/mender/CTFS/treelover/tsv2ctfs/split' already exists
5: In dir.create(elevDir) :
  '/Users/mender/CTFS/treelover/tsv2ctfs/elev' already exists
> list.files('/Users/mender/CTFS/treelover/tsv2ctfs/full/')
[1] "Laupahoehoe.full1.rdata" "Laupahoehoe.full5.rdata" "Palamanui.full1.rdata"  
[4] "Palamanui.full5.rdata"   "PalaTest.full1.rdata"    "PalaTest.full5.rdata" 
```

As you can see, additional files **PalaTest.full1.rdata** and **PalaTest.full5.rdata** were saved. Load them using the **load** function, e.g.

```r
> load('/Users/mender/CTFS/treelover/tsv2ctfs/full/PalaTest.full1.rdata')
> str(PalaTest.full1)
'data.frame':	14641 obs. of  21 variables:
 $ treeID   : int  0 1 2 3 4 5 6 7 8 9 ...
 $ stemID   : chr  NA NA NA NA ...
 $ tag      : chr  "5675" "5676" "5677" "5678" ...
 $ StemTag  : chr  NA NA NA NA ...
 $ sp       : chr  "PSYODO" "PSYODO" "PSYODO" "PSYODO" ...
 $ quadrat  : chr  "0305" "0305" "0305" "0305" ...
 $ gx       : num  60.8 60.6 61.4 60.8 61.2 ...
 $ gy       : num  101 101 102 103 104 ...
 $ MeasureID: int  1 9 11 15 17 19 23 27 29 31 ...
 $ CensusID : int  1 1 1 1 1 1 1 1 1 1 ...
 $ dbh      : num  1.67 1.82 2.12 1.65 19.45 ...
 $ pom      : chr  "130" "130" "130" "130" ...
 $ hom      : num  130 130 130 130 130 130 130 130 130 130 ...
 $ ExactDate: chr  "2008-08-26 00:00:00" "2008-08-26 00:00:00" "2008-08-26 00:00:00" "2008-08-26 00:00:00" ...
 $ DFstatus : chr  "alive" "alive" "alive" "alive" ...
 $ codes    : chr  NA NA NA NA ...
 $ nostems  : num  4 1 2 1 1 2 2 1 1 6 ...
 $ status   : chr  "A" "A" "A" "A" ...
 $ date     : num  2454704 2454704 2454704 2454704 2454704 ...
 $ agb      : num  0.00316 0.00344 0.00401 0.00312 0.02593 ...
 $ RawStatus: chr  "alive" "alive" "alive" "alive" ...
```
Now these dataframes can be used with the CTFS R Package.

## Using the **prepGrowth** function

It seems the CTFS website tutorials use _mm_ as the units for DBH. You can switch from our HIPPNET _cm_ units easily:

```{r}
> Laupahoehoe.full1$dbh = 100* Laupahoehoe.full1$dbh
> Laupahoehoe.full5$dbh = 100* Laupahoehoe.full5$dbh
```

There is an auxiliary function called **prepGrowth** 
that was loaded when **starter.R** was sourced. 
The arguments to the function are defined as 

### prepGrowth
------
##### ARGUMENTS 
* **censusA,censusB** ( CTFS tree/full-style dataframes made with tsv2ctfs the function)
* IMPORTANT: **censusA** should pre-date **censusB**, i.e. **censusB** is a recensus of **censusA**
* **min\_delta** ( minimum dbh difference between two growth years for a given tree, in same units as dbh )
* **species\_filter**  ( a vector or species labels (e.g. CIBMEN) )
* **bad\_trees** ( a integrer vector of bad tree ID numbers )
* **thresh** ( a threshold for outlier removal; lower thresh means more outliers removed  ) 

##### RETURNS
* a list of the growth-filtered censusA and censusB 
* reference each with "$censusA" and "$censusB"

> The **thresh** (threshold) parameter is used to remove growth outliers on a per-species basis. 
> Setting thresh=3 means all trees of species X whose dbh growth measure is more than 3 standard deviations away from the mean are thrown out. 
> (only here we consider standrad deviation of the median, as opposed to the mean) Here is the reference to the [code](http://stackoverflow.com/questions/22354094/pythonic-way-of-detecting-outliers-in-one-dimensional-observation-data). See also: _Boris Iglewicz and David Hoaglin (1993), "Volume 16: How to Detect and Handle Outliers", The ASQC Basic References in Quality Control: Statistical Techniques, Edward F. Mykytka, Ph.D., Editor._

Certain tree species will not be used to calculate growth. At this time we will exclude the following

* CIBGLA
* CIBMEN
* CIBCHA

from Laupahoehoe. In addition, trees with new main stems cannot be used to calculate growth: 

```{r}
> growthDataLau = prepGrowth(censusA=Laupahoehoe.full1, censusB=Laupahoehoe.full5, species_filter=c("CIBMEN","CIBCHA","CIBGLA"), thresh=3, min_delta=-4, bad_trees=c() )

    Beginning species-specific outlier detection...
    =============================================== 
    METPOL: removed 154 outliers (6.69 %)
    CHETRI: removed 98 outliers (3.97 %)
    BROARG: removed 15 outliers (8.24 %)
    VACCAL: removed 8 outliers (4.65 %)
    COPRHY: removed 12 outliers (2.14 %)
    ILEANO: removed 34 outliers (4.64 %)
    PIPALB: removed 0 outliers (0.00 %)
    ACAKOA: removed 2 outliers (1.46 %)
    CLEPAR: removed 0 outliers (0.00 %)
    PERSAN: removed 0 outliers (0.00 %)
    MYRLES: removed 14 outliers (7.29 %)
    MELCLU: removed 0 outliers (0.00 %)
    HEDHIL: removed 1 outliers (4.00 %)
    LEPTAM: removed 0 outliers (0.00 %)
    MYRSAN: removed 0 outliers (0.00 %)
    PSYHAW: removed 0 outliers (0.00 %)

> Laupahoehoe.growth1 = growthDataLau$censusA
> Laupahoehoe.growth5 = growthDataLau$censusB
```

### NOTES
I would suggest using separate, self-implemented code to cross-validate any major results obtained with the CTFS R Package.
