# AUTHOR: DEREK MENDEZ (dermendarko@gmail.com) #


##############################
# ABOVE-GROUND BIOMASS STUFF #
##############################

.biomass.heightModel <- function( a,b,c,d,e,f,g,diam)
{
##########################################################
#                                                        #
#  - a,b,c,d,e,f,g are all regression coefficients       #
#  - diam is "diameter at breate height" in centimeters, #
#  - returns tree height in meters                       #
#                                                        #
##########################################################
    return ( a + b*log(diam) + c*exp( d + e*log(diam) + f*diam + g*log(diam)*log(diam)   ) )
}

.biomass.agbModel_global <- function( a,b,c,d,e,f, diam, ht, rho )
{
################################################################
#                                                              #
# - a,b,c,d,e,f are regression coefficients for this model     #
# - diam is "diameter at breast height" in centimeters (cm),   #
# - ht is tree height in meters,                               #
# - rho is species wood density in gram / cm^3                 #
# - returns above ground biomass measured in  kilograms (kg)   #
#                                                              #   
################################################################
    return ( a + b*diam + c*exp( d + e *log(rho*ht*diam*diam) + f*log(diam)  ) )
}

.biomass.agbModel_S1_wet <- function(a,b,c,d,e,f,dbh_cutoff,diam,ht,rho)
# SPECIES SPECIFIC AGB MODEL FOR TREES IN WET FORESTS
{
    {
    if (diam <= dbh_cutoff) 
        return( .biomass.agbModel_global( a,b,c,d,e,f,diam,ht,rho ) )
    else 
        return( .biomass.agbModel_global(0,0,1,-2.557,.94,0,diam,ht,rho)  )
    }
}

.biomass.agbModel_S1_dry <- function(a,b,c,d,e,f,dbh_cutoff,diam,ht,rho)
# SPECIES SPECIFIC AGB MODEL FOR TREES IN DRY FORESTS
{
    {
    if (diam <= dbh_cutoff) 
        return( .biomass.agbModel_global( a,b,c,d,e,f,diam,ht,rho ) )
    else 
        return( .biomass.agbModel_global(0,0,1,-2.187,.916,0,diam,ht,rho)  )
    }
}

.biomass.agbModel_ferns <- function( diam, ht, rho  )
# SIMPLE AGB MODEL FOR TREE FERNS
{
    area   = pi * diam*diam / 4 # cross sectional area in cm^2
    volume = area * ht * 100    # volume in cm^3
    mass   = rho * volume       # weight in grams
    return (mass / 1000)        # returns weight in kg
}

# density below is in (grams / cm^3)
.biomass.Lau_AGB = list(
 'ACAKOA'=list( 'density' =0.550,
                'ht_coef' = list('a'=0,'b'=0,'c'=1.0156,'d'=0.1795,'e'=1.1016,'f'=0,'g'=-0.08),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1.0171,'d'=-2.3270 ,'e'=0,'f'=2.35,'dbh_cutoff'=30), 
                'agb_model'=  .biomass.agbModel_S1_wet),
 'ANTPLA'=list( 'density' = 0.670,	
                'ht_coef'  = list('a'=9.6743,'b'=0,'c'=-8.5503,'d'=0,'e'=0,'f'= -0.1079,'g'=0),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.557 ,'e'=.94,'f'=0), 
                'agb_model'= .biomass.agbModel_global),
 'BROARG'=list( 'density'=0.360,
                'ht_coef' = list('a'=0,'b'=0,'c'=1.0409,'d'=0.512,'e'=0.7583,'f'=0,'g'=-0.0322),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.557 ,'e'=.94,'f'=0 ), 
                'agb_model'= .biomass.agbModel_global),
 'CHETRI'=list( 'density'=0.470,
                'ht_coef' = list('a'=12.6477,'b'=0,'c'=-12.6477,'d'=0,'e'=0,'f'= -0.1365,'g'=0),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.557 ,'e'=.94,'f'=0), 
                'agb_model'= .biomass.agbModel_global),
 'CIBCHA'=list( 'density'=0.210,
                'ht_coef' = list('a'=0,'b'=0,'c'=0.012763,'d'=0.6457,'e'=1.5932,'f'=0,'g'=0),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list(),
                'agb_model'= .biomass.agbModel_ferns),
 'CIBGLA'=list( 'density'=0.220,
                'ht_coef' = list('a'=0,'b'=0,'c'=0.011386,'d'=-.6277,'e'=1.691,'f'=0,'g'=0),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list(),
                'agb_model'= .biomass.agbModel_ferns),
 'CIBMEN'=list( 'density'=0.190,
                'ht_coef' = list('a'=0,'b'=0,'c'=0.011705,'d'=-.6549,'e'=1.8683,'f'=0,'g'=0),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list(),
                'agb_model'= .biomass.agbModel_ferns),
 'CLEPAR'=list( 'density'=0.500,
                'ht_coef' = list('a'=0,'b'=0,'c'=1.0409,'d'=0.5120,'e'=.7583,'f'=0,'g'=-0.0322),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.557 ,'e'=.94,'f'=0),
                'agb_model'= .biomass.agbModel_global),
 'COPRHY'=list( 'density'=0.480,
                'ht_coef' = list('a'=10.2252,'b'=0,'c'=-10.2252,'d'=0,'e'=0,'f'= -0.2257,'g'=0),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.557 ,'e'=.94,'f'=0),
                'agb_model'= .biomass.agbModel_global),
 'HEDHIL'=list( 'density'=0.380,
                'ht_coef' = list('a'=0,'b'=0,'c'=1.0409,'d'=0.5120,'e'=.7583,'f'=0,'g'=-0.0322),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.557 ,'e'=.94,'f'=0),
                'agb_model'= .biomass.agbModel_global),
 'ILEANO'=list( 'density'=0.480,
                'ht_coef' = list('a'=13.0821,'b'=0,'c'=-13.0821,'d'=0,'e'=0,'f'= -0.1339,'g'=0),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.557 ,'e'=.94,'f'=0),
                'agb_model'= .biomass.agbModel_global),
 'LEPTAM'=list( 'density'=0.670,
                'ht_coef' = list('a'=0,'b'=0,'c'=1.0409,'d'=0.5120,'e'=.7583,'f'=0,'g'=-0.0322),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.557 ,'e'=.94,'f'=0),
                'agb_model'= .biomass.agbModel_global),
 'MELCLU'=list( 'density'=0.480,
                'ht_coef' = list('a'=0,'b'=0,'c'=1.0409,'d'=0.5120,'e'=.7583,'f'=0,'g'=-0.0322),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.557 ,'e'=.94,'f'=0), 
                'agb_model'= .biomass.agbModel_global),
 'METPOL'=list( 'density'=0.690,
                'ht_coef' = list('a'=22.9975,'b'=0,'c'=-22.9975,'d'=0,'e'=0,'f'=-.0452,'g'=0),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1.0671,'d'=-2.1311 ,'e'=0,'f'=2.5011,'dbh_cutoff'=33), 
                'agb_model'=  .biomass.agbModel_S1_wet),
 'MYRLES'=list( 'density'=0.500,
                'ht_coef' = list('a'=0,'b'=0,'c'=1.0409,'d'=0.5120,'e'=.7583,'f'=0,'g'=-0.0322),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.557 ,'e'=.94,'f'=0),
                'agb_model'= .biomass.agbModel_global),
 'MYRSAN'=list( 'density'=0.610,
                'ht_coef' = list('a'=10.2293,'b'=0,'c'=-8.1291,'d'=0,'e'=0,'f'= -0.0489,'g'=0),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.557 ,'e'=.94,'f'=0),
                'agb_model'= .biomass.agbModel_global),
 'PERSAN'=list( 'density'=0.410,
                'ht_coef' = list('a'=13.2279,'b'=0,'c'=-11.8919,'d'=0,'e'=0,'f'= -0.0772,'g'=0),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.557 ,'e'=.94,'f'=0),
                'agb_model'= .biomass.agbModel_global),
 'PIPALB'=list( 'density'=0.300,
                'ht_coef' = list('a'=0,'b'=0,'c'=1.0409,'d'=0.5120,'e'=.7583,'f'=0,'g'=-0.0322),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.557 ,'e'=.94,'f'=0), 
                'agb_model'= .biomass.agbModel_global),
 'PSYHAW'=list( 'density'=0.540,
                'ht_coef' = list('a'=0,'b'=0,'c'=1.0409,'d'=0.5120,'e'=.7583,'f'=0,'g'=-0.0322),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.557 ,'e'=.94,'f'=0), 
                'agb_model'= .biomass.agbModel_global),
 'TREGRA'=list( 'density'=0.500,
                'ht_coef' = list('a'=0,'b'=0,'c'=1.0409,'d'=0.5120,'e'=.7583,'f'=0,'g'=-0.0322),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.557 ,'e'=.94,'f'=0),
                'agb_model'= .biomass.agbModel_global),
 'VACCAL'=list( 'density'=0.500,
                'ht_coef' = list('a'=0,'b'=0,'c'=1.0409,'d'=0.5120,'e'=.7583,'f'=0,'g'=-0.0322),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.557 ,'e'=.94,'f'=0), 
                'agb_model'= .biomass.agbModel_global),
 'COPMON'=list( 'density'=0.480,
                'ht_coef' = list('a'=10.2252,'b'=0,'c'=-10.2252,'d'=0,'e'=0,'f'= -0.2257,'g'=0),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.557 ,'e'=.94,'f'=0),
                'agb_model'= .biomass.agbModel_global),
 'MYOSAN'=list( 'density' = 0.881,	
                'ht_coef'  = list('a'=0,'b'=0,'c'=1.0514,'d'=.348,'e'=.6056,'f'=0,'g'=-0.0246),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.557 ,'e'=.94,'f'=0), 
                'agb_model'= .biomass.agbModel_global))

.biomass.Pn_AGB = list(
 'CHAMUL'=list( 'density' = 0.75,	
                'ht_coef'  = list('a'=0,'b'=0,'c'=1.0514,'d'=.348,'e'=.6056,'f'=0,'g'=-0.0246),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.187 ,'e'=.916,'f'=0), 
                'agb_model'= .biomass.agbModel_global),
 'DIOSAN'=list( 'density' =0.740,
                'ht_coef' = list('a'=6.0846,'b'=0,'c'=-6.0846,'d'=0,'e'=0,'f'=-0.101,'g'=0),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=1.333,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=.Machine$integer.max), 
                'agb_model'=  .biomass.agbModel_S1_dry),
 'DODVIS'=list( 'density' = 0.947,	
                'ht_coef'  = list('a'=0,'b'=0,'c'=1.0514,'d'=.348,'e'=.6056,'f'=0,'g'=-0.0246),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.187 ,'e'=.916,'f'=0), 
                'agb_model'= .biomass.agbModel_global),
 'ERYSAN'=list( 'density' = 0.29,	
                'ht_coef'  = list('a'=0,'b'=0,'c'=1.0514,'d'=.348,'e'=.6056,'f'=0,'g'=-0.0246),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.187 ,'e'=.916,'f'=0), 
                'agb_model'= .biomass.agbModel_global),
 'EUPMUL'=list( 'density' = 0.47,	
                'ht_coef'  = list('a'=0,'b'=0,'c'=1.0514,'d'=.348,'e'=.6056,'f'=0,'g'=-0.0246),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.187 ,'e'=.916,'f'=0), 
                'agb_model'= .biomass.agbModel_global),
 'METPOL'=list( 'density' = 0.69,	
                'ht_coef'  = list('a'=14.134,'b'=0,'c'=-14.134,'d'=0,'e'=0,'f'=-.0573,'g'=0),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1.0671,'d'=-2.1311 ,'e'=0,'f'=2.5011,'dbh_cutoff'=33), 
                'agb_model'= .biomass.agbModel_S1_dry),
 'MYOSAN'=list( 'density' = 0.881,	
                'ht_coef'  = list('a'=0,'b'=0,'c'=1.0514,'d'=.348,'e'=.6056,'f'=0,'g'=-0.0246),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.187 ,'e'=.916,'f'=0), 
                'agb_model'= .biomass.agbModel_global),
 'OSTANT'=list( 'density' = 0.7,	
                'ht_coef'  = list('a'=0,'b'=0,'c'=1.0514,'d'=.348,'e'=.6056,'f'=0,'g'=-0.0246),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.187 ,'e'=.916,'f'=0), 
                'agb_model'= .biomass.agbModel_global),
 'PITTER'=list( 'density' = 0.67,	
                'ht_coef'  = list('a'=0,'b'=0,'c'=1.0514,'d'=.348,'e'=.6056,'f'=0,'g'=-0.0246),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.187 ,'e'=.916,'f'=0), 
                'agb_model'= .biomass.agbModel_global),
 'PLEHAW'=list( 'density' = 0.418,	
                'ht_coef'  = list('a'=0,'b'=0,'c'=1.0514,'d'=.348,'e'=.6056,'f'=0,'g'=-0.0246),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.187 ,'e'=.916,'f'=0), 
                'agb_model'= .biomass.agbModel_global),
 'PSYODO'=list( 'density' = 0.87,	
                'ht_coef'  = list('a'=6.5442,'b'=0,'c'=-6.5442,'d'=0,'e'=0,'f'=-.1832,'g'=0),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=1.89,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=.Machine$integer.max), # no dbh cutoff for PSYODO
                'agb_model'= .biomass.agbModel_S1_dry),
 'SANPAN'=list( 'density' = 0.76,	
                'ht_coef'  = list('a'=0,'b'=0,'c'=1.0265,'d'=.4386,'e'=.3883,'f'=0,'g'=-0.0246),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.187 ,'e'=.916,'f'=0), 
                'agb_model'= .biomass.agbModel_global),
 'SENGAU'=list( 'density' = 0.6,	
                'ht_coef'  = list('a'=0,'b'=0,'c'=1.0514,'d'=.348,'e'=.6056,'f'=0,'g'=-0.0246),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.187 ,'e'=.916,'f'=0), 
                'agb_model'= .biomass.agbModel_global),
 'SIDFAL'=list( 'density' = 0.452,	
                'ht_coef'  = list('a'=0,'b'=0,'c'=1.0514,'d'=.348,'e'=.6056,'f'=0,'g'=-0.0246),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.187 ,'e'=.916,'f'=0), 
                'agb_model'= .biomass.agbModel_global),
 'SOPCHR'=list( 'density' = 0.64,	
                'ht_coef'  = list('a'=5.3775,'b'=0,'c'=-5.3775,'d'=0,'e'=0,'f'=-.2262,'g'=0),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.187 ,'e'=.916,'f'=0), 
                'agb_model'= .biomass.agbModel_global),
 'MYRLAN'=list( 'density' = 0.53,
                'ht_coef'  = list('a'=15.9421,'b'=0,'c'=-14.0107,'d'=0,'e'=0,'f'=-0.0238,'g'=0),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.187 ,'e'=.916,'f'=0), 
                'agb_model'= .biomass.agbModel_global),
 'WIKSAN'=list( 'density' = 0.425,	
                'ht_coef'  = list('a'=0,'b'=0,'c'=1.0514,'d'=.348,'e'=.6056,'f'=0,'g'=-0.0246),	
                'ht_model' = .biomass.heightModel,
                'agb_coef' = list('a'=0, 'b'=0,'c'=1,'d'=-2.187 ,'e'=.916,'f'=0), 
                'agb_model'= .biomass.agbModel_global))




