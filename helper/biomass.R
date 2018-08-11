# AUTHOR: DEREK MENDEZ (dermendarko@gmail.com) #


##############################
# ABOVE-GROUND BIOMASS STUFF #
##############################

.biomass.heightModel_Palau <- function( diam)
{
######################################### #
#
# This is a generic model for Palau trees #
#
###########################################
    return( 1.0409 * exp( 0.5120 + 0.783 * log(diam) - 0.0322 * log(diam)*log(diam)  ))
}

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
                'agb_model'= .biomass.agbModel_global),
               
	'ADENPA'=list( 'density' = 0.6870,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'AGLAMA'=list( 'density' = 0.6590,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'ALPHCA'=list( 'density' = 0.5290,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'ALPICA'=list( 'density' = 0.1000,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'ANACGL'=list( 'density' = 0.7260,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'ATUNRA'=list( 'density' = 0.6650,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'BARRRA'=list( 'density' = 0.4540,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'BUCHEN'=list( 'density' = 0.4110,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'CALLEL'=list( 'density' = 0.3500,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'CALOIN'=list( 'density' = 0.5850,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'CALOPE'=list( 'density' = 0.5760,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'CAMPBR'=list( 'density' = 0.3040,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'CANAHI'=list( 'density' = 0.4900,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'CASEHI'=list( 'density' = 0.6270,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'CELTPA'=list( 'density' = 0.5540,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'CERBFL'=list( 'density' = 0.4340,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'COLOSC'=list( 'density' = 0.3400,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'COMMBA'=list( 'density' = 0.4000,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'DECAPA'=list( 'density' = 0.7220,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'DIOSFE'=list( 'density' = 0.9400,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'DRACMU'=list( 'density' = 0.4180,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'DRYPNI'=list( 'density' = 0.7070,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'ELAEJO'=list( 'density' = 0.4890,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'EUGERE'=list( 'density' = 0.7280,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'EURYJA'=list( 'density' = 0.6200,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'FAGRBE'=list( 'density' = 0.8270,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'FAGRKS'=list( 'density' = 0.7390,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'FICUMI'=list( 'density' = 0.4120,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'FICUPR'=list( 'density' = 0.4120,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'FICUTI'=list( 'density' = 0.4120,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'FINSCH'=list( 'density' = 0.6330,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'FLACRU'=list( 'density' = 0.7500,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'GARCMA'=list( 'density' = 0.7350,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'GIROCE'=list( 'density' = 0.4610,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'GMELPA'=list( 'density' = 0.4900,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'GONICA'=list( 'density' = 0.4400,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'HEDYKO'=list( 'density' = 0.6360,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'HORSIR'=list( 'density' = 0.3690,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'HORSPA'=list( 'density' = 0.4430,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'IXORCA'=list( 'density' = 0.7930,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'MACACA'=list( 'density' = 0.3810,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'MARACO'=list( 'density' = 0.7980,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'MELAMA'=list( 'density' = 0.4400,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'MELIDE'=list( 'density' = 0.4820,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'MORIPE'=list( 'density' = 0.5580,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'MYRSPA'=list( 'density' = 0.7420,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'ORMOCA'=list( 'density' = 0.4300,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'ORMOCO'=list( 'density' = 0.7420,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'OSMONG'=list( 'density' = 0.3700,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'OSMOOL'=list( 'density' = 0.3700,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'OSMOPA'=list( 'density' = 0.3700,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'PANDAI'=list( 'density' = 0.3310,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'PHALNI'=list( 'density' = 0.5140,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'PHYLMA'=list( 'density' = 0.6180,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'PHYLOT'=list( 'density' = 0.6180,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'PHYLPA'=list( 'density' = 0.6180,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'PINAIN'=list( 'density' = 0.5570,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list(),
			'agb_model'= .biomass.agbModel_ferns),

	'PLANOB'=list( 'density' = 0.7020,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'PREMSE'=list( 'density' = 0.5500,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'PSYCLE'=list( 'density' = 0.5640,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'RHUSTA'=list( 'density' = 0.3700,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'SALACH'=list( 'density' = 0.7600,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'SEMEVE'=list( 'density' = 0.3570,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'SPHANI'=list( 'density' = 0.2060,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'SYMPRA'=list( 'density' = 0.5310,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'SYZYCU'=list( 'density' = 0.6730,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'TIMOMO'=list( 'density' = 0.5940,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'TIMOSU'=list( 'density' = 0.5940,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'TRICLE'=list( 'density' = 0.3130,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet),

	'WIKSEL'=list( 'density' = 0.5140,
			'ht_coef'  = list(),
			'ht_model' = .biomass.heightModel_Palau,
			'agb_coef' = list('a'=0, 'b'=0,'c'=0,'d'=0 ,'e'=0,'f'=0,'dbh_cutoff'=-1),
			'agb_model'= .biomass.agbModel_S1_wet))


###############
###############
###############
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

 


