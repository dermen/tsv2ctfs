# <function>
# <name>
# individual_grow.table
# </name>
# <description>
# Create a table of individual trees and their growth over two censuses, with many species included. 
# The option rnd is used to rounddown dbhs for certain intervals. The flag ctr is used to center the time variable,
# which is the number of years since 1960 of the interval midpoint. There is a logarithmic transformation, for which all growth<=0
# is converted to mingrow. There is also a power transformation, where each growth rate is raised to the power given by powertransformation.
# In the latter, negative growths are transformed to negative, so do not need to be corrected. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
individual_grow.table=function(cnsdata=list(bci.full1,bci.full2,bci.full3,bci.full4,bci.full5,bci.full6,bci.full7),powertransformation=0.45,
                               rnd=c(FALSE,TRUE,rep(FALSE,4)),mingrow=0.1,mindbh=10,maxdbh=10000,maxerrorSD=4,maxerrorGrow=75,center=1992,debug=FALSE)
{ 
 for(i in 1:(length(cnsdata)-1))
    {
        cns=i:(i+1)
        gtbl=growth.indiv(cnsdata[[i]],cnsdata[[i+1]],rounddown=rnd[i],err.limit=maxerrorSD,maxgrow=maxerrorGrow)
        gtbl$time=(cnsdata[[i+1]]$date+cnsdata[[i]]$date)/(2*365.25)
        gtbl$census=i
        gtbl$censusfact=as.factor(i)
        # browser()
        
        section=subset(gtbl,!is.na(incgr) & dbh1<maxdbh & dbh1>=mindbh)
        
        if(i==1) final=section
        else final=rbind(final,section)
    }
 
 final$growth=final$incgr
 final$growth[final$incgr<=0]=mingrow
 final$LnGrowth=log(final$growth)
 final$LnSize=log(final$dbh1)-mean(log(final$dbh1))
 final$CRGrowth=pospower(final$incgr,powertransformation)
 if(debug) browser()
 
 colnames(final)[which(colnames(final)=='sp')]='species'
 
 final=subset(final,status2!='M',
              select=c('treeID','tag','gx','gy','species','dbh1','dbh2','LnSize','incgr','LnGrowth','CRGrowth','time','census','censusfact'))
 if(!is.null(center)) final$time=final$time+1960-center
     
 return(final)
}
# </source>
# </function>
# 
# 

# <function>
# <name>
# individual_mort.table
# </name>
# <description>
# Create a table of individual trees and their survival status over two censuses, with many species included. 
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
individual_mort.table=function(cnsdata=list(bci.full1,bci.full2,bci.full3,bci.full4,bci.full5,bci.full6,bci.full7),
                               mindbh=10,maxdbh=10000,alivecode=c("A","AB","AS"),center=1992)
{ 
 for(i in 1:(length(cnsdata)-1))
    {
        cns=i:(i+1)
        
        section=subset(cnsdata[[i]],select=c('date','treeID','tag','sp','gx','gy','dbh','status'))
        section$status2=cnsdata[[i+1]]$status
        section$date2=cnsdata[[i+1]]$date
        section$census=i
        
        section=subset(section,status=='A' & dbh>=mindbh & dbh<maxdbh)
        
        if(i==1) final=section
        else final=rbind(final,section)
    }
    
 final$interval=(final$date2-final$date)/365.25
 final$time=(final$date2+final$date)/(2*365.25)+1960-center
 
 final=subset(final,status2!='M',select=c('treeID','tag','sp','gx','gy','dbh','interval','status2','time','census'))
 final$censusfact=as.factor(final$census)
 
 A=rep(NA,dim(final)[1])
 for(i in 1:length(alivecode)) A[final$status2==alivecode[i]]=TRUE
 A[final$status2=='D']=FALSE
 final$fate=A
 final$status2=NULL
 
 return(final)
}
# </source>
# </function>
# 
# 

# 
# <function>
# <name>
# calcMortIndivTable
# </name>
# <description>
# Calculate mortality rate per species per census interval using the output of individual_mort.table. 
# Formerly named calcMortlmerTable.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# </sample>
# <source>
calcMortIndivTable=function(mtable,by='species')
{
 if(by=='species') splitby=list(mtable$sp,mtable$census)
 else splitby=mtable$census
 
 S=tapply(mtable$fate,splitby,sum)
 S[is.na(S)]=0
 N=tapply(mtable$fate,splitby,length)
 N[is.na(N)]=0
 time=tapply(mtable$interval,splitby,mean,na.rm=TRUE)
 
 mortality=mortality.calculation(N,S,time)$rate
 return(mortality)
}
# </source>
# </function>
 
