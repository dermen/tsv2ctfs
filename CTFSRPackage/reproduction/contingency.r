
# <function> 
# <name> 
# contingencyanalysis
# </name> 
# <description> 
# Perform contingency analyses to evaluate the null hypothesis that trap records occur uniformly throughout the year for each species.
# The analysis is performed separately for fruit and for flowers.
# To identify multimodal phenologies, tally the number of switches of Tukey-Freeman deviates from significantly negative to significantly positive.
#   The function tukey.freeman.switches must also be sourced.
# The basic observation for the contingency analysis is a trap record.
#   Trap records are defined as the number of trap-census combinations for which the species was present as a fruit or flower.
#      Evidence of fruit includes mature fruit, seeds, capsules, fruits damaged by frugivores and seeds damaged by seed predators.
#      Evidence of flowers includes perfect/female and male flowers.
#   For species with more than 59 trap records, the analysis is performed for 12 months.
#   For species with 30 to 59 trap records, the analysis is performed for six two-month intervals. 
#     There are two possible six two-month intervals. The possibility with the largest chi-square value is selected.
#   For species with less than 30 trap records, no analysis is performed.
# The output file holds species, number of trap records, chi-square value, p-value, and number of switches of Tukey-Freeman deviates as described above.
# </description> 
# <arguments> 
# IF NECESSARY, LIST ARGUMENTS AND DEFINITIONS
# The function arguments are (1) an input file and (2) an output file.
# The function preprecords can be used to create the input file. An input file holds flower or fruit data (but not both). 
# The input file must include variables with the following names: sp, fecha and quantity. Variable names must be in lower case letters.
#   sp = the mnemonic or code used to identify species
#   fecha = the date of the observation. The format must by YYYYMMDD. For example, 11 May 2011 becomes 20110511.
#   quantity = the number of trap records for flowers (or for fruit) for that value of sp and fecha. 
#     Trap records are defined as the number of trap-fecha combinations for which the species was present.
# The output file holds species, number of trap records, chi-square value, p-value, and number of switches of Tukey-Freeman deviates as described above.
# </arguments> 
# <sample> 
# No example is given. 
# </sample> 
# <source> 
contingencyanalysis=function(infile,outfile) {
  # CONTINGENCY ANALYSIS FOR SEASONALITY (12 MOS, SP WITH >=60 HITS)
  # The next 2 lines tabulate by species and month and store the result in spmo.
	a=read.table(infile,header=T)
  a$month= round(a$fecha/100, 0)-100*round(a$fecha/10000, 0)
  spmo=as.data.frame(with(a,table(sp,month)), responseName = "records")
  # Calculate chi-square value and probability for each species
  nrecord=table(a$sp)
  ngood=nrecord[nrecord>=60]
  sp=names(ngood)
  nsp=length(sp)
  for (i in 1:nsp) {
     inc=spmo$sp==sp[i]
     temp=chisq.test(spmo$records[inc])
     results=cbind(temp$statistic, temp$p.value, tukey.freeman.switches(temp$observed,temp$expected))
     if (i==1) allresults=results else allresults=rbind(allresults,results)
  }
  t12mo=as.data.frame(sp)
  t12mo$records=ngood
  t12mo$chisq=allresults[,1]
  t12mo$pvalue=allresults[,2]
  t12mo$switches=allresults[,3]
  ## Contingency analysis for species with 30 to 59 records. Uses 6 paired months. Pairs start JF & DJ.
  # The next 5 lines define a$month, a$jf and a$dj then store in a1.
  a$month= round(a$fecha/100, 0)-100*round(a$fecha/10000, 0)
  jf = ifelse(as.numeric(a$month)%%2==1, a$month, as.numeric(a$month)-1)
  temp = ifelse(as.numeric(a$month)==1, 13,  as.numeric(a$month)) 
  dj = ifelse(temp%%2==0, temp, temp-1)
  a1=cbind(a,jf,dj)
  # The next line tabulates by species and jf and stores the result in spmo.
  spmo=as.data.frame(with(a1,table(sp,jf)), responseName = "records")
  # Calculate chi-square value and probability for each species
  nrecord=table(a$sp)
  ngood=nrecord[nrecord<60 & nrecord>=30]
  sp=names(ngood)
  nsp=length(sp)
  for (i in 1:nsp) {
     inc=spmo$sp==sp[i]
     temp=chisq.test(spmo$records[inc])
     results=cbind(temp$statistic, temp$p.value, tukey.freeman.switches(temp$observed,temp$expected))
     if (i==1) allresults=results else allresults=rbind(allresults,results)
  }
  jf=as.data.frame(sp)
  jf$records=ngood
  jf$chi.square=allresults[,1]
  jf$p.value=allresults[,2]
  jf$switches=allresults[,3]
  # The next line tabulates by species and dj and stores the result in spmo.
  spmo=as.data.frame(with(a1,table(sp,dj)), responseName = "records")
  # Calculate chi-square value and probability for each species
  nrecord=table(a$sp)
  ngood=nrecord[nrecord<60 & nrecord>=30]
  sp=names(ngood)
  nsp=length(sp)
  for (i in 1:nsp) {
     inc=spmo$sp==sp[i]
     temp=chisq.test(spmo$records[inc])
     results=cbind(temp$statistic, temp$p.value, tukey.freeman.switches(temp$observed,temp$expected))
     if (i==1) allresults=results else allresults=rbind(allresults,results)
  }
  dj=as.data.frame(sp)
  dj$records=ngood
  dj$chi.square=allresults[,1]
  dj$p.value=allresults[,2]
  dj$switches=allresults[,3]
  # Merge results for paired months beginning jf and dj and choose the largest chi-square value.
  t6mo=as.data.frame(sp)
  t6mo$records=ngood
  t6mo$chisq=ifelse(jf$chi.square>dj$chi.square, jf$chi.square, dj$chi.square)
  t6mo$pvalue = ifelse(jf$chi.square>dj$chi.square, jf$p.value, dj$p.value)
  t6mo$switches = ifelse(jf$chi.square>dj$chi.square, jf$switches, dj$switches)
  #Append contingency analyses for 12 mos and for six pairs of two months.
  contingency = rbind(t12mo,t6mo)
  write.table(contingency,file=outfile,row.names=F,sep="\t")
} # end contingencyanalysis
# </source> 
# </function> 

