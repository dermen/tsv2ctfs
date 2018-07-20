# <function>
# <name>
# lmerBayes
# </name>
# <description>
# A Metropolis MCMC version for any modeling y~x, without random effects (as in lmerBayes). This version is built off lmerBayes, with the hyperdistributions
# excluded. A single independent variable, y, can be fit against any number of predictors, x, 
# The model error can be binomial, Poisson, or Gaussian, # but there are two alternatives for the Gaussian (described below). 
# <br><br>
# Data are submitted the way lm or lmer require, with one single table, one row per observation. The formula, however, is not submitted using the R-style 'squiggle' ~. 
# Rather, the names of x, y columns, are given. The model describing y's function of the x's is passed, and must be provided by the user 
# (several are available within the CTFS R Package, though, in the Utilities topic). Examples below will serve to explain.
# <br><br>
# <br><br>
# A starting set of parameters for the model must be submitted. It is a vector as long as the number of
# parameters required by the model. 
# <br><br>
# <br><br>
# The return value is a list with several components:
# <ul>
# <li> mu: A 2D array with the entire chain of model parameters (ie, fixed effects) from the Gibbs sampler
# <li> sigma: A 3D array with the entire chain of covariances from the Gibbs sampler; if includeCovar==FALSE, only the diagonal is non-zero
# <li> bestmu: Best estimate of the model parameters for the entire data (ie, fixed effect) 
# <li> bestsigma: Best estimate of the covariance (ie, group-level variance or error)
# <li> resid: The entire chain parameters for the model of residuals
# <li> bestresid: The best estimate of parameters for the model of residuals
# <li> CIresid: Credible intervals for the parameters for the model of residuals
# <li> best: The best estimates of model parameters for each random effect
# <li> lower: Lower credible intervals of model parameters for each random effect
# <li> upper: Uower credible intervals of model parameters for each random effect      
# <li> burn: The burn-in
# <li> llike: Full log-likelihood of the model at each step of the Gibbs' sampler
# <li> bestlike: The log-likelihood of the optimal parameter combination (means of the posterior distribution)
# <li> DIC: Deviance information criterion of the model
# <li> obs: The original y (dependent) variable, just as submitted
# <li> data: The original x (independent) variables, just as submitted
# <li> model: The model's predictions, as a list with one element per random effect
# <li> keep: The steps of the Gibbs sampler after burn-in, as a vector of negative numbers 
# <li> start: The start parameters submitted
# <li> parnames: The names of the model parameters
# <\ul>
# <br><br>
# </description>
# <arguments>
# <ul>
# <li> data: The table of data, in lmer-style, including one column to be modeled (dependent variable, y), one or more predictors (independent variables, x), and one random effect, using any column names.
# <li> ycol: The name of the column holding the y variable, with quote marks; this variable must be numeric.
# <li> ycol: The name of one or more columns holding the x variables, with quote marks; these can be numeric or character variables.
# <li> start: Starting parameter values, either a vector with as many parameters as the model needs, or a matrix of such vectors, one per random effect
# <li> startSD: A single starting value for the residual standard deviation, only used with Gaussian error models.
# <li> model: The function name holding the model describing y's relationship to all the x's, without quote marks. The first argument of the function must be named x, the second param, with additional arguments allowed. The model may accept as x either a vector or a matrix, the latter for a multiple regression. There can be any number of parameters, but the number must match the number given as start parameters. The return value must be a numeric vector with the same size as x. 
# <li> error: A character variable with 3 possible values, Binom, Gauss, or GaussMultResid, with quote marks. 
# <ul>
# <li> 'Binom' uses binomial error for residuals
# <li> 'Poisson' uses Poisson error for residuals
# <li> 'Gauss' uses Gaussian error for residuals with constant standard deviation across groups
# <li> 'GaussMultResid' uses Gaussian error for residuals, with standard deviation a constant fraction of the model's prediction
#         (and thus only appropriate if predictions are strictly positive)
# <li> 'Flat' is a trivial model where the same likelihood is returned regardless of parameters or data. It is for testing how parameter search behaves in absence of data, as for describing an implied prior. 
# </ul>
# <li> update: 'conjugate' or 'metropolis', whether to use inverse-gamma (or inverse-Wishart for full covariance) vs. metropolis steps for updating covariances.
# <li> badparam: The name of a function (unquoted) that tests a set of model parameters for validity; must return TRUE if parameters are valid, otherwise FALSE.
# <li> sdfunc: The name of a function (unquoted) that models the residual standard deviation as a function of the x's, just like the model function. The default uses the function named constant, meaning the standard deviation is the same for all values of x. Parameters for this function are estimated, just as parameters for the model function are .
# <li> badSDparam: The name of a function which tests for invalid parameters for sdfunc, returning TRUE or FALSE (analogous to badparam); a simple version is provided, called badSD, which rejects a single parameter if it is < 0. 
# <li> steps: The number of steps to run the Gibbs sampler.
# <li> showstep: Information is printed to the screen every showstep steps.
# <li> burnin: The number of steps to remove as burn-in before calculating posterior distributions; not that all parameters are saved and returned regardless.
# <li> debug: TRUE or FALSE, whether to pause and debug; for advanced users and developers.
# <li> ...: The typical R means for submitting additional parameters for various functions used in the model (model, sdfunc, badparam, badSDparam).
# </ul>
# </arguments>
# <sample>
# </sample>
# <source>
modelBayes=function(data,ycol,xcol,start,startSD,model=logistic.standard,error='Binom',update='conjugate',badparam=NULL,
                    sdfunc=constant,badSDparam,steps=1000,showstep=100,burnin=100,debug=FALSE,...)
{
 data=subset(data,!is.na(data[,ycol]))
 for(onex in xcol) data=subset(data,!is.na(data[,onex]))
 
 y=data[,ycol]
 x=data[,xcol]
 
 if(!is.null(names(start))) parnames=names(start)
 else parnames=c('Inter',xcol)
 noparam=length(start)
   
 param=array(dim=c(steps,noparam),dimnames=list(NULL,parnames))
 param[1,]=start
 scale=start
 scale[scale<=0]=1
 
 # Residual standard deviation is required if error=Gauss or GaussMultResid. It is calculated from sdfunc and sdpar. If error==Binom, resid is ignored.
 if(error=='Gauss' | error=='GaussMultResid')
    {
	 noSDparam=length(startSD)
	 resid=matrix(ncol=noSDparam,nrow=steps)
	 resid[1,]=startSD
	 scale.resid=startSD
	 scale.resid[scale.resid<=0]=1
	}
 else if(error=='Binom' | error=='Pois' | error=='Flat') resid=NULL 
  
 if(debug) browser()
 
 llike=numeric()
 i=1
 # Use the function within lmer Bayes that handles a single random effect, with mu=NULL since no hyperdistribution involved
 llike[i]=llike.model.lmer(test=param[i,1],allparam=param[i,],whichtest=1,data=x,trueN=y,model=model,sdmodel=sdfunc,sdpar=resid[i,],
                           errormodel=error,badparam=badparam,mu=NULL,covar=NULL,...)
 if(debug) browser()
 
 for(i in 2:steps)
  {
   ##### Update the parameters for every one of the random effects ####
    for(j in 1:noparam)
        {
         testparam=arrangeParam.Gibbs(i,j,param)

         metropResult=metrop1step(func=llike.model.lmer,start.param=testparam[j],scale.param=scale[j],allparam=testparam,whichtest=j,
                                  data=x,trueN=drp(y),sdmodel=sdfunc,sdpar=resid[i-1,],model=model,errormodel=error,mu=NULL,
                                  badparam=badparam,adjust=1.02,target=0.25,...)

         param[i,j]=metropResult[1]
         scale[j]=metropResult[2]
        }

   if(debug) browser()

   #### Update the residual standard deviation (not used for Binom error). Since the residual SD is calculated from sdfunc, each of the sdparams must be updated. 
   if(error=='Gauss' | error=='GaussMultResid')
      {
       for(j in 1:noSDparam)
        {
         testparam=arrangeParam.Gibbs(i,j,resid)
          
         metropResult=metrop1step(func=residual.llike.modelBayes,start.param=testparam[j],scale.param=scale.resid[j],sdpar=testparam,whichtest=j,fullpar=param[i,],
                                  data=x,trueN=y,model=model,errormodel=error,sdmodel=sdfunc,badparam=badSDparam,adjust=1.02,target=0.25,...)
         resid[i,j]=metropResult[1]
         scale.resid[j]=metropResult[2]      
        }
      }
     
   #### Calculate full likelihood for current set of parameters ####
   llike[i]=llike.model.lmer(test=param[i,1],allparam=param[i,],whichtest=1,data=x,trueN=y,model=model,sdmodel=sdfunc,sdpar=resid[i,],
                             errormodel=error,badparam=badparam,mu=NULL,covar=NULL,...)
   if(debug) browser()
   if(is.infinite(llike[i])) browser()

   #### Display progress to the screen every showstep steps ####
   if(i%%showstep==2)
      {
       cat("Full counter ", i, " at time ", date())
       cat("... ",round(param[i,],5))
       cat(" ...and likelihood...",round(llike[i],1),"\n")
      }


  }

 #### Summary calculations are moved to a separate function
 result=list(resid=resid,fullparam=param,burn=burnin,steps=steps,llike=llike,obs=y,data=x,parnames=parnames,start=start)
 return(summaryModelMCMC(fit=result,model=model,error=error,sdmodel=sdfunc,badparam=badparam,paramfile=NULL,...))
}
# </source>
# </function>
# 
# 
#
# <function>
# <name>
# residual.llike.lmerBayes
# </name>
# <description>
# Calculate likelihood of residual standard deviation, given observations plus the predicting model and data (to make predictions).
# This likelihood does not depend on the hyperparameters. It does require data and prediction.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# <source>
residual.llike.modelBayes=function(test,whichtest,data,trueN,model,sdpar,fullpar,sdmodel,badparam,errormodel='Gauss',debug=FALSE,...)
{
 param=arrangeParam.llike(test,sdpar,whichtest)
 if(!is.null(badparam)) if(badparam(x=data,param=param,...)) return(-Inf)

 norand=length(data)
 
 modeled=model(x=data,param=fullpar,...)
 if(errormodel=='Gauss') withinSD=sdmodel(x=data,param=param)
 else if(errormodel=='GaussMultResid') withinSD=sdmodel(x=modeled,param=param)
 if(length(which(withinSD<=0))>0) return(-Inf)
 # browser()
 
 all.llike=dnorm(x=drp(trueN),mean=modeled,sd=withinSD,log=TRUE)
 # all.llike=dnorm(x=drp(trueN),mean=modeled,sd=withinSD*modeled,log=TRUE)
 if(is.na(sum(all.llike))) browser()
 llike=sum(all.llike)
 
 return(sum(llike))
}
# </source>
# </function>
# 
#

#
# <function>
# <name>
# summaryMCMC
# </name>
# <description>
# Make summary calculations based on the full Gibbs sampler. The argument fit is an object holding all steps of the sampler, plus data, observations,
# and likelihood. Estimates of confidence limits of all parameters are returned. Full likelihood at the best parameters is calculated and likelihood 
# at each step in sampler are used to calculate DIC.
# </description>
# <arguments>
# 
# </arguments>
# <sample>
# 
# <source>
summaryModelMCMC=function(fit,model,error,sdmodel,badparam,paramfile=NULL,returnfull=TRUE,...)
{
 noparam=length(fit$parnames)
 # browser()
 
 keep=(1+fit$burn):fit$steps
 good=length(keep)
 paramarray=array(fit$fullparam[keep,],dim=c(good,noparam))
 
 fit$best=colMeans(paramarray)
 fit$CI=apply(paramarray,2,CI)
 
 if(!is.null(fit$resid))
  {
   useresid=fit$resid[keep,]
   if(is.null(dim(useresid)))
    {
     fit$bestresid=mean(useresid)
     fit$CIresid=CI(useresid)
    }
   else
    {
     fit$bestresid=colMeans(useresid)
     fit$CIresid=apply(useresid,2,CI)
    } 
  }
 else bestresid=CIresid=NULL

 # Calculate predictions of observations, and likelihood of data given those predictions
 predbest=model(x=fit$data,param=fit$best)
 
 localmodel=function(param,x) return(model(x,param))
 nosamples=dim(fit$fullparam[keep,,drop=FALSE])[1]

 if(nosamples>1000) r=sample.int(n=nosamples,size=1000)
 else r=1:nosamples
 #  manypred=array(apply(array(paramarray[r,],dim=c(nosamples,noparam)),1,localmodel,x=fit$data),dim=c(nosamples,1))  ## Don't know what I did here; dim=c(nosamples,1) makes no sense
 manypred=(t(apply(array(paramarray[r,],dim=c(length(r),noparam)),1,localmodel,x=fit$data)))  ## This will fail sometime
 
 predmean=colMeans(manypred)
 predCI=apply(manypred,2,CI)
 
 fit$pred=data.frame(x=fit$data,obs=fit$obs,best=predbest,mean=predmean,low=predCI[1,],hi=predCI[2,])
 fit$many=manypred
 fit$keep=keep

 return(fit)
}
# </source>
# </function>
# 

