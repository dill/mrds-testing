# testing for fortran code

# load mrds
library(mrds)


# function to put the results into the above structure
res.get<-function(struct,ds.object){

  # what do we have?
  # give the detection function model
  struct$detfct<-c(struct$detfct,paste(ds.object$ds$aux$ftype,"+",ds.object$adj.series,"(",paste(ds.object$adj.order,sep=","),")",sep=""))
  #likelihood	
  struct$lnl<-c(struct$lnl,ds.object$lnl)

  # parameters
  for (i in 1:length(ds.object$par)){
    eval(parse(text=paste("struct$par",i,"<-c(struct$par",i,",ds.object$par[",i,"])",sep=""))) 
  }
  
  # monotonicity constraints
  struct$nonmono<-c(struct$nonmono,ds.object$meta.data$nonmono)
  struct$strict<-c(struct$strict,ds.object$meta.data$strict)

  return(struct)

}

############## lt example from Distance ##################

# make sure the csv is in the same directory
ltexample<-read.csv("linetransectexample.csv")
# something to hold the results
lt.example.results<-list(lnl=c(),par1=c(),par2=c(),par3=c(),nonmono=c(),strict=c())

## Using just the half-normal detection function
# fit
xx<-ddf(dsmodel = ~mcds(key = "hn", formula = ~1), data = ltexample, method = "ds", meta.data = list(width = max(ltexample$distance),engine="foptim",nonmono=FALSE,strict=FALSE))
lt.example.results<-res.get(lt.example.results,xx)
# weak monotonicity
xx<-ddf(dsmodel = ~mcds(key = "hn", formula = ~1), data = ltexample, method = "ds", meta.data = list(width = max(ltexample$distance),engine="foptim",nonmono=TRUE,strict=FALSE))
lt.example.results<-res.get(lt.example.results,xx)
# strict monotonicity
xx<-ddf(dsmodel = ~mcds(key = "hn", formula = ~1), data = ltexample, method = "ds", meta.data = list(width = max(ltexample$distance),engine="foptim",nonmono=TRUE,strict=TRUE))
lt.example.results<-res.get(lt.example.results,xx)

## with adjustment terms (cos order 2), scaled by width
# non
xx<-ddf(dsmodel = ~mcds(key = "hn",adj.series="cos",adj.order=c(2),adj.scale="width", formula = ~1), data = ltexample, method = "ds", meta.data = list(width = max(ltexample$distance),engine="foptim",nonmono=FALSE,strict=FALSE))
lt.example.results<-res.get(lt.example.results,xx)
# weak
xx<-ddf(dsmodel = ~mcds(key = "hn",adj.series="cos",adj.order=c(2),adj.scale="width", formula = ~1), data = ltexample, method = "ds", meta.data = list(width = max(ltexample$distance),engine="foptim",nonmono=TRUE,strict=FALSE))
lt.example.results<-res.get(lt.example.results,xx)
# strict
xx<-ddf(dsmodel = ~mcds(key = "hn",adj.series="cos",adj.order=c(2),adj.scale="width", formula = ~1), data = ltexample, method = "ds", meta.data = list(width = max(ltexample$distance),engine="foptim",nonmono=TRUE,strict=TRUE))
lt.example.results<-res.get(lt.example.results,xx)

## with adjustment terms (cos order 2), scaled by key scale
# non
xx<-ddf(dsmodel = ~mcds(key = "hn",adj.series="cos",adj.order=c(2),adj.scale="scale", formula = ~1), data = ltexample, method = "ds", meta.data = list(width = max(ltexample$distance),engine="foptim",nonmono=FALSE,strict=FALSE))
lt.example.results<-res.get(lt.example.results,xx)
# weak
xx<-ddf(dsmodel = ~mcds(key = "hn",adj.series="cos",adj.order=c(2),adj.scale="scale", formula = ~1), data = ltexample, method = "ds", meta.data = list(width = max(ltexample$distance),engine="foptim",nonmono=TRUE,strict=FALSE))
lt.example.results<-res.get(lt.example.results,xx)
# strict
xx<-ddf(dsmodel = ~mcds(key = "hn",adj.series="cos",adj.order=c(2),adj.scale="scale", formula = ~1), data = ltexample, method = "ds", meta.data = list(width = max(ltexample$distance),engine="foptim",nonmono=TRUE,strict=TRUE))
lt.example.results<-res.get(lt.example.results,xx)

## with adjustment terms (cos order 2 and 4), scaled by width
# non
xx<-ddf(dsmodel = ~mcds(key = "hn",adj.series="cos",adj.order=c(2,4),adj.scale="width", formula = ~1), data = ltexample, method = "ds", meta.data = list(width = max(ltexample$distance),engine="foptim",nonmono=FALSE,strict=FALSE))
lt.example.results<-res.get(lt.example.results,xx)
# weak
xx<-ddf(dsmodel = ~mcds(key = "hn",adj.series="cos",adj.order=c(2,4),adj.scale="width", formula = ~1), data = ltexample, method = "ds", meta.data = list(width = max(ltexample$distance),engine="foptim",nonmono=TRUE,strict=FALSE))
lt.example.results<-res.get(lt.example.results,xx)
# strict
xx<-ddf(dsmodel = ~mcds(key = "hn",adj.series="cos",adj.order=c(2,4),adj.scale="width", formula = ~1), data = ltexample, method = "ds", meta.data = list(width = max(ltexample$distance),engine="foptim",nonmono=TRUE,strict=TRUE))
lt.example.results<-res.get(lt.example.results,xx)


## with adjustment terms (hermite order 3), scaled by width
# non
xx<-ddf(dsmodel = ~mcds(key = "hn",adj.series="herm",adj.order=c(3),adj.scale="width", formula = ~1), data = ltexample, method = "ds", meta.data = list(width = max(ltexample$distance),engine="foptim",nonmono=FALSE,strict=FALSE))
lt.example.results<-res.get(lt.example.results,xx)
# weak
xx<-ddf(dsmodel = ~mcds(key = "hn",adj.series="herm",adj.order=c(3),adj.scale="width", formula = ~1), data = ltexample, method = "ds", meta.data = list(width = max(ltexample$distance),engine="foptim",nonmono=TRUE,strict=FALSE))
lt.example.results<-res.get(lt.example.results,xx)
# strict
xx<-ddf(dsmodel = ~mcds(key = "hn",adj.series="herm",adj.order=c(3),adj.scale="width", formula = ~1), data = ltexample, method = "ds", meta.data = list(width = max(ltexample$distance),engine="foptim",nonmono=TRUE,strict=TRUE))
lt.example.results<-res.get(lt.example.results,xx)


## with adjustment terms (poly order 4), scaled by width
# non
xx<-ddf(dsmodel = ~mcds(key = "hn",adj.series="poly",adj.order=c(4),adj.scale="width", formula = ~1), data = ltexample, method = "ds", meta.data = list(width = max(ltexample$distance),engine="foptim",nonmono=FALSE,strict=FALSE))
lt.example.results<-res.get(lt.example.results,xx)
# weak
xx<-ddf(dsmodel = ~mcds(key = "hn",adj.series="poly",adj.order=c(4),adj.scale="width", formula = ~1), data = ltexample, method = "ds", meta.data = list(width = max(ltexample$distance),engine="foptim",nonmono=TRUE,strict=FALSE))
lt.example.results<-res.get(lt.example.results,xx)
# strict
xx<-ddf(dsmodel = ~mcds(key = "hn",adj.series="poly",adj.order=c(4),adj.scale="width", formula = ~1), data = ltexample, method = "ds", meta.data = list(width = max(ltexample$distance),engine="foptim",nonmono=TRUE,strict=TRUE))
lt.example.results<-res.get(lt.example.results,xx)


########### Williams data
# just doing the final fits from the paper


# something to hold the results
williams.results<-list(lnl=c(),par1=c(),par2=c(),par3=c(),nonmono=c(),strict=c())

library(mrds)
# load the Williams data
source("loadwilliamsdata.R")

# harbour porpoise, truncation at 500, no monotonicity
xx<-ddf(dsmodel = ~mcds(key = "hr", formula = ~1), data = harbourporpoise, method = "ds", meta.data = list(width = 500,engine="foptim",nonmono=FALSE,strict=FALSE))
williams.results<-res.get(williams.results,xx)


