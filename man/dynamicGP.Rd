% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dynamicGP.R
\name{dynamicGP}
\alias{dynamicGP}
\title{Performs a data analysis for data with adaptive treatments.}
\usage{
dynamicGP(
  datafile = NULL,
  dataref = NULL,
  method = "BART",
  stg1.outcome,
  stg1.treatment,
  stg1.x.explanatory = NULL,
  stg1.x.confounding = NULL,
  stg1.tr.hte = NULL,
  stg1.tr.values = NULL,
  stg1.tr.type = "Discrete",
  stg1.time,
  stg1.time.value = NULL,
  stg1.outcome.type = "Continuous",
  stg1.outcome.bound_censor = "neither",
  stg1.outcome.lb = NULL,
  stg1.outcome.ub = NULL,
  stg1.outcome.censor.lv = NULL,
  stg1.outcome.censor.uv = NULL,
  stg1.outcome.censor.yn = NULL,
  stg1.outcome.link = "identity",
  stg1.c.margin = NULL,
  stg2.outcome,
  stg2.treatment,
  stg2.x.explanatory = NULL,
  stg2.x.confounding = NULL,
  stg2.tr1.hte = NULL,
  stg2.tr2.hte = NULL,
  stg2.tr.values = NULL,
  stg2.tr.type = "Discrete",
  stg2.time,
  stg2.time.value = NULL,
  stg2.outcome.type = "Continuous",
  stg2.outcome.bound_censor = "neither",
  stg2.outcome.lb = NULL,
  stg2.outcome.ub = NULL,
  stg2.outcome.censor.lv = NULL,
  stg2.outcome.censor.uv = NULL,
  stg2.outcome.censor.yn = NULL,
  stg2.outcome.link = "identity",
  stg2.c.margin = NULL,
  burn.num = 500,
  mcmc.num = 500,
  x.categorical = NULL,
  mi.datafile = NULL,
  mi.dataref = NULL,
  sheet = NULL,
  mi.sheet = NULL,
  seed = 5000,
  token = NULL,
  use.cache = NULL
)
}
\arguments{
\item{datafile}{File to upload (.csv or .xls)}

\item{dataref}{Reference to already uploaded file.}

\item{method}{The method to be used. "GP" for GP method and "BART" for BART method. The default value is "BART".}

\item{stg1.outcome}{The name of the intermediate outcome variable for stage 1.}

\item{stg1.treatment}{The name of the treatment variable for stage 1.}

\item{stg1.x.explanatory}{A vector of the name of the explanatory variables for stage 1.}

\item{stg1.x.confounding}{A vector of the name of the confounding variables for stage 1.}

\item{stg1.tr.hte}{An optional vector specifying categorical variables which may have heterogeneous treatment effect with the treatment variable for stage 1.}

\item{stg1.tr.values}{User-defined values for the calculation of ATE if the treatment variable is continuous for stage 1.}

\item{stg1.tr.type}{The type of treatment at stage 1. "Continuous" for continuous treatment and "Discrete" for categorical treatment. The default value is "Discrete".}

\item{stg1.time}{Time variable.}

\item{stg1.time.value}{Pre-specified time exposure.}

\item{stg1.outcome.type}{Intermediate outcome type ("Continuous" or "Discrete") for stage 1.}

\item{stg1.outcome.bound_censor}{The default value is "neither".
"neither" if the intermediate outcome is not bounded or censored.
"bounded" if the intermediate outcome is bounded.
"censored" if the intermediate outcome is censored.}

\item{stg1.outcome.lb}{Stage 1 lower bound if the intermediate outcome is bounded.}

\item{stg1.outcome.ub}{Stage 1 upper bound if the intermediate outcome is bounded.}

\item{stg1.outcome.censor.lv}{lower variable of censored interval if the intermediate outcome is censored.}

\item{stg1.outcome.censor.uv}{upper variable of censored interval if the intermediate outcome is censored.}

\item{stg1.outcome.censor.yn}{Censoring variable if the intermediate outcome is censored.}

\item{stg1.outcome.link}{function for the intermediate outcome; the default value is ``identity''.
"identity" if no transformation needed.
"log" for log transformation.
"logit" for logit transformation.}

\item{stg1.c.margin}{An optional vector of user-defined values of c for PrTE at stage 1.}

\item{stg2.outcome}{The name of the outcome variable for stage 2.}

\item{stg2.treatment}{The name of the treatment variable for stage 2.}

\item{stg2.x.explanatory}{A vector of the name of the explanatory variables for stage 2.}

\item{stg2.x.confounding}{A vector of the name of the confounding variables for stage 2.}

\item{stg2.tr1.hte}{At stage 2, an optional vector specifying categorical variables which may have heterogeneous treatment effect with the stage 1 treatment variable}

\item{stg2.tr2.hte}{At stage 2, an optional vector specifying categorical variables which may have heterogeneous treatment effect with the stage 2 treatment variable}

\item{stg2.tr.values}{User-defined values for the calculation of ATE if the treatment variable is continuous for stage 2.}

\item{stg2.tr.type}{The type of treatment at stage 2. "Continuous" for continuous treatment and "Discrete" for categorical treatment. The default value is "Discrete".}

\item{stg2.time}{Time variable.}

\item{stg2.time.value}{Pre-specified time exposure.}

\item{stg2.outcome.type}{Outcome type ("Continuous" or "Discrete") for stage 2.}

\item{stg2.outcome.bound_censor}{The default value is "neither".
"neither" if the intermediate outcome is not bounded or censored.
"bounded" if the intermediate outcome is bounded.
"censored" if the intermediate outcome is censored.}

\item{stg2.outcome.lb}{Stage 2 lower bound if the outcome is bounded.}

\item{stg2.outcome.ub}{Stage 2 upper bound if the outcome is bounded.}

\item{stg2.outcome.censor.lv}{lower variable of censored interval if the outcome is censored.}

\item{stg2.outcome.censor.uv}{upper variable of censored interval if the outcome is censored.}

\item{stg2.outcome.censor.yn}{Censoring variable if the outcome is censored.}

\item{stg2.outcome.link}{function for the outcome; the default value is ``identity''.
"identity" if no transformation needed.
"log" for log transformation.
"logit" for logit transformation.}

\item{stg2.c.margin}{An optional vector of user-defined values of c for PrTE at stage 2.}

\item{burn.num}{numeric; the number of MCMC 'burn-in' samples, i.e. number of MCMC to be discarded. The default value is 500.}

\item{mcmc.num}{numeric; the number of MCMC samples after 'burn-in'. The default value is 500.}

\item{x.categorical}{A vector of the name of categorical variables in data.}

\item{mi.datafile}{File to upload (.csv or .xls) that contains the imputed data in the model.}

\item{mi.dataref}{Reference to already uploaded file that contains the imputed data in the model.}

\item{sheet}{If \code{datafile} or \code{dataref} points to an Excel file this variable specifies which sheet to load.}

\item{mi.sheet}{If \code{mi.datafile} or \code{mi.dataurl} points to an Excel file this variable specifies which sheet to load.}

\item{seed}{Sets the seed. The default value is 5000.}

\item{token}{Authentication token.}

\item{use.cache}{Use cached results (default True).}
}
\value{
jobid
}
\description{
Performs Bayesian's Gaussian process regression or Bayesian
   additive regression tree for data with adaptive treatment(s).
}
