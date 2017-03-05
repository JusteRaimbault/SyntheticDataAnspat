

###
# L2 ANSPAT 2016-2017
#
# Synthetic Data Generation for incomplete questionnaire
#

library(readODS)

##
# Estimation procedure
#   1) Draw supplementary adresses from adress file
#   2) Estimate min transportation time to facs
#   3) Estimate average and max time (simple model of adress,min time)
#   4) Draw dwelling situation conditionnaly to address (Paris or not)
#   5) Modal Choice : MultinomialLogit[dwelling;tpsmin;tpsmax]
#   6) Comments conditionnaly to modal choice
#   7) Choice of fac : proba of distance (gravity model) ; random then
#   8) Service access : somehow similar to fac
#
 







