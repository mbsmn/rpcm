

# TODO add Data Preparation aus sample_skript_for_purya_with_glmer.R
# convert data from wide format to long format
# lme4 Paket benoetigt
rpcm_glmer <- function() {

  # benoetigte Variablen (im Datensatz)
  # teilweise aus reshape der Datenvorverarbeitung
  # score
  # person = person ID
  # timelimit = Vektor fuer timelimit pro Item (fit2)
  # subset2

  #evtl ueberflüessig

  ####################################################
  # Model 0: Toy model without item parameters       #
  # (not seriously used, just for didactic purposes) #
  ####################################################
  fit0 <- glmer(score ~  # score is the dependent variable
                  (1|person), # this adds a person specific intercept (= the theta*_j variables)
                data = ctlu, # name of dataset
                family = poisson) # poisson model

  ########################################################################################
  # Model 1: Unidimensional RPCM with a log-normal marginal distribution (no time limit) #
  ########################################################################################

  # This model assumes, that theta*_j follows a normal distribution, i.e.
  # theta*_j ~ N(0, (s_theta*)^2)
  # where s_theta* is to be estimated and is interpreted as the standard deviation
  # of the log-person ability. The model further estimates sigma*_i.

  fit1 <- glmer(score ~  # score is the dependent variable
                  -1 + # this suppresses the intercept (interpretation is easier)
                  (1|person) +  # this adds a person specific intercept (= the theta*_j variables)
                  subtest2, # this adds a fixed effect for each task (= the sigma*_i)
                data = ctlu, # name of dataset
                family = poisson) # poisson model

  ###########################################################
  # Model 2: A model for timed data (i.e. t_i unequal to 1) #
  ###########################################################

  # Also this model assumes a log-normal marginal distribution

  # use similar syntax, but add offset for time limit
  fit2 <- glmer(score ~  # score is the dependent variable
                  -1 + # this suppresses the intercept (interpretation is easier)
                  (1|person) +  # this adds a person specific intercept (= the theta*_j variables)
                  subtest2, # this adds a fixed effect for each task (= the sigma*_i)
                data = ctll, # name of dataset
                family = poisson, # poisson model
                offset = log(ctll$timelimit))



}
