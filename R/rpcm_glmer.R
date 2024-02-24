

# lme4 Paket benoetigt

#@Marie: Ich habe include_offset in der Funktion bestimmt. Dann benoetigt die rpcm_gmer
# Funktion keine weiteren Argumente, oder?
rpcm_glmer <- function(data_long) {

  include_offset <- ifelse((sum(data_long$log_time_limit) == 0),  FALSE,  TRUE )

  if(include_offset) {
    fit <- glmer(score ~ -1 + (1|id) + item,
                 data = data_long,
                 family = poisson,
                 offset = log_time_limit)

  } else {
    fit <- glmer(score ~ -1 + (1|id) + item,
                 data = data_long,
                 family = poisson)
  }

  return(fit)
}
