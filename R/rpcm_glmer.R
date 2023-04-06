
# convert data from wide format to long format
# lme4 Paket benoetigt
rpcm_glmer <- function(data_long) { # TODO noch die Argument einfuegen

  # TODO wir bruachen: include_offset; das ist TRUE wenn wir einen offset drin haben wollen,
  # sonst FALSE - vgl. E-Mail

  if(include_offset) {
    fit <- glmer(score ~ -1 + (1|id) + item,
                  data = data_long,
                  family = poisson,
                  offset = log_time_limit)

  } else {
    fit <- glmer(core ~ -1 + (1|id) + item,
                 data = data_long,
                 family = poisson)
  }

  return(fit)
}
