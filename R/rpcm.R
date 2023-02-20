# main model fitting function
rpcm <- function(data, engine) {

  # TODO input checks

  # TODO data preparation

  # TODO data checks

  # TODO ueber das engine argument geben wir hier dann die verschiedenen
  # fitting optionen hinein
  if (engine == "cml") {
    # TODO hier code einfuegen um das modell mit rpcm_cml zu fitten
    fit <- rpcm_cml()# TODO
  } else if (engine == "glmer") {
    # TODO hier code einfuegen um das modell mit rpcm_glmer zu fitten
    fit <- rpcm_glmer() # TODO
  } else if (engine == "em") {
    # TODO hier code einfuegen um das modell mit rpcm_em zu fitten
    fit <- rpcm_em()
  }

  # TODO prep for output
}
