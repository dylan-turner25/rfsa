.onLoad <- function(libname, pkgname) {
  # set scientific notation options
  options(scipen = 999)

  # set global timeout limit
  options(timeout = 3600)

  # set verbose to F for piggyback (progress bar is not working for some reason)
  options(piggyback.verbose = F)


  # memoise functions
  get_fsa_payments <<- memoise::memoise(get_fsa_payments)

}
