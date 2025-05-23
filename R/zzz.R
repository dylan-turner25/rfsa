.onLoad <- function(libname, pkgname) {
  # set scientific notation options
  options(scipen = 999)

  # set global timeout limit
  options(timeout = 3600)

  # memoise functions
  get_fsa_payments <<- memoise::memoise(get_fsa_payments)

}
