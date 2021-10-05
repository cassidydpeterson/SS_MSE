### Run OM with -nohess option ####
# NOTE: careful with windows vs. linux


### function requirements ####
# library(r4ss)


RunOM_NoHess <- function(OMdir, extras = "", intern = FALSE) {
  newdir <- file.path(OMdir)
  oldwd <- getwd() # save working directory
  on.exit(setwd(oldwd))
  setwd(newdir)

  system(paste0("ss -nohess", " ", extras), intern = intern)
}
