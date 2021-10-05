####################
# RUN EM ASSESSMENT
####################


### FROM r4ss ####
# library(r4ss)

RunEM <- function(EMdir, extras = "", intern = FALSE) {
  # CallType="system"

  newdir <- file.path(EMdir)
  oldwd <- getwd()
  on.exit(setwd(oldwd))
  setwd(newdir)

  system(paste0("ss -nohess ", extras), intern = intern)
}
