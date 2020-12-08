################################################################################
# Router logic of the Shiny app
#
# Author: Lathan Liou
# Created: # Fri Nov 27 08:48:40 2020 ------------------------------
############################################################################

library(shiny.router)

router <- make_router(
  route("lond", LOND_page)
)