# most packages work fine if installed from CRAN
packs <-
  c(
    "dplyr"
  )
# for (i in 1:length(packs)) {
#   if (!require(packs[i], character.only = TRUE, quietly = TRUE))
#     install.packages(packs[i], character.only = TRUE)
# }

# load all libraries
for (i in 1:length(packs)) {
  library(packs[i], character.only = TRUE)
}

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.18")
BiocManager::install("EBImage")
