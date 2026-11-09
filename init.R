if (!require("remotes")) install.packages("remotes")
remotes::install_github("jasp-stats/jaspTools")

# Point jaspTools to your local JASP installation
library(jaspTools)
jaspTools::initJaspTools()




setupJaspTools()
setPkgOption("module.dirs", getwd())

library(jaspTools)

# Test Continuous Norming with the cNORM dataset
opts <- analysisOptions("normingContinuous")
opts$raw <- "raw"
opts$group <- "group"
opts$method <- "taylor"
opts$k <- 4
opts$t <- 3
opts$normAge <- "3.5"

runAnalysis("normingContinuous", dataset = cNORM::elfe, options = opts)




# In RStudio console:
if (!require("remotes")) install.packages("remotes")

# Install JASP internal packages
remotes::install_github("jasp-stats/jaspBase")
remotes::install_github("jasp-stats/jaspGraphs")

# Ensure cNORM is installed
if (!require("cNORM")) install.packages("cNORM")

# Generate NAMESPACE and documentation
devtools::document()






library(jaspTools)
setupJaspTools()                       # once; may take a while
setPkgOption("module.dirs", getwd())   # working directory = module root

opts <- analysisOptions("normingContinuous")
opts$raw <- "raw"; opts$group <- "group"
runAnalysis("normingContinuous", dataset = cNORM::elfe, options = opts)
