##########
# Loading and Citing Packages Used
# Created by: Ed Anderson
# Date: 14th April 2017
###########

#Load
library(repmis)

#Load & Cite

PackagesUsed<-c("repmis",
                "httr",
                "data.table"
                )

LoadandCite(PackagesUsed,
            file='Analysis/PackageReferences.bib')
