library(knitr)
knit("vignettes/glca.Rmd.orig", output = "vignettes/glca.Rmd")

# remove file path vignettes
replace <- readLines("vignettes/glca.Rmd")
replace <- gsub("<img src=\"vignettes/", "<img src=\"", replace)
fileConn <- file("vignettes/glca.Rmd")
writeLines(replace, fileConn)
close(fileConn)
