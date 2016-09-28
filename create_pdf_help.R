#make pdf help file from all .Rd files
pack <- "israelcurves"
path <- find.package(pack)
system(paste(shQuote(file.path(R.home("bin"), "R")),"CMD", "Rd2pdf", shQuote(path)))
