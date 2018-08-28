fileConn <- file("../biblio/bib.txt")
writeLines(dir("../biblio"), fileConn)
close(fileConn)