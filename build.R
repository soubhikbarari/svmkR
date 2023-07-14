devtools::build_vignettes() # note: do not edit `/doc`, only edit `/vignettes`
devtools::document()
devtools::build()
devtools::test()

pkgdown::build_site()