## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(x3ptools)
logo <- read_x3p(system.file("csafe-logo.x3p", package="x3ptools"))
names(logo)

## ------------------------------------------------------------------------
logo$header.info

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

