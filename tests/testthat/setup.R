# Setup file for tests
library(dplyr)
# List files that are present before the start of testing
# if (!exists("okfiles")) {
#   okfiles <- list.files(here::here("tests/"), ".Rdata", full.names = T)
# }

`%>%` <- magrittr::`%>%`

url_unreachable <- function(url) {
  ("try-error" %in% class(try(xml2::read_html(url), silent = T)))
}
