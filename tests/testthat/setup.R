# Setup file for tests
# library(dplyr)
# List files that are present before the start of testing
# if (!exists("okfiles")) {
#   okfiles <- list.files(here::here("tests/"), ".Rdata", full.names = T)
# }

`%>%` <- magrittr::`%>%`

url_unreachable <- function(url) {
  ("try-error" %in% class(try(xml2::read_html(url), silent = T)))
}

set.seed(323523)
dftest <- expand.grid(x = 1:6, y = 1:7) %>%
  as.data.frame() %>%
  dplyr::mutate(z = rnorm(42, 0, .1) + sqrt((x - 3.5)^2 + (y - 3.5)^2),
                value = z)

x3ptest <- df_to_x3p(dftest[,c(1, 2, 4)])


bigdf <- expand.grid(x = 0:50, y = 0:100) %>%
  as.data.frame() %>%
  dplyr::mutate(z = sqrt((x - 25.5)^2 + (y - 50.5)^2),
                value = z)

x3pbig <- df_to_x3p(bigdf[,c(1, 2, 4)]) %>%
  x3p_add_mask()
