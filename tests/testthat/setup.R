# Setup file for tests
# library(dplyr)
# List files that are present before the start of testing
# if (!exists("okfiles")) {
#   okfiles <- list.files(here::here("tests/"), ".Rdata", full.names = T)
# }

#`%>%` <- dplyr::`%>%`

url_unreachable <- function(url) {
  ("try-error" %in% class(try(xml2::read_html(url), silent = T)))
}


set.seed(323523)
dftest <- expand.grid(x = 1:6, y = 1:7) %>%
  as.data.frame() %>%
  dplyr::mutate(
    z = rnorm(42, 0, .1) + sqrt((x - 3.5)^2 + (y - 3.5)^2),
    value = z,
    mask = factor(sample(c("#ffff00", "#0000ff", "#00ff00"), size = 42, replace=TRUE))
  )

x3ptest <- df_to_x3p(dftest[, c(1, 2, 4)])


x3ptest_mask <- df_to_x3p(dftest[, c(1, 2, 4, 5)])
x3ptest_mask <- x3ptest_mask %>% 
  x3p_add_annotation("#FFFF00", "yellow")  %>%
  x3p_add_annotation("#00FF00", "green")  %>%
  x3p_add_annotation("#0000FF", "blue")  
  
bigdf <- expand.grid(x = 0:50, y = 0:100) %>%
  as.data.frame() %>%
  dplyr::mutate(
    z = sqrt((x - 25.5)^2 + (y - 50.5)^2),
    value = z
  )

x3pbig <- df_to_x3p(bigdf[, c(1, 2, 4)]) %>%
  x3p_add_mask()

# Setup for F/D issue
tmpfile <- tempfile(fileext = ".x3p")
# write a copy of the file into a temporary file
write_x3p(x3ptest, file = tmpfile, size = 4, quiet = T)

# Set up file to read in with mask
tmpfile2 <- tempfile(fileext = ".x3p")
# write a copy of the file into a temporary file
write_x3p(x3ptest %>% x3p_add_mask(), file = tmpfile2, quiet = T)


x3pbig2 <- x3pbig
x3pbig2$mask[1:5, ] <- "black"

x3pbig2$matrix.info$Mask$Annotations <- list(
  Annotation = structure(list("testing"), color = "black")
)

logo <- x3p_read(system.file("csafe-logo.x3p", package = "x3ptools"))
