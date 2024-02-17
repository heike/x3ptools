x3p_bin_rotate_1 <- x3p_vertical_1(x3p_inner_impute, min_score_cut = 25, loess_span = 0.2, ifplot = TRUE)
x3p_bin_rotate_2 <- x3p_vertical_2(x3p_inner_impute, min_score_cut = 25, loess_span = 0.2, ifplot = TRUE)

identical(x3p_bin_rotate_1, x3p_bin_rotate_2)

x3p_vertical_1(x3p_bin_rotate_1, min_score_cut = 25, loess_span = 0.2, ifplot = TRUE) %>%
  attr("nfline_red_plot")
ggsave("inst/vertical_1.png")
x3p_vertical_2(x3p_bin_rotate_2, min_score_cut = 25, loess_span = 0.2, ifplot = TRUE) %>%
  attr("nfline_red_plot")
ggsave("inst/vertical_2.png")


x3p_vertical_1(x3p_bin_rotate_1, min_score_cut = 25, loess_span = 0.2, ifplot = TRUE) %>%
  attr("MLE_loess_red_plot")
x3p_vertical_2(x3p_bin_rotate_2, min_score_cut = 25, loess_span = 0.2, ifplot = TRUE) %>%
  attr("MLE_loess_red_plot")