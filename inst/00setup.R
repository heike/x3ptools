library(wire)
library(tidyverse)

x3p <- x3p_read("../Wirecuts/scans/T1AW-LI-R2.x3p")
insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1)
x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res,
                               ifout = FALSE, ifsave = FALSE, dir_name = NULL, ifplot = FALSE
)
