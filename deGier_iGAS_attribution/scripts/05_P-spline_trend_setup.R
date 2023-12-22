#
# P-spline setup for large-scale time trend
#

# Here we set-up orthogonal P-spline base matrices for efficient sampling in Stan
# It basically reparametrises the B-spline basis matrix and the penalty matrix into
# orthogonal unpenalized null-space part (matrix) and a penalized part (matrix)
# Use the default settings in ortho_Pspline()
ps_setup <- ortho_Pspline(
  x_data = igas_data |> nrow() |> seq_len(),
  n_seg = 20, spline_degree = 3, diff_ord = 2)

# The unpenalized null-space does not include an intercept
# Therefore, add a column with ones to Xu_data
# Then there is no need to explicitly do this in Stan anymore
ps_setup$Xu_data <- cbind(1, ps_setup$Xu_data)
