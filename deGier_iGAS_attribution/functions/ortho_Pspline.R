#' Create orthogonal P-spline basis
#'
#' @param x_data Numeric vector of values at which to evaluate the matrices
#' @param x_pred Numeric vector of values at which to evaluate the matrices for prediction
#' @param x_min Minimum value for x
#' @param x_max Maximum value for x
#' @param n_seg Number of segments of the B-spline basis. Default is 20
#' @param spline_degree Positive integer giving the spline_degree of the spline function
#' @param diff_ord Positive interger giving the order of the differences
#' @param cyclic Locical indicating if the spline should be cyclic
#' @param rank Numeric value indicating rank reduction
#' @param tol Numeric value indicating the tolerance
#'
#' @author Jan van de Kassteele
#'
#' @return A list containing
#' \item Xu_data Model matrix with polynomial without intercept at x_data
#' \item Xp_data Model matrix with reduced rank orthogonal basis at x_data
#' \item Xu_pred Model matrix with polynomial without intercept at x_pred, if provided
#' \item Xp_pred Model matrix with reduced rank orthogonal basis at x_pred, if provided
#'
#' @references
#' \item https://www.jstatsoft.org/index.php/jss/article/view/v043i14/v43i14.pdf, page 8 - 9
#' \item https://github.com/fabian-s/spikeSlabGAM/blob/master/R/utils.R
#'
#' @export
ortho_Pspline <- function(
  x_data, x_pred = NULL,
  x_min = min(c(x_data, x_pred)), x_max = max(c(x_data, x_pred)),
  n_seg = 20, spline_degree = 3, diff_ord = 2, cyclic = FALSE,
  rank = 0.999, tol = 1e-10) {

  # Function to shift and wrap vector p places in forward direction
  # We use this function to create the cyclic difference operator matrix
  shift <- function(x, p) {
    n <- length(x)
    x[c(seq_len(n)[-seq_len(n - p)], seq_len(n - p))]
  }

  # Difference order should be > 0
  if (diff_ord <= 0) {
    stop("Difference order should be > 0")
  }

  # Create B-spline basis
  if (!cyclic) {
    # Model matrix for regular B-spline basis
    dx <- (x_max - x_min)/n_seg
    knots <- seq(from = x_min - spline_degree*dx, to = x_max + spline_degree*dx, by = dx)
    B_data <- splines::splineDesign(x = x_data, knots = knots, ord = spline_degree + 1, outer.ok = TRUE)
  } else {
    # Model matrix for cyclic B-spline basis
    knots <- seq(from = x_min, to = x_max, length = n_seg + 1)
    B_data <- mgcv::cSplineDes(x = x_data, knots = knots, ord = spline_degree + 1)
  }

  # Create difference operator matrix D
  k <- ncol(B_data)
  D <- diff(diag(k), diff = diff_ord)
  if (cyclic) {
    # For cyclic P-splines, add additional rows to D
    for (i in 1:diff_ord) {
      D <- rbind(D, shift(D[k - diff_ord, ], p = i))
    }
  }
  # Create penalty matrix
  P <- crossprod(D)

  # Create unpenalized null space of the penalty
  # No intercept is included here. Include the intercept in your model
  # This becomes the unpenalized part Xu
  # The unpenalized effects, e.g. the constant (d = 1) or the straight line (d = 2) are called the null space of the penalty
  # Cyclic P-splines have a null space that is always constant
  if (diff_ord - 1 > 0 & !cyclic) {
    # Differences of order d penalize deviations from a polynomial of order d − 1
    Xu_data <- poly(x_data, degree = diff_ord - 1, simple = TRUE)
    attr(Xu_data, "dimnames") <- NULL
  } else {
    warning("No null space. Adjust your model accordingly!")
    Xu_data <- NULL
  }

  # Center B-spline basis around the null space
  # This becomes the penalized part Xp
  # Include the intercept here to induce a sum-to-zero constraint
  Xp_data <- qr.resid(
    qr = qr(cbind(rep(1, length(x_data)), Xu_data)),
    y = B_data)

  # Create reduced rank orthogonal Xp
  # Only the first ncol_Xp non-null eigenvectors and -values whose sum represents at least a given rank of
  # the sum of all eigenvalues are used to construct the reduced rank orthogonal basis
  P_inv <- MASS::ginv(P)
  C <- Xp_data %*% P_inv %*% t(Xp_data)
  C_eigen <- eigen(C, symmetric = TRUE)
  null_eigen_values <- C_eigen$values < tol
  ncol_Xp <- max(3, min(which(cumsum(C_eigen$values[!null_eigen_values])/sum(C_eigen$values[!null_eigen_values]) >= rank)))
  use <- 1:ncol_Xp
  Xp_data <- C_eigen$vectors[, use] %*% diag(sqrt(C_eigen$values[use]))

  # Scale Xp to have a marginal standard deviation of approx. 1, in other words:
  # (Xp_data/sd_ref) %*% matrix(rnorm(n = ncol_Xp*1000), nrow = ncol_Xp, ncol = 1000) |> apply(MARGIN = 1, FUN = sd) should be around 1
  # Do this because marginal standard deviation increases with n_seg and diff_ord
  # See https://www.sciencedirect.com/science/article/abs/pii/S2211675313000407 eq. (7)
  sd_ref <- exp(mean(0.5*log(diag(C))))
  Xp_data <- Xp_data/sd_ref

  # Create list for output
  output <- list(
    Xu_data = Xu_data,
    Xp_data = Xp_data)

  # If x_pred is provided, also include model matrices for prediction
  # These are based on interpolation
  # In case of extrapolation the results should be handled with great care!
  if (!is.null(x_pred)) {
    # Warn if extrapolation occurs
    if (any(x_pred < x_min | x_pred > x_max)) {
      warning("Predictions outside fitted range!")
    }
    output_pred <- lapply(
      # Take Xu_data and Xp_data
      X = output,
      FUN = function(X) {
        # Do nothing if X is NULL
        if (is.null(X)) return(NULL)
        apply(
          # For each column in Xu_data and Xp_data...
          X = X,
          MARGIN = 2,
          FUN = function(y) {
            # ...make a (periodic) spline interpolation from x_data to x_pred
            if (!cyclic) {
              splinefun(x = x_data, y = y, method = "natural")(x = x_pred)
            } else {
              splinefun(x = x_data, y = y, method = "periodic")(x = x_pred)
            }
          }
        )
      }
    )
    names(output_pred) <- c("Xu_pred", "Xp_pred")
    output <- c(output, output_pred)
  }

  # Return output
  return(output)

}
