discount_rate <- function(x, amount, ...) UseMethod("discount_rate")

discount_rate.Electronics <- function(x, amount, ...) ifelse(amount >= 10000, 0.05, 0.02)
discount_rate.Fashion <- function(x, amount, ...) ifelse(amount > 3000, 0.10, 0.05)
discount_rate.`Home & Kitchen` <- function(x, amount, ...) 0.06
discount_rate.`Books & Stationery` <- function(x, amount, ...) 0

discount_rate.Product <- function(x, amount, ...) {
  method <- getS3method("discount_rate", class(x)[1], optional = TRUE)
  if (is.null(method)) 0 else method(x, amount, ...)
}

apply_discount <- function(product, amount) {
  require_or_stop(inherits(product, "Product"), "Expected a Product object.")
  rate <- discount_rate(product, amount)
  list(rate = as.numeric(rate), discount = amount * as.numeric(rate),
       net = amount * (1 - as.numeric(rate)))
}
