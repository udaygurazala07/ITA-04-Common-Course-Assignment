VALID_CATEGORIES <- c("Electronics", "Fashion", "Home & Kitchen", "Books & Stationery")
VALID_PAYMENTS <- c("UPI", "Card", "Cash", "Wallet")

validate_id <- function(x) is.character(x) && length(x) == 1 && nzchar(trimws(x))
validate_quantity <- function(qty) is.numeric(qty) && length(qty) == 1 && !is.na(qty) && qty > 0 && qty == as.integer(qty)
validate_price <- function(price) is.numeric(price) && length(price) == 1 && !is.na(price) && price >= 0
validate_category <- function(x) x %in% VALID_CATEGORIES
validate_payment <- function(x) x %in% VALID_PAYMENTS
require_or_stop <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

validate_customer <- function(id, name, city) {
  require_or_stop(validate_id(id), "Customer ID is required.")
  require_or_stop(validate_id(name), "Customer name is required.")
  require_or_stop(validate_id(city), "City is required.")
  TRUE
}

validate_product <- function(id, name, category, price, stock) {
  require_or_stop(validate_id(id), "Product ID is required.")
  require_or_stop(validate_id(name), "Product name is required.")
  require_or_stop(validate_category(category), "Invalid product category.")
  require_or_stop(validate_price(price), "Price must be non-negative.")
  require_or_stop(validate_quantity(stock) || identical(as.integer(stock), 0L), "Stock must be a non-negative integer.")
  TRUE
}
