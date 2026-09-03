new_customer <- function(id, name, city) {
  x <- list(customer_id = id, customer_name = name, city = city)
  class(x) <- c("Customer", "list")
  x
}

new_product <- function(id, name, category, price, stock) {
  x <- list(product_id = id, product_name = name, category = category,
            price = as.numeric(price), stock = as.integer(stock))
  class(x) <- c(category, "Product", "list")
  x
}

new_order <- function(id, customer_id, product_id, date, quantity,
                     unit_price, payment_mode, status = "Completed") {
  x <- list(order_id = id, customer_id = customer_id, product_id = product_id,
            order_date = as.Date(date), quantity = as.integer(quantity),
            unit_price = as.numeric(unit_price), payment_mode = payment_mode,
            status = status)
  class(x) <- c("Order", "list")
  x
}

print.Customer <- function(x, ...) {
  cat(x$customer_id, "-", x$customer_name, "-", x$city, "\n")
}

print.Product <- function(x, ...) {
  cat(x$product_id, "-", x$product_name, "|", x$category,
      "| Price: ₹", x$price, "| Stock:", x$stock, "\n", sep = "")
}

print.Order <- function(x, ...) {
  cat(x$order_id, "| Customer:", x$customer_id, "| Product:", x$product_id,
      "| Qty:", x$quantity, "| Status:", x$status, "\n")
}

customers_to_df <- function(x) {
  if (!length(x)) return(data.frame())
  do.call(rbind, lapply(x, function(z) as.data.frame(z, stringsAsFactors = FALSE)))
}

products_to_df <- function(x) {
  if (!length(x)) return(data.frame())
  do.call(rbind, lapply(x, function(z) as.data.frame(z, stringsAsFactors = FALSE)))
}

orders_to_df <- function(x) {
  if (!length(x)) return(data.frame())
  out <- do.call(rbind, lapply(x, function(z) as.data.frame(z, stringsAsFactors = FALSE)))
  rownames(out) <- NULL
  out
}
