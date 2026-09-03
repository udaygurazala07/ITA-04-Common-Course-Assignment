register_customer <- function(customers, id, name, city) {
  validate_customer(id, name, city)
  require_or_stop(!id %in% vapply(customers, `[[`, "", "customer_id"), "Customer ID already exists.")
  c(customers, setNames(list(new_customer(id, name, city)), id))
}

add_product <- function(products, id, name, category, price, stock) {
  validate_product(id, name, category, price, stock)
  require_or_stop(!id %in% vapply(products, `[[`, "", "product_id"), "Product ID already exists.")
  c(products, setNames(list(new_product(id, name, category, price, stock)), id))
}

update_stock <- function(products, product_id, delta) {
  require_or_stop(product_id %in% names(products), "Product not found.")
  new_stock <- products[[product_id]]$stock + as.integer(delta)
  require_or_stop(new_stock >= 0, "Stock cannot become negative.")
  products[[product_id]]$stock <- new_stock
  products
}

place_order <- function(customers, products, orders, customer_id, product_id,
                        quantity, payment_mode, date = Sys.Date()) {
  require_or_stop(customer_id %in% names(customers), "Customer not found.")
  require_or_stop(product_id %in% names(products), "Product not found.")
  require_or_stop(validate_quantity(quantity), "Quantity must be a positive integer.")
  require_or_stop(validate_payment(payment_mode), "Invalid payment mode.")
  product <- products[[product_id]]
  require_or_stop(product$stock >= quantity, "Insufficient stock.")
  gross <- quantity * product$price
  sale <- apply_discount(product, gross)
  id <- sprintf("O%03d", length(orders) + 1L)
  order <- new_order(id, customer_id, product_id, date, quantity,
                     product$price, payment_mode, "Completed")
  orders <- c(orders, setNames(list(order), id))
  products <- update_stock(products, product_id, -quantity)
  list(customers = customers, products = products, orders = orders,
       order = order, sale = sale)
}

cancel_order <- function(products, orders, order_id) {
  require_or_stop(order_id %in% names(orders), "Order not found.")
  order <- orders[[order_id]]
  require_or_stop(order$status == "Completed", "Only completed orders can be cancelled.")
  order$status <- "Cancelled"
  orders[[order_id]] <- order
  products <- update_stock(products, order$product_id, order$quantity)
  list(products = products, orders = orders)
}
