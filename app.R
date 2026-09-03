options(stringsAsFactors = FALSE)

packages <- c("ggplot2", "dplyr", "tidyr", "readr", "rpart")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")

source("R/models.R")
source("R/validation.R")
source("R/discounts.R")
source("R/orders.R")
source("R/analytics.R")
source("R/reports.R")

load_data <- function() {
  customers_df <- read.csv("data/customers.csv")
  products_df <- read.csv("data/products.csv")
  orders_df <- read.csv("data/orders.csv")
  customers <- setNames(lapply(seq_len(nrow(customers_df)), function(i)
    new_customer(customers_df$customer_id[i], customers_df$customer_name[i], customers_df$city[i])), customers_df$customer_id)
  products <- setNames(lapply(seq_len(nrow(products_df)), function(i)
    new_product(products_df$product_id[i], products_df$product_name[i], products_df$category[i], products_df$price[i], products_df$stock[i])), products_df$product_id)
  orders <- setNames(lapply(seq_len(nrow(orders_df)), function(i)
    new_order(orders_df$order_id[i], orders_df$customer_id[i], orders_df$product_id[i], orders_df$order_date[i], orders_df$quantity[i], orders_df$unit_price[i], orders_df$payment_mode[i], orders_df$status[i])), orders_df$order_id)
  list(customers = customers, products = products, orders = orders)
}

state <- load_data()

menu <- function() {
  cat("\n===============================================\n")
  cat(" E-COMMERCE ORDER & CUSTOMER ANALYTICS\n")
  cat("===============================================\n")
  cat("1. Customer Management\n2. Product Management\n3. Place Order\n4. Cancel Order\n5. Inventory Status\n6. Quarterly Sales Analytics\n7. Customer Analytics\n8. Export Reports\n9. Reload Demo Data\n10. Run Statistical Analysis\n0. Exit\n")
}

show_dashboard <- function() {
  d <- prepare_order_detail(state$customers, state$products, state$orders)
  c <- completed_orders(d)
  cat("\nCompleted revenue: ₹", format(sum(c$Net_Revenue), big.mark = ",", nsmall = 2),
      " | Completed orders:", nrow(c),
      " | Customers:", length(state$customers),
      " | Low-stock:", nrow(low_stock_report(state$products)), "\n", sep = "")
}

customer_menu <- function() {
  cat("\n1. Register customer\n2. Search by ID\n3. Search by city\n")
  ch <- readline("Choice: ")
  if (ch == "1") {
    id <- readline("Customer ID: "); name <- readline("Name: "); city <- readline("City: ")
    tryCatch({ state$customers <<- register_customer(state$customers, id, name, city); cat("Customer registered.\n") }, error = function(e) cat("Error:", e$message, "\n"))
  } else if (ch == "2") {
    id <- readline("ID: "); if (id %in% names(state$customers)) print(state$customers[[id]]) else cat("Customer not found.\n")
  } else if (ch == "3") {
    city <- readline("City: "); x <- Filter(function(z) tolower(z$city) == tolower(city), state$customers)
    if (length(x)) invisible(lapply(x, print)) else cat("No customers found.\n")
  }
}

product_menu <- function() {
  cat("\n1. Add product\n2. View catalogue\n3. Update stock\n4. Low-stock report\n")
  ch <- readline("Choice: ")
  if (ch == "1") {
    id <- readline("Product ID: "); name <- readline("Name: "); catg <- readline("Category: ")
    price <- suppressWarnings(as.numeric(readline("Price: "))); stock <- suppressWarnings(as.integer(readline("Stock: ")))
    tryCatch({ state$products <<- add_product(state$products, id, name, catg, price, stock); cat("Product added.\n") }, error = function(e) cat("Error:", e$message, "\n"))
  } else if (ch == "2") invisible(lapply(state$products, print))
  else if (ch == "3") {
    id <- readline("Product ID: "); delta <- suppressWarnings(as.integer(readline("Stock change (+/-): ")))
    tryCatch({ state$products <<- update_stock(state$products, id, delta); cat("Stock updated.\n") }, error = function(e) cat("Error:", e$message, "\n"))
  } else if (ch == "4") print(low_stock_report(state$products))
}

place_order_menu <- function() {
  cid <- readline("Customer ID: "); pid <- readline("Product ID: ")
  qty <- suppressWarnings(as.integer(readline("Quantity: ")))
  pay <- readline("Payment (UPI/Card/Cash/Wallet): ")
  tryCatch({
    result <- place_order(state$customers, state$products, state$orders, cid, pid, qty, pay)
    state$products <<- result$products; state$orders <<- result$orders
    cat("Order", result$order$order_id, "created. Gross ₹", result$sale$discount + result$sale$net,
        ", Discount ", result$sale$rate * 100, "%, Net ₹", result$sale$net, "\n", sep = "")
  }, error = function(e) cat("Order rejected:", e$message, "\n"))
}

analytics_menu <- function() {
  d <- prepare_order_detail(state$customers, state$products, state$orders)
  cat("\nQuarterly:\n"); print(quarterly_summary(d))
  cat("\nCategory:\n"); print(category_summary(d))
  cat("\nTop products:\n"); print(head(product_summary(d)[order(product_summary(d)$Net_Revenue, decreasing = TRUE), ], 10))
  cat("\nLow stock:\n"); print(low_stock_report(state$products))
  try(plot_reports(d), silent = TRUE)
}

run_stats <- function() {
  d <- prepare_order_detail(state$customers, state$products, state$orders)
  cat("\nCorrelation (quantity vs discount rate):", round(correlation_analysis(d), 4), "\n")
  fit <- revenue_regression(d)
  print(summary(fit))
  cat("\nHypothetical order prediction:\n")
  print(predict(fit, data.frame(Quantity = 2, Unit_Price = 2799, Discount_Rate = 0.06)))
  tree <- fast_moving_tree(d)
  if (!is.null(tree)) print(tree)
}

repeat {
  show_dashboard(); menu(); choice <- readline("Select option: ")
  if (choice == "0") break
  if (choice == "1") customer_menu()
  else if (choice == "2") product_menu()
  else if (choice == "3") place_order_menu()
  else if (choice == "4") {
    id <- readline("Order ID: ")
    tryCatch({ z <- cancel_order(state$products, state$orders, id); state$products <<- z$products; state$orders <<- z$orders; cat("Order cancelled and stock restored.\n") }, error = function(e) cat("Error:", e$message, "\n"))
  } else if (choice == "5") print(products_to_df(state$products))
  else if (choice == "6") analytics_menu()
  else if (choice == "7") print(customer_summary(prepare_order_detail(state$customers, state$products, state$orders)))
  else if (choice == "8") { export_reports(state$customers, state$products, state$orders); cat("Reports exported to reports/.\n") }
  else if (choice == "9") { state <<- load_data(); cat("Demo data reloaded.\n") }
  else if (choice == "10") run_stats()
  else cat("Invalid menu choice.\n")
}
cat("Application closed.\n")
