prepare_order_detail <- function(customers, products, orders) {
  cdf <- customers_to_df(customers)
  pdf <- products_to_df(products)
  odf <- orders_to_df(orders)
  if (!nrow(odf)) return(data.frame())
  names(cdf) <- c("Customer_ID", "Customer_Name", "City")
  names(pdf) <- c("Product_ID", "Product_Name", "Category", "Unit_Price_Catalogue", "Stock")
  names(odf) <- c("Order_ID", "Customer_ID", "Product_ID", "Order_Date", "Quantity", "Unit_Price", "Payment_Mode", "Status")
  out <- merge(odf, pdf, by = "Product_ID", all.x = TRUE)
  out <- merge(out, cdf, by = "Customer_ID", all.x = TRUE)
  out$Order_Date <- as.Date(out$Order_Date)
  out$Gross_Revenue <- out$Quantity * out$Unit_Price
  out$Discount_Rate <- mapply(function(cat, amount) {
    p <- new_product("tmp", "tmp", cat, amount, 0)
    apply_discount(p, amount)$rate
  }, out$Category, out$Gross_Revenue)
  out$Net_Revenue <- out$Gross_Revenue * (1 - out$Discount_Rate)
  out
}

completed_orders <- function(detail) subset(detail, Status == "Completed")

quarterly_summary <- function(detail) {
  x <- completed_orders(detail)
  if (!nrow(x)) return(data.frame())
  x$Quarter <- paste0("Q", ((as.integer(format(x$Order_Date, "%m")) - 1) %/% 3) + 1,
                      "-", format(x$Order_Date, "%Y"))
  aggregate(Net_Revenue ~ Quarter, x, sum)
}

category_summary <- function(detail) {
  x <- completed_orders(detail)
  if (!nrow(x)) return(data.frame())
  aggregate(Net_Revenue ~ Category, x, sum)
}

product_summary <- function(detail) {
  x <- completed_orders(detail)
  if (!nrow(x)) return(data.frame())
  aggregate(Net_Revenue ~ Product_ID + Product_Name + Category, x, sum)
}

customer_summary <- function(detail) {
  x <- completed_orders(detail)
  if (!nrow(x)) return(data.frame())
  spending <- aggregate(Net_Revenue ~ Customer_ID + Customer_Name + City, x, sum)
  freq <- aggregate(Order_ID ~ Customer_ID, x, length)
  names(freq)[2] <- "Completed_Orders"
  out <- merge(spending, freq, by = "Customer_ID")
  out[order(-out$Net_Revenue), ]
}

low_stock_report <- function(products, threshold = 8) {
  p <- products_to_df(products)
  if (!nrow(p)) return(p)
  names(p) <- c("Product_ID", "Product_Name", "Category", "Price", "Stock")
  p[p$Stock < threshold, , drop = FALSE][order(p[p$Stock < threshold, "Stock"]), ]
}

correlation_analysis <- function(detail) {
  x <- completed_orders(detail)
  if (nrow(x) < 2) return(NA_real_)
  cor(x$Quantity, x$Discount_Rate, use = "complete.obs")
}

revenue_regression <- function(detail) {
  x <- completed_orders(detail)
  lm(Net_Revenue ~ Quantity + Unit_Price + Discount_Rate, data = x)
}

fast_moving_tree <- function(detail) {
  if (!requireNamespace("rpart", quietly = TRUE)) return(NULL)
  p <- product_summary(detail)
  if (!nrow(p)) return(NULL)
  stock <- unique(detail[c("Product_ID", "Stock")])
  d <- merge(p, stock, by = "Product_ID")
  d$Movement <- ifelse(d$Net_Revenue >= median(d$Net_Revenue), "Fast-Moving", "Slow-Moving")
  rpart::rpart(Movement ~ Net_Revenue + Stock + Category, data = d, method = "class")
}
