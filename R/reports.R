export_reports <- function(customers, products, orders, out_dir = "reports") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  detail <- prepare_order_detail(customers, products, orders)
  write.csv(customers_to_df(customers), file.path(out_dir, "customers.csv"), row.names = FALSE)
  write.csv(products_to_df(products), file.path(out_dir, "products.csv"), row.names = FALSE)
  write.csv(orders_to_df(orders), file.path(out_dir, "orders.csv"), row.names = FALSE)
  write.csv(quarterly_summary(detail), file.path(out_dir, "quarterly_summary.csv"), row.names = FALSE)
  write.csv(category_summary(detail), file.path(out_dir, "category_summary.csv"), row.names = FALSE)
  write.csv(product_summary(detail), file.path(out_dir, "product_summary.csv"), row.names = FALSE)
  write.csv(customer_summary(detail), file.path(out_dir, "customer_summary.csv"), row.names = FALSE)
  write.csv(low_stock_report(products), file.path(out_dir, "low_stock.csv"), row.names = FALSE)
  invisible(TRUE)
}

plot_reports <- function(detail, output_dir = "reports/figures") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2 first.")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  q <- quarterly_summary(detail)
  if (nrow(q)) {
    p <- ggplot2::ggplot(q, ggplot2::aes(Quarter, Net_Revenue)) +
      ggplot2::geom_col() + ggplot2::labs(title = "Quarterly Completed Revenue", x = "Quarter", y = "Revenue (₹)")
    ggplot2::ggsave(file.path(output_dir, "quarterly_revenue.png"), p, width = 8, height = 5)
  }
  c <- category_summary(detail)
  if (nrow(c)) {
    p <- ggplot2::ggplot(c, ggplot2::aes(Category, Net_Revenue)) +
      ggplot2::geom_col() + ggplot2::labs(title = "Revenue by Category", x = NULL, y = "Revenue (₹)") + ggplot2::coord_flip()
    ggplot2::ggsave(file.path(output_dir, "category_revenue.png"), p, width = 8, height = 5)
  }
  p <- product_summary(detail)
  if (nrow(p)) {
    p <- p[order(p$Net_Revenue, decreasing = TRUE), ]
    g <- ggplot2::ggplot(utils::head(p, 10), ggplot2::aes(reorder(Product_Name, Net_Revenue), Net_Revenue)) +
      ggplot2::geom_col() + ggplot2::coord_flip() + ggplot2::labs(title = "Top Revenue Products", x = NULL, y = "Revenue (₹)")
    ggplot2::ggsave(file.path(output_dir, "top_products.png"), g, width = 9, height = 6)
  }
  invisible(TRUE)
}
