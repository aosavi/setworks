waterfall_setwork <- function(data, elements, sets) {

  # Prepare data ===============================================================

  df <- data %>% dplyr::select(-!!elements)
  df <- df %>% dplyr::select(!!!sets)
  set_names <- names(df)
  set_n <- length(set_names)
  names(df) <- as.character(1:set_n)

  # Transform data =============================================================

  # Add intersections to the matrix
  cols <- 1:set_n

  combs <- unlist(sapply(cols[-1], function(x) {
    asplit(combn(cols, m = x), 2)
  }), recursive = FALSE)

  # Find all intersections
  lapply(combs, function(x) {
    df <<- df %>% dplyr::mutate(!!paste0(x, collapse = " ") :=
                                  as.numeric(rowMeans(df[, x]) == 1))
  })

  # From wide data frame to long (intersection size) data frame
  intersect_min <- 1
  df_nodes <- df %>%
    dplyr::summarize(across(everything(), sum)) %>%
    tidyr::pivot_longer(everything()) %>%
    dplyr::rename(size = value)

  # Keep intersection >= intersect_min (and always keep full sets)
  keep_ind <- c(1:set_n,
                which(df_nodes$size >= intersect_min)[
                  which(df_nodes$size >= intersect_min) > set_n])
  df_nodes <- df_nodes[keep_ind, ]

  # From intersection size data frame to node and edge data frames
  tmp_list <- unlist(mapply(combn,
                            list(1:set_n),
                            seq_along(1:set_n),
                            simplify = FALSE),
                     recursive = FALSE)
  tmp_list <- tmp_list[keep_ind]

  df_edges <- which(t(sapply(tmp_list, function(x) sapply(tmp_list, function(y) {
    all(x %in% y) & length(y) == length(x) + 1
  }))), arr.ind = TRUE)

  df_edges <- setNames(as.data.frame(df_edges),
                       c("from", "to"))[order(df_edges[,1]),]
  df_edges <- df_edges %>%
    dplyr::left_join(df_nodes %>% tibble::rowid_to_column(),
                     by = c("to" = "rowid"))

  # Create graph ===============================================================

  graph <- tidygraph::tbl_graph(nodes = df_nodes,
                             edges = df_edges)

  layout <- ggraph::create_layout(graph,
                                  layout = "igraph",
                                  algorithm = "sugiyama")

  layout %>% ggraph::ggraph() +
    ggplot2::scale_size(range = c(1, 14),
                        breaks = c(5, 10, 50, 100)) +
    ggraph::scale_edge_size(range = c(1, 10)) +
    ggraph::geom_edge_link(aes(width = size),
                           color = "dimgrey") +
    ggraph::geom_node_point(aes(size = size),
                            shape = 21,
                            color = "dimgrey",
                            fill = "white") +
    ggraph::geom_node_label(aes(label = name),
                            nudge_y = -.3,
                            size = 3,
                            color = "white",
                            fill = "grey") +
    ggplot2::theme_void() +
    ggplot2::labs(size = "Set/Intersection Size", edge_width = "Overlap Size") +
    ggplot2::theme(legend.position = "bottom",
                   plot.margin = unit(c(1, 1, 1, 1), "pt")) +
    ggplot2::guides(size = guide_legend(title.position = "top"),
                    edge_width = guide_legend(title.position = "top")) +
    ggplot2::scale_x_continuous(expand = expansion(c(.075, .075))) +
    ggplot2::scale_y_continuous(expand = expansion(c(.10, .10)))
}
