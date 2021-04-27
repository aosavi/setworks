pairwise_setwork <- function(data, elements, sets) {

  # Prepare data ===============================================================

  df <- data %>% dplyr::select(-!!elements)
  df <- df %>% dplyr::select(!!!sets)

  set_names <- names(df)
  set_n <- length(set_names)
  names(df) <- as.character(1:set_n)

  # Transform data =============================================================

  # Nodes
  df_nodes <- df %>%
    colSums() %>%
    tibble::as_tibble(rownames = "name") %>%
    dplyr::rename(size = value) %>%
    dplyr::mutate(label = set_names)

  # Pairwise intersections
  cols <- 1:set_n

  combs <- asplit(combn(cols, 2), 2)

  lapply(combs, function(x) {
    df <<- df %>% dplyr::mutate(!!paste0(x, collapse = " ") :=
                                  as.numeric(rowMeans(df[, x]) == 1))
  })

  # Edges
  df_edges <- df[-cols] %>%
    colSums() %>%
    tibble::as_tibble(rownames = "name") %>%
    tidyr::separate(name, c("from", "to")) %>%
    dplyr::rename(size = value)

  # Create graph ===============================================================

  graph <- tidygraph::tbl_graph(nodes = df_nodes, edges = df_edges)

  graph %>%
    ggraph::ggraph(layout = 'linear', circular = TRUE) +
    ggplot2::scale_size(range = c(1, 14)) +
    ggraph::scale_edge_size(range = c(1, 10)) +
    ggraph::geom_edge_link(aes(width = size),
                           color = "dimgrey") +
    ggraph::geom_node_point(aes(size = size),
                            shape = 21,
                            color = "dimgrey",
                            fill = "white") +
    ggraph::geom_node_label(aes(label = label),
                            nudge_y = -.1,
                            size = 3,
                            color = "white",
                            fill = "grey") +
    ggplot2::theme_void() +
    ggplot2::labs(size = "Set Size", edge_width = "Intersection Size") +
    ggplot2::theme(legend.position = "bottom",
                   plot.margin = unit(c(1, 1, 1, 1), "pt")) +
    ggplot2::guides(size = guide_legend(title.position = "top"),
                    edge_width = guide_legend(title.position = "top")) +
    ggplot2::scale_x_continuous(expand = expansion(c(.075, .075))) +
    ggplot2::scale_y_continuous(expand = expansion(c(.10, .10)))
}
