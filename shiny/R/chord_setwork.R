chord_setwork <- function(data, elements, sets) {

  # Prepare data ===============================================================

  df <- data %>% dplyr::select(!!!sets, !!elements)
  df <- df %>% rename(name = !!elements)
  set_names <- colnames(df %>% dplyr::select(!!!sets))
  superset <- "Super Set"

  # Transform data =============================================================

  # Edges: from set to element
  df_edges <- df %>%
    tidyr::pivot_longer(!name, names_to = "from") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(to = paste(c(name, from), collapse = " ")) %>%
    dplyr::ungroup() %>%
    dplyr::filter(value == 1) %>%
    dplyr::select(from, to) %>%
    dplyr::bind_rows(tibble(from = superset, to = set_names))

  # Nodes: elements and sets
  df_nodes <- df %>%
    tidyr::pivot_longer(!name, names_to = "from") %>%
    dplyr::rename(actual_name = name,
                  set = from) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(name = paste(c(actual_name, set), collapse = " ")) %>%
    dplyr::ungroup() %>%
    dplyr::filter(value == 1) %>%
    dplyr::select(name, actual_name, set) %>%
    dplyr::bind_rows(tibble(name = set_names,
                            actual_name = set_names,
                            set = set_names)) %>%
    dplyr::bind_rows(tibble(name = superset,
                            actual_name = superset,
                            set = superset)) %>%
    dplyr::arrange(set, name)

  # Intersections
  df_intersect <- tibble::as_tibble(t(combn(set_names, 2))) %>%
    tibble::rowid_to_column("intersection") %>%
    dplyr::rename(from_set = V1, to_set = V2) %>%
    dplyr::bind_rows(tibble(intersection = .$intersection,
                            from_set = .$to_set,
                            to_set = .$from_set))

  # Connections: from element to element
  df_connect <- df_nodes %>%
    dplyr::right_join(df_nodes, by = "actual_name") %>%
    dplyr::select(-actual_name) %>%
    dplyr::rename(from = name.x, to = name.y,
                  from_set = set.x, to_set = set.y) %>%
    dplyr::filter(from_set != to_set) %>%
    dplyr::left_join(df_intersect, by = c("from_set", "to_set"))

  # Pick correct indices
  from <- match(df_connect$from, df_nodes$name)
  to <- match(df_connect$to, df_nodes$name)
  col <- factor(df_connect$intersection)

  # Create graph ===============================================================

  graph <- tidygraph::tbl_graph(nodes = df_nodes, edges = df_edges)

  graph %>%
    ggraph::ggraph(layout = 'dendrogram', circular = TRUE) +
    ggraph::geom_conn_bundle(data = ggraph::get_con(from = from,
                                                    to = to,
                                                    col = col),
                             alpha = .3,
                             aes(color = col),
                             width = 2,
                             tension = .8) +
    ggraph::geom_node_point(aes(x = x * 1,
                                y = y * 1,
                                filter = leaf,
                                color = set),
                            size = 2) +
    ggraph::geom_node_text(aes(x = x * 1.025,
                               y = y * 1.025,
                               filter = leaf,
                               label = actual_name,
                               angle = -((-ggraph::node_angle(x, y) +
                                            90)%%180) + 90,
                               hjust = "outward",
                               color = set),
                           size = 2) +
    ggraph::scale_edge_color_discrete() +
    ggplot2::scale_color_discrete() +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::labs(color = "Set",
                  edge_color = "Intersection") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(color = guide_legend(title.position = "top", nrow = 2),
                    edge_color = guide_legend(title.position = "top",
                                              override.aes = list(alpha = 1))) +  # https://github.com/thomasp85/ggraph/issues/291
    ggplot2::theme(plot.margin = unit(c(1, 1, 1, 1), "pt")) +
    ggplot2::scale_x_continuous(expand = expansion(c(.075, .075))) +
    ggplot2::scale_y_continuous(expand = expansion(c(.10, .10))) +
    ggplot2::expand_limits(x = c(-1.2, 1.2), y = c(-1.2, 1.2))
}
