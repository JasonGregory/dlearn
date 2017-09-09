#' Summarize data to quickly learn about it.
#'
#' @param data Data frame that you want to explore.
#' @param type Default is basic. For more advanced summary do type = "advanced".
#' @return A data frame
#' @export

summarize_data <- function(data, type = "basic") {
  if (type == "basic") {

    describe <- tibble::tibble(
      variable = names(data),
      class = purrr::map_chr(data, ~class(.x)),
      null_count = purrr::map_dbl(data, ~sum(is.na(.x))),
      distinct_count = purrr::map_dbl(data, ~data.table::uniqueN(.x)),
      total_count = purrr::map_dbl(data, ~length(.x))
    )
    describe <- mutate(describe,
                       null_perc = round(null_count/total_count, 6),
                       distinct_perc = round(distinct_count/total_count, 6)
    ) %>%
      select(variable, class, null_perc, distinct_perc, null_count, distinct_count, total_count)

  } else {

    describe1 <- tibble::tibble(
      variable = names(data),
      class = purrr::map_chr(data, ~class(.x)),
      null_count = purrr::map_dbl(data, ~sum(is.na(.x))),
      distinct_count = purrr::map_dbl(data, ~data.table::uniqueN(.x)),
      total_count = purrr::map_dbl(data, ~length(.x))
    )

    distinct <- describe1 %>% filter(total_count == distinct_count) %>% lselect(variable)

    if (length(distinct) != 0) {

      tbl_distinct <- filter(describe1, variable == distinct) %>%
        mutate(table = list(tibble(value = "distinct",
                                   count = total_count,
                                   rank = 1) %>% distinct)
        ) %>%
        select(variable, table)

      tbl_describe <- tibble::tibble(
        variable = names(select(data, -one_of(distinct))),
        table = select(data, -one_of(distinct)) %>%
          purrr::map(~ data.table::data.table(.x)[, .N, keyby = .x] %>%
                       as_tibble %>%
                       rename(count = N, value = .x) %>%
                       mutate(value = as.character(value)) %>%
                       mutate(rank = min_rank(desc(count))) %>%
                       arrange(desc(count))
          )
      )

      describe1 <- describe1 %>%
        left_join(bind_rows(tbl_distinct, tbl_describe), by = "variable")

      names(describe1$table) <- describe1$variable

    } else {

      describe1 <- describe1 %>%
        mutate(
          table = purrr::map(data, ~ data.table::data.table(.x)[, .N, keyby = .x] %>%
                               as_tibble %>%
                               rename(count = N, value = .x) %>%
                               mutate(value = as.character(value)) %>%
                               mutate(rank = min_rank(desc(count))) %>%
                               arrange(desc(count)))
        )
    }

    numeric <- select_if(data, is.numeric)

    describe2 <- tibble::tibble(
      variable = names(numeric),
      max = purrr::map_dbl(numeric, ~max(.x, na.rm = TRUE)),
      min = purrr::map_dbl(numeric, ~min(.x, na.rm = TRUE)),
      standard_deviation = purrr::map_dbl(numeric, ~sd(.x, na.rm = TRUE)),
      mean = purrr::map_dbl(numeric, ~mean(.x, na.rm = TRUE)),
      median = purrr::map_dbl(numeric, ~median(.x, na.rm = TRUE))
    )

    top <- describe1 %>%
      lselect(table) %>%
      filter(rank == 1 | rank == 2) %>%
      group_by(name, rank) %>%
      mutate(top_count = n()) %>%
      arrange(name, rank) %>%
      ungroup() %>%
      mutate(top_value = if_else(top_count == 1, value, "multiple")) %>%
      select(name, rank, top_value) %>%
      distinct() %>%
      tidyr::spread(rank, top_value) %>%
      rename(top_1 = `1`, top_2 = `2`) %>%
      mutate(top_2 = if_else((top_1 %in% c("multiple","distinct") & is.na(top_2)), top_1, top_2))

    describe <- describe1 %>%
      left_join(describe2, by = "variable") %>%
      left_join(top, by = c("variable" = "name")) %>%
      mutate(null_perc = round(null_count/total_count, 6),
             distinct_perc = round(distinct_count/total_count, 6)
      ) %>%
      select(variable, class, null_perc, distinct_perc, top_1, top_2, max, min, mean, median, standard_deviation, null_count, distinct_count, total_count, table)
    names(describe$table) <- describe$variable
  }
  return(describe)
}

#' #' Summary of variable imporance.
#' #'
#' #' @param data Data frame that you want to explore.
#' #' @param type Default is basic. For more advanced summary do type = "advanced".
#' #' @return A data frame
#' #' @export
#'
#' summarize_importance <- function() {
#'
#' }
#'
#' summarize_plots <- function() {
#'
#' }
#'
#' summarize_models <- function() {
#'
#' }
