build_filter_query = function(type, filters, server) {
  assert_list(filters, names = "unique")

  filters = imap_chr(filters, function(x, name) {
    if (is.numeric(x)) {
      if (name %in% c("data_id", "task_id", "task", "run", "function")) {
        x = assert_integerish(x, min.len = 1L, any.missing = FALSE, coerce = TRUE)
        paste0(name, "/", paste0(x, collapse = ","))
      } else {
        x = assert_integerish(x,
          min.len = 1L, max.len = 2L, any.missing = FALSE,
          .var.name = sprintf("filter value of '%s'", name), coerce = TRUE
        )
        paste0(name, "/", paste0(x, collapse = ".."))
      }
    } else {
      assert_character(x,
        min.len = 1L, min.chars = 1L, any.missing = FALSE,
        .var.name = sprintf("filter value of '%s'", name)
      )
      paste0(name, "/", x, collapse = "/")
    }
  })

  paste0(
    server, "/json/", type, "/list/",
    paste0(filters, collapse = "/")
  )
}
