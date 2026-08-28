dependency_matrix <- function(schema) {
  nms <- table_names(schema)
  n <- length(nms)
  m <- matrix(FALSE, n, n, dimnames = list(nms, nms))

  for (i in seq_len(n)) {
    for (fk in keep_class(schema@tables[[i]]@constraints, sqlr_foreign_key)) {
      j <- match(fk@ref_table, nms)
      if (!is.na(j)) m[i, j] <- TRUE
    }
  }

  m
}

reachability <- function(m) {
  for (k in seq_len(nrow(m))) {
    for (i in seq_len(nrow(m))) {
      if (m[i, k]) m[i, ] <- m[i, ] | m[k, ]
    }
  }

  m
}

cyclic_foreign_keys <- function(schema) {
  nms <- table_names(schema)
  reach <- reachability(dependency_matrix(schema))

  lapply(seq_along(schema@tables), function(i) {
    vapply(
      schema@tables[[i]]@constraints,
      function(con) {
        if (!S7_inherits(con, sqlr_foreign_key)) {
          return(FALSE)
        }
        j <- match(con@ref_table, nms)
        !is.na(j) && (j == i || reach[j, i])
      },
      logical(1L)
    )
  })
}

table_order <- function(schema, hoisted) {
  nms <- table_names(schema)
  n <- length(nms)

  deps <- lapply(seq_len(n), function(i) {
    inline <- schema@tables[[i]]@constraints[!hoisted[[i]]]
    targets <- vapply(
      keep_class(inline, sqlr_foreign_key),
      function(fk) fk@ref_table,
      character(1L)
    )
    setdiff(match(intersect(targets, nms), nms), i)
  })

  ordered <- integer()
  pending <- seq_len(n)

  while (length(pending)) {
    ready <- pending[vapply(
      pending,
      function(i) all(deps[[i]] %in% ordered),
      logical(1L)
    )]

    if (!length(ready)) {
      ready <- pending[[1L]]
    }

    ordered <- c(ordered, ready)
    pending <- setdiff(pending, ready)
  }

  ordered
}
