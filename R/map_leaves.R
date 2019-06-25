map_leaves <- function(.x, .f, ...){
    UseMethod("map_leaves")
}

map_leaves.default <- function(.x, .f, ...){
    if (rlang::is_bare_list(.x)) {
        purrr::map(.x, map_leaves, .f, ...)    # recurse!
    } else {
        .f <- purrr::as_mapper(.f, ...)
        .f(.x, ...)    # call function on node
    }
}
##l %>% map_leaves(~.x + 1) %>% str()

leafapply <- function(X, FUN, ...){
    if (is.list(X) && is.null(attr(X, "class"))) {
        lapply(X, leafapply, FUN, ...)
    } else {
        FUN(X, ...)
    }
}
##ltcars <- leafapply(l, function(x) mtcars[x, ])
##leafapply(ltcars, function(x) aggregate(. ~ 1, x, mean))
deflate <- function(.x, .f, ...){
    UseMethod("deflate")
}

deflate.default <- function(.x, .f, ...){
    .f <- purrr::as_mapper(.f)
    is_sublist <- purrr::map_lgl(.x, rlang::is_bare_list)
    .x[is_sublist] <- purrr::map(.x[is_sublist], deflate, .f, ...)
    purrr::invoke(.f, .x, ...)
}
##deflate(l, c)


Deflate <- function(f, x, ...){
    is_sublist <- vapply(x, function(y) is.list(y) && is.null(attr(y, "class")), logical(1))
    x[is_sublist] <- lapply(x[is_sublist], Deflate, f = f, ...)
    do.call(f, c(x, ...))
}

