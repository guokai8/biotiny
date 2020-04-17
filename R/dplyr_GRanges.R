## This will allow you to use dplyr functions with Bioconductor's
## S4Vectors::DataFrame class. They simply call "as.data.frame" on
## DataFrame objects and then pass them to the methods for
## "data.frame". Note that this entails conversion of S4 vector
## columns to primitive vectors. No attempt is made to convert the
## results back to DataFrame, since such a conversion would not
## restore S4 vector columns that were converted to primitive vectors.

library(S4Vectors)
library(dplyr)

single.table.verbs <- c(
    "filter", "select", "mutate", "transmute", "group_by",
    "summarise", "do", "arrange", "rename", "distinct")
for (verb in single.table.verbs) {
    generic.name <- str_c(verb, "_")
    method.name <- str_c(generic.name, ".DataFrame")
    assign(method.name, local({
        generic <- get(generic.name)
        function(.data, ...) {
            .data %>% as.data.frame %>% generic(...)
        }
    }))
}

two.table.verbs <- c("inner_join", "left_join", "right_join",
                     "full_join", "semi_join", "anti_join", "intersect", "union",
                     "setdiff")
for (verb in two.table.verbs) {
    generic.name <- verb
    method.name <- str_c(generic.name, ".DataFrame")
    assign(method.name, local({
        generic <- get(generic.name)
        function(x, y, ...) {
            generic(as.data.frame(x), as.data.frame(y), ...)
        }
    }))
    ## Set up S4 method to dispatch to dplyr function
    local({
        generic <- get(generic.name, envir=as.environment("package:dplyr"))
        setMethod(verb, c(x="DataFrame", y="ANY"),
                  function(x, y, ...) {
                      generic(x=x, y=y)
                  })
    })
}
