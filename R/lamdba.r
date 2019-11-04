##' @title lambda funtion
##' @importFrom rlang f_lhs
##' @importFrom rlang f_env
##' @importFrom rlang set_names
lambda <- function(x){
    arg_names <- all.vars(f_lhs(x))
    new_function(args = set_names(rep(list(expr()), 
                                      length(arg_names)), 
                                  arg_names), 
                 body = f_rhs(x), 
                 env = f_env(x))
}
