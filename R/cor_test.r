##' @title cor.test for data.frame
##' @importFrom broom tidy
##' @importFrom purrr map_df
##' @importFrom purrr pmap
##' @importFrom magrittr %>%
cor_test<-function(x, use.colnames = TRUE,self = TRUE){
  if(use.colnames == TRUE){
    comb <- expand.grid(colnames(x),colnames(x))
    }else{
    comb <- expand.grid(rownames(x),rownames(x))
    }
  if(self == FALSE){
    comb <- comb[comb$Var1 != comb$Var2, ]
    }
  res <- pmap(comb, ~cor.test(x[[.x]],x[[.y]]))%>%map_df(tidy)%>%cbind(comb,.)
  return(res)
  }
