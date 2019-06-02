##' sort character by the number inside
##' @name sortchar
##' @param x vector of character
##' @param order use order or not
##' @author Kai Guo
##' @export
sortchar<-function(x,order=FALSE){
    tmp<-gsub('[a-zA-Z]','',x)
    tmp<-gsub('[,\\_@\\*\\&\\^\\$\\#\\~]','',tmp)
    tmp<-as.numeric(tmp)
    names(tmp)<-x
    if(order==TRUE){
        return(order(tmp))
    }else{
        return(sort(tmp))
    }
}

##' set colors by giving length
##' @name setcolor
##' @param x number indicate length of color you want have
##' @author Kai Guo
##' @export
setcolor <- function(x){
    mycolor =c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C",
               "#1B9E77", "brown", "#7570B3", "#E7298A", "#7FC97F", "#A6761D",
               "#BEAED4", "#FDC086", "chartreuse1", "cyan3", "purple",
               "pink4", "cyan", "royalblue", "violet", "springgreen2", "gold3",
               "darkseagreen4", "#E5D8BD",
               "#00AFBB", "#FC4E07", "#9999FF", "#FF9326",
               "#984EA3", "#F781BF", "#B3B3B3",
               "#CCCCCC", "#666666", "#01665E", "#542788")
    if(x < length(mycolor)){
        res <- mycolor[seq_len(x)]
    }else{
        res <- c(mycolor, sample(colors(), x - length(mycolor), replace = FALSE))
    }
    return(res)
}