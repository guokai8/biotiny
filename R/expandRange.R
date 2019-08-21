expandRange<-function(x, upstream=3, downstream=3) {
    strand_is_minus = strand(x) == "-"
    on_plus = which(!strand_is_minus)
    on_minus = which(strand_is_minus)
    start(x)[on_plus] = start(x)[on_plus] - upstream
    start(x)[on_minus] = start(x)[on_minus] - downstream
    end(x)[on_plus] = end(x)[on_plus] + downstream
    end(x)[on_minus] = end(x)[on_minus] + upstream
    x
}
