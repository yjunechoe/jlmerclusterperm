df_to_NT <- function(df) {
  JuliaConnectoR::juliaLet("NamedTuple{Tuple(Symbol.(x))}(y)", x = colnames(df), y = unname(as.list(df)))
}
