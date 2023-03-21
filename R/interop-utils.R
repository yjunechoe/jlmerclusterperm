df_to_NT <- function(df) {
  JuliaConnectoR::juliaLet("NamedTuple{Tuple(Symbol.(x))}(y)", x = names(df), y = unname(as.list(df)))
}
