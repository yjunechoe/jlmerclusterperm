skip_conditionally()

stopJulia()
system2("julia", '-e "using InteractiveUtils; println(versioninfo());"')

# Individually run `jlmerclusterperm_setup()`
jlmerclusterperm:::start_with_threads(max_threads = 2, verbose = TRUE)
jlmerclusterperm:::set_projenv(verbose = TRUE)
cat(JuliaConnectoR::juliaCall("Pkg.status"))
cat(readLines(file.path(jlmerclusterperm:::.jlmerclusterperm$opts$projdir, "Manifest.toml")), sep = "\n")
jlmerclusterperm:::source_jl(verbose = TRUE)
jlmerclusterperm:::define_globals()
jlmerclusterperm:::cleanup_jl()
