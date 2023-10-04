skip_conditionally()

JuliaConnectoR::stopJulia()
system2("julia", '-e "using InteractiveUtils; println(versioninfo());"')

jlmerclusterperm:::start_with_threads(max_threads = 2, verbose = TRUE)
jlmerclusterperm:::set_projenv(verbose = TRUE)
cat(JuliaConnectoR::juliaCall("Pkg.status"))
cat(readLines(file.path(jlmerclusterperm:::.jlmerclusterperm$opts$projdir, "Manifest.toml")), sep = "\n")
