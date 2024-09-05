skip_conditionally()

JuliaConnectoR::stopJulia()
julia_cmd <- asNamespace("JuliaConnectoR")$getJuliaExecutablePath()
system2(julia_cmd, '-e "using InteractiveUtils; println(versioninfo());"')

jlmerclusterperm:::start_with_threads(, max_threads = 2, verbose = TRUE)
jlmerclusterperm:::set_projenv(cache_dir = tempdir(), verbose = TRUE)
cat(JuliaConnectoR::juliaCall("Pkg.status"))
cat(readLines(file.path(jlmerclusterperm:::.jlmerclusterperm$opts$projdir, "Manifest.toml")), sep = "\n")
