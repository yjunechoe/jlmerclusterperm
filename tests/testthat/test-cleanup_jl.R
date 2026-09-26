test_that("cleanup_jl() removes everything except the cached Manifest.toml", {
  projdir <- tempfile()
  on.exit(unlink(projdir, recursive = TRUE))
  # "manifest.tom" only contains characters from "Manifest.toml", which fooled the old regex
  dir.create(file.path(projdir, "JlmerClusterPerm", "src"), recursive = TRUE)
  file.create(file.path(projdir, c("Manifest.toml", "Project.toml", "load-pkgs.jl", "manifest.tom")))
  cleanup_jl(projdir)
  expect_equal(dir(projdir), "Manifest.toml")
})
