# jlmerclusterperm (development version)

* Better detection of Julia executable

* `clusterpermute()` is faster because the permuted timewise statistics now stay in Julia, and only the null cluster-mass distribution is returned to R. Results are identical to the piecemeal workflow.

* Faster permutation of predictors (up to ~2x faster `clusterpermute()` for GLMs). Results are identical to previous versions under the same RNG state.

* `permute_by_predictor()` and `permute_timewise_statistics()` give an informative error when a predictor is not constant within the units being shuffled (participants, or trials within participants).

* Fixed the `predictors` argument of `permute_timewise_statistics()` and `clusterpermute()`, which previously always errored.

* `extract_null_cluster_dists()` no longer drops simulations whose statistics mix `Inf` and `-Inf`.

* Cluster data frames now have a consistent column order (`cluster_id`, `cluster_start`, `cluster_end`, `statistic`) regardless of whether the first predictor or simulation contains a cluster.

* Fixed crash in mixed-effects models when the response is constant at a time point.

* Fixed cluster-mass statistics being returned as a list when a cluster spans a time point with a constant response.

# jlmerclusterperm 1.1.4

- Imports `{JuliaFormulae}` package for parsing R formula into Julia formula

# jlmerclusterperm 1.1.3

No user-visible changes

Fixes for CRAN:

- Stricter specification of package versions in `Project.toml` to avoid version conflicts.

# jlmerclusterperm 1.1.2

- `jlmerclusterperm()` exposes a `cache_dir` argument for manually specifying the cache directory. This was added largely for the convenience of testing. The default value of `cache_dir = NULL` preserves old behavior.

Fixes for CRAN:

- Write cache to `tempdir()` for the purposes of running examples and tests.

# jlmerclusterperm 1.1.1

- More informative warnings when constructing a spec object

- Fixed minor bugs when interfacing with Julia

- Fixed bug with reporting convergence warnings when only one is encountered

# jlmerclusterperm 1.1.0

Minor breaking change:

- Slightly loosened the default convergence criterion for `permute_timewise_statistics()`

Fixes for CRAN:

- Specified importing `R_user_dir()` from `{backports}` which was missing previously

# jlmerclusterperm 1.0.6

- Fixed bug where `add1` argument to `walk_threshold_steps()` was not being passed down properly

- R < 4.0 falls back to using `{backports}` for `R_user_dir()`

Fixes for CRAN:

- Downgrade DataFrames.jl dependency to 1.3 to avoid a mysterious pre-compilation failure on CRAN windows checks

# jlmerclusterperm 1.0.5

No user-visible changes

Fixes for CRAN:

- Ensure minimum version requirements for Tables.jl and DataAPI.jl are met

# jlmerclusterperm 1.0.4

No user-visible changes

- Restructured internal Julia code into a module (`JlmerClusterPerm.jl`)

- Upon activating project, Manifest.toml is cached to `tools::R_user_dir()` to speed up pre-compilation.

# jlmerclusterperm 1.0.3

Fixes for CRAN:

- Use string for package version comparison

# jlmerclusterperm 1.0.2

### Bug fixes

- `jlmerclusterperm_setup()` now works for rc and alpha builds of Julia

Fixes for CRAN:

- The exclusion of Manifest.toml is now specified in .Rbuildignore - only Project.toml is bundled as intended.

# jlmerclusterperm 1.0.1

`jlmerclusterperm_setup()` now exits early if Julia version requirement (>=1.8) is not met.

Fixes for CRAN:

- Check Julia version requirement before proceeding to examples and tests

# jlmerclusterperm 1.0.0

### Breaking changes

- The `threshold_steps` argument of `walk_threshold_steps()` is renamed to `steps`.

### New features

- New functions to interface with Julia RNG seed: `get_rng_seed()` and `set_rng_seed()`

### Other improvements

- `jlmerclusterperm_setup()` now echos `Pkg.instantiate()` to print precompilation information upon the first setup call

# jlmerclusterperm 0.2.0

Added vignettes. Significant usability improvements

# jlmerclusterperm 0.1.0

Initial release
