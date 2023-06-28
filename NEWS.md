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
