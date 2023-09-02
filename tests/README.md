Tests and Coverage
================
02 September, 2023 09:24:44

- [Coverage](#coverage)
- [Unit Tests](#unit-tests)

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

| Object                                                                | Coverage (%) |
|:----------------------------------------------------------------------|:------------:|
| jlmerclusterperm                                                      |    92.30     |
| [R/utils.R](../R/utils.R)                                             |    78.57     |
| [R/permute_timewise_statistics.R](../R/permute_timewise_statistics.R) |    81.01     |
| [R/compute_timewise_statistics.R](../R/compute_timewise_statistics.R) |    88.64     |
| [R/aaa.R](../R/aaa.R)                                                 |    89.23     |
| [R/threshold_search.R](../R/threshold_search.R)                       |    89.47     |
| [R/jlmer_spec.R](../R/jlmer_spec.R)                                   |    91.19     |
| [R/clusters_methods.R](../R/clusters_methods.R)                       |    93.83     |
| [R/calculate_pvalue.R](../R/calculate_pvalue.R)                       |    94.44     |
| [R/permute.R](../R/permute.R)                                         |    95.24     |
| [R/clusterpermute.R](../R/clusterpermute.R)                           |    95.45     |
| [R/interop-utils-unexported.R](../R/interop-utils-unexported.R)       |    96.00     |
| [R/tidy.R](../R/tidy.R)                                               |    97.14     |
| [R/extract_clusters.R](../R/extract_clusters.R)                       |    100.00    |
| [R/interop-utils.R](../R/interop-utils.R)                             |    100.00    |
| [R/jlmer.R](../R/jlmer.R)                                             |    100.00    |
| [R/julia_rng.R](../R/julia_rng.R)                                     |    100.00    |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat) package.

| file                                                              |   n |  time | error | failed | skipped | warning |
|:------------------------------------------------------------------|----:|------:|------:|-------:|--------:|--------:|
| [test-aaa.R](testthat/test-aaa.R)                                 |   4 | 45.21 |     0 |      0 |       0 |       0 |
| [test-clusterpermute.R](testthat/test-clusterpermute.R)           |  10 | 21.30 |     0 |      0 |       0 |       0 |
| [test-jlmer.R](testthat/test-jlmer.R)                             |   6 |  2.27 |     0 |      0 |       0 |       0 |
| [test-jlmer_spec.R](testthat/test-jlmer_spec.R)                   |   5 |  0.09 |     0 |      0 |       0 |       0 |
| [test-julia_rng.R](testthat/test-julia_rng.R)                     |   9 |  0.12 |     0 |      0 |       0 |       0 |
| [test-permute.R](testthat/test-permute.R)                         |   7 |  1.38 |     0 |      0 |       0 |       0 |
| [test-progress.R](testthat/test-progress.R)                       |   5 |  0.06 |     0 |      0 |       0 |       0 |
| [test-singularity.R](testthat/test-singularity.R)                 |   3 |  3.72 |     0 |      0 |       0 |       0 |
| [test-threshold_search.R](testthat/test-threshold_search.R)       |   1 |  0.25 |     0 |      0 |       0 |       0 |
| [test-timewise_statistics.R](testthat/test-timewise_statistics.R) |   9 |  0.08 |     0 |      0 |       0 |       0 |

<details closed>
<summary>
Show Detailed Test Results
</summary>

| file                                                                  | context             | test                                                      | status |   n |  time |
|:----------------------------------------------------------------------|:--------------------|:----------------------------------------------------------|:-------|----:|------:|
| [test-aaa.R](testthat/test-aaa.R#L11)                                 | aaa                 | Setup with seed works (use 2 for testing)                 | PASS   |   2 | 45.18 |
| [test-aaa.R](testthat/test-aaa.R#L27)                                 | aaa                 | RNG initializes to seed=1 counter=0                       | PASS   |   2 |  0.03 |
| [test-clusterpermute.R](testthat/test-clusterpermute.R#L18)           | clusterpermute      | CPAs under the same RNG state are identical               | PASS   |   1 | 17.73 |
| [test-clusterpermute.R](testthat/test-clusterpermute.R#L33)           | clusterpermute      | Piecemeal and wholesale CPAs are identical                | PASS   |   4 |  0.03 |
| [test-clusterpermute.R](testthat/test-clusterpermute.R#L40)           | clusterpermute      | Errors on incompatible clusters                           | PASS   |   3 |  2.19 |
| [test-clusterpermute.R](testthat/test-clusterpermute.R#L50)           | clusterpermute      | Errors on no predictors                                   | PASS   |   2 |  1.35 |
| [test-jlmer.R](testthat/test-jlmer.R#L14)                             | jlmer               | direct and indirect fits identical                        | PASS   |   2 |  1.11 |
| [test-jlmer.R](testthat/test-jlmer.R#L19)                             | jlmer               | returns julia object                                      | PASS   |   2 |  0.00 |
| [test-jlmer.R](testthat/test-jlmer.R#L28)                             | jlmer               | direct and indirect fits identical - mixed                | PASS   |   2 |  1.16 |
| [test-jlmer_spec.R](testthat/test-jlmer_spec.R#L2)                    | jlmer_spec          | Minimal specification without grouping structures allowed | PASS   |   2 |  0.03 |
| [test-jlmer_spec.R](testthat/test-jlmer_spec.R#L7)                    | jlmer_spec          | Inform misspecifications in grouping structure            | PASS   |   2 |  0.03 |
| [test-jlmer_spec.R](testthat/test-jlmer_spec.R#L12)                   | jlmer_spec          | Warn uneven time sampling rate                            | PASS   |   1 |  0.03 |
| [test-julia_rng.R](testthat/test-julia_rng.R#L6)                      | julia_rng           | RNG counter setter/getter                                 | PASS   |   2 |  0.06 |
| [test-julia_rng.R](testthat/test-julia_rng.R#L11)                     | julia_rng           | RNG seed setter/getter                                    | PASS   |   2 |  0.01 |
| [test-julia_rng.R](testthat/test-julia_rng.R#L17)                     | julia_rng           | RNG restore                                               | PASS   |   3 |  0.02 |
| [test-julia_rng.R](testthat/test-julia_rng.R#L23)                     | julia_rng           | RNG generate random seed                                  | PASS   |   2 |  0.03 |
| [test-permute.R](testthat/test-permute.R#L20)                         | permute             | preserves participant structure                           | PASS   |   1 |  0.89 |
| [test-permute.R](testthat/test-permute.R#L27)                         | permute             | preserves temporal structure                              | PASS   |   1 |  0.05 |
| [test-permute.R](testthat/test-permute.R#L32)                         | permute             | guesses type                                              | PASS   |   1 |  0.10 |
| [test-permute.R](testthat/test-permute.R#L36)                         | permute             | increments counter                                        | PASS   |   1 |  0.02 |
| [test-permute.R](testthat/test-permute.R#L44)                         | permute             | shuffling reproducibility                                 | PASS   |   1 |  0.08 |
| [test-permute.R](testthat/test-permute.R#L54)                         | permute             | levels of a category shuffled together                    | PASS   |   2 |  0.24 |
| [test-progress.R](testthat/test-progress.R#L6)                        | progress            | No side effects when called empty                         | PASS   |   1 |  0.02 |
| [test-progress.R](testthat/test-progress.R#L13)                       | progress            | Side effect by show, width, or both                       | PASS   |   3 |  0.03 |
| [test-progress.R](testthat/test-progress.R#L22)                       | progress            | Restore startup option                                    | PASS   |   1 |  0.01 |
| [test-singularity.R](testthat/test-singularity.R#L12_L15)             | singularity         | Informs on singularity                                    | PASS   |   3 |  3.72 |
| [test-threshold_search.R](testthat/test-threshold_search.R#L15)       | threshold_search    | tests all steps                                           | PASS   |   1 |  0.25 |
| [test-timewise_statistics.R](testthat/test-timewise_statistics.R#L14) | timewise_statistics | chisq bound by 0-1                                        | PASS   |   1 |  0.03 |
| [test-timewise_statistics.R](testthat/test-timewise_statistics.R#L18) | timewise_statistics | dims of empirical stats                                   | PASS   |   2 |  0.02 |
| [test-timewise_statistics.R](testthat/test-timewise_statistics.R#L23) | timewise_statistics | tidy dims of empirical stats                              | PASS   |   2 |  0.00 |
| [test-timewise_statistics.R](testthat/test-timewise_statistics.R#L31) | timewise_statistics | dims of null stats                                        | PASS   |   2 |  0.01 |
| [test-timewise_statistics.R](testthat/test-timewise_statistics.R#L36) | timewise_statistics | tidy dims of null stats                                   | PASS   |   2 |  0.02 |

</details>
<details>
<summary>
Session Info
</summary>

| Field    | Value                             |
|:---------|:----------------------------------|
| Version  | R version 4.3.0 (2023-04-21 ucrt) |
| Platform | x86_64-w64-mingw32/x64 (64-bit)   |
| Running  | Windows 11 x64 (build 22621)      |
| Language | English_United States             |
| Timezone | America/New_York                  |

| Package  | Version |
|:---------|:--------|
| testthat | 3.1.10  |
| covr     | 3.6.2   |
| covrpage | 0.2     |

</details>
<!--- Final Status : pass --->
