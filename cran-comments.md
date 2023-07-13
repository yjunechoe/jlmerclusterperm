* Small patch to fix a failure to parse the Julia version when it's in rc or alpha.

* The exclusion of Manifest.toml is now specified in .Rbuildignore - only Project.toml is bundled as intended. Users will generate their own Manifest.toml as part of activating the project. This resolves the errors from a prior submission where the accidental inclusion of my own Manifest.toml triggered a stricter criteria for resolving the versions of (downstream) dependencies.

Apologies for frequency of resubmission.

## R CMD check results

0 errors | 0 warnings | 0 notes
