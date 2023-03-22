using StatsBase
using Statistics
using MixedModels
using DataFrames
using ProgressMeter
using Random
if contains(first(Sys.cpu_info()).model, "Intel")
  using MKL
end
