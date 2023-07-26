module JlmerClusterPerm

using Suppressor
using Random
using Random123
using StatsBase
using StatsModels
using Distributions
using GLM
using MixedModels
using DataFrames
using ProgressMeter

export jlmer
export compute_timewise_statistics
export extract_clusters
export permute_by_predictor
export permute_timewise_statistics

include("01-utils.jl")
include("02-jlmer.jl")
include("03-compute_timewise_statistics.jl")
include("04-extract_clusters.jl")
include("05-permute.jl")
include("06-permute_timewise_statistics.jl")

end # module JlmerClusterPerm
