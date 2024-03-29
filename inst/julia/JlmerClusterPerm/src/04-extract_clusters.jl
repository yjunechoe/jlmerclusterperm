"""
    extract_clusters(t_matrix::Matrix{<:AbstractFloat}, binned::Bool, n::Integer)

Extract clusters from a predictor-by-time matrix of test statistics.

!!! note
    Called from R function `jlmerclusterperm::extract_empirical_clusters()`
    and `jlmerclusterperm::extract_null_cluster_dists()`
"""
function extract_clusters(t_matrix::Matrix{<:AbstractFloat}, binned::Bool, n::Integer)
    out = Vector{DataFrame}(undef, size(t_matrix, 1))
    for i in 1:length(out)
        out[i] = _extract_clusters(t_matrix[i, :], binned, n, i)
    end
    return vcat(out...)
end

function _extract_clusters(
    t_vec::Vector{<:AbstractFloat},
    binned::Bool,
    n::Integer,
    id::Integer
)
    runs = rle(sign.(t_vec))
    run_inds = vcat(0, cumsum(runs[2]))
    clusters = (:).(run_inds[1:(end - 1)] .+ 1, run_inds[2:end])
    run_groups = getindex.(Ref(t_vec), clusters)
    sum_t = (sum.(x -> isinf(x) ? 0 : x, run_groups))
    cluster_ranges = extrema.(clusters)
    clusters_df = DataFrame(cluster_ranges)
    rename!(clusters_df, :1 => :cluster_start, :2 => :cluster_end)
    clusters_df.statistic = sum_t
    filter!(:statistic => !≈(0), clusters_df)
    clusters_df.abs_stat = abs.(clusters_df.statistic)
    if !binned
        filter!([:cluster_end, :cluster_start] => !=, clusters_df)
    end
    clusters_df.cluster_id = 1:nrow(clusters_df)
    sort!(clusters_df, :abs_stat; rev=true)
    select!(clusters_df, [:cluster_id, :cluster_start, :cluster_end, :statistic])
    if nrow(clusters_df) == 0
        out = DataFrame(;
            cluster_start=0,
            cluster_end=0,
            statistic=0,
            cluster_id=0,
            id=id,
        )
    else
        out = first(clusters_df, n)
        out.id .= id
    end
    return out
end
