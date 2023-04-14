function _extract_clusters(t_vec, binned, n, id)
    runs = rle(sign.(t_vec))
    run_inds = vcat(0, cumsum(runs[2]))
    clusters = (:).(run_inds[1:end-1] .+ 1, run_inds[2:end])
    run_groups = getindex.(Ref(t_vec), clusters)
    sum_t = (sum.(x -> isinf(x) ? 0 : x, run_groups))
    cluster_ranges = extrema.(clusters)
    clusters_df = DataFrame(cluster_ranges)
    rename!(clusters_df, :1 => :cluster_start, :2 => :cluster_end)
    clusters_df.statistic = sum_t
    filter!(:statistic => !≈(0), clusters_df)
    transform!(clusters_df, :statistic => ByRow(abs) => :abs_stat)
    if !binned
        filter!([:cluster_end, :cluster_start] => !=, clusters_df)
    end
    transform!(clusters_df, eachindex => :cluster_id)
    sort!(clusters_df, :abs_stat, rev = true)
    select!(clusters_df, [:cluster_id, :cluster_start, :cluster_end, :statistic])
    if nrow(clusters_df) == 0
        out = DataFrame(
            cluster_start = 0,
            cluster_end = 0,
            statistic = 0,
            cluster_id = 0,
            id = id,
        )
    else
        out = first(clusters_df, n)
        out.id .= id
    end
    out
end

function extract_clusters(t_matrix, binned, n)
    out = Vector{DataFrame}(undef, size(t_matrix, 1))
    for i = 1:length(out)
        out[i] = _extract_clusters(t_matrix[i, :], binned, n, i)
    end
    vcat(out...)
end
