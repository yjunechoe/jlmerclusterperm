function _compute_largest_clusters(t_vec, binned, n)
  runs = rle(sign.(t_vec))
  run_inds = vcat(0, cumsum(runs[2]))
  clusters = (:).(run_inds[1:end-1].+1, run_inds[2:end])
  run_groups = getindex.(Ref(t_vec), clusters)
  sum_t = (sum.(x -> isinf(x) ? 0 : x, run_groups))
  cluster_ranges = extrema.(clusters)
  clusters_df = DataFrame(cluster_ranges)
  rename!(clusters_df, :1 => :start, :2 => :end)
  clusters_df.statistic = sum_t
  filter!(:statistic => !≈(0), clusters_df)
  transform!(clusters_df, [:end, :start] => (-) => :length, eachindex => :cluster_id)
  select!(clusters_df, [5,1,2,3,4])
  sort!(clusters_df, :statistic, rev = true)
  first(clusters_df, n)
end

function compute_largest_clusters(t_matrix, binned, n)
  out = Vector{DataFrame}(undef, size(t_matrix, 1))
  Threads.@threads for i = 1:length(out)
    out[i] = _compute_largest_clusters(t_matrix[i,:], binned, n)
  end
  out
end
