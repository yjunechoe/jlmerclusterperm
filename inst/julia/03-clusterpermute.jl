function clusterpermute(formula, data, time, family, contrasts, nsim, threshold, participant_col, binned; opts...)

  response_var = formula.lhs.sym
  fm_schema = MixedModels.schema(formula, data, contrasts)
  form = MixedModels.apply_schema(formula, fm_schema, MixedModel)
  re_term = [isa(x, MixedModels.AbstractReTerm) for x in form.rhs]
  fixed = String.(Symbol.(form.rhs[.!re_term][1].terms))
  grouping_vars = [String(Symbol(x.rhs)) for x in form.rhs[re_term]]
  predictors = Symbol.(filter(!=("1"), fixed))

  times = sort(unique(data[!,time]))
  n_times = length(times)

  n_fixed = length(fixed)

  res = zeros(nsim, n_times, n_fixed)

  for p in 1:n_fixed
    if fixed[p] != "1"
      permute_data = copy(data)
      @showprogress for i in 1:nsim
        guess_and_permute(permute_data, fixed[p], participant_col, trial_col)
        zs = _jlmer_by_time(formula, permute_data, time, family, contrasts, response_var, fixed, grouping_vars, times, n_times, false; opts...)
        res[i,:,p] = zs[p,:]
      end
    end
  end

  res = res[:, :, findall(!=("1"), fixed)]
  replace!(x -> (<(threshold) ∘ abs)(x) ? 0 : x, res)
  time_is_point = (family isa Bernoulli) || !binned

  converges = map(p -> all.(!isnan, eachrow(res[:,:,p])), 1:length(predictors))

  out = map(p -> find_largest_cluster.(eachrow(res[converges[p], :, p]), Ref(true)), 1:length(predictors))
  NamedTuple{Tuple(predictors)}(map(x -> Float64[y.sum_z for y in x], out))

end

function guess_and_permute(df, predictor_col, participant_col, trial_col)
  subj_pred_pair = unique(df[!,[participant_col, predictor_col]])
  between_participant = length(unique(df[!, participant_col])) == nrow(subj_pred_pair)
  if between_participant
    shuffle!(subj_pred_pair[!,predictor_col])
    select!(df, Not(predictor_col))
    leftjoin!(df, subj_pred_pair, on = participant_col)
  else
    trial_pred_pair = unique(df[!, [participant_col, trial_col, predictor_col]])
    transform!(groupby(trial_pred_pair, participant_col), predictor_col => shuffle!, renamecols = false)
    select!(df, Not(predictor_col))
    leftjoin!(df, trial_pred_pair, on = [participant_col, trial_col])
  end
end

function find_largest_cluster(zs, time_is_point)
  runs = rle(sign.(zs))
  run_inds = vcat(0, cumsum(runs[2]))
  run_groups = getindex.(Ref(zs), (:).(run_inds[1:end-1].+1, run_inds[2:end]))
  sum_z = (sum.(x -> isinf(x) ? 0 : x, run_groups))
  longest_run = findmax(abs, sum_z)
  largest_cluster = sum_z[longest_run[2]]
  longest_run_window = run_inds[longest_run[2]] .+ collect(1:runs[2][longest_run[2]])
  if largest_cluster == 0 || (time_is_point && length(longest_run_window) == 1)
    return (cluster = Missing, sum_z = 0)
  end
  NamedTuple{(:cluster, :sum_z)}([longest_run_window, largest_cluster])
end
