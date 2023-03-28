function clusterpermute(formula, data, time, family, contrasts, nsim, participant_col, trial_col, term_groups, predictors_subset, is_mem; opts...)

  response_var = formula.lhs.sym
  times = sort(unique(data[!,time]))
  n_times = length(times)

  predictors_exclude = ["(Intercept)"]
  if length(predictors_subset) > 0
    term_groups_est = filter(grp -> any(in(predictors_subset), grp.p), term_groups)
  else
    term_groups_est = filter(grp -> !all(in(predictors_exclude), grp.p), term_groups)
  end

  nsims = nsim * length(term_groups_est)
  pg = Progress(nsims)

  if is_mem
    fm_schema = MixedModels.schema(formula, data, contrasts)
    form = MixedModels.apply_schema(formula, fm_schema, MixedModel)
    re_term = [isa(x, MixedModels.AbstractReTerm) for x in form.rhs]
    fixed = String.(Symbol.(form.rhs[.!re_term][1].terms))
    grouping_vars = [String(Symbol(x.rhs)) for x in form.rhs[re_term]]
  else
    fm_schema = StatsModels.schema(formula, data)
    form = StatsModels.apply_schema(formula, fm_schema)
    fixed = String.(Symbol.(form.rhs.terms))
  end

  n_fixed = length(fixed)
  res = zeros(nsim, n_times, n_fixed)

  for term_groups in term_groups_est
    predictors = term_groups.p
    permute_data = copy(data)
    shuffle_type = guess_shuffle_as(permute_data, predictors, participant_col, trial_col == "" ? missing : 3)
    for i in 1:nsim
      shuffle_as!(permute_data, shuffle_type, predictors, participant_col, trial_col)
      if is_mem
        zs = _jlmer_by_time(formula, permute_data, time, family,
                            contrasts, response_var, fixed, grouping_vars, times, n_times, false; opts...)
      else
        zs = _jlm_by_time(formula, permute_data, time, family,
                          response_var, fixed, times, n_times)
      end
      for term_ind in term_groups.i
        res[i,:,term_ind] = zs[term_ind,:]
      end
      next!(pg)
    end
  end


  predictors = vcat(map(terms -> terms.p, term_groups_est)...)
  res = res[:, :, vcat(map(terms -> terms.i, term_groups_est)...)]

  (z_array = res, predictors = predictors)

end

function guess_shuffle_as(df, predictor_cols, participant_col, trial_col)
  if (ismissing(trial_col))
    return "between_participant"
  else
    subj_pred_pair = unique(df[!,vcat(participant_col, predictor_cols)])
    between_participant = length(unique(df[!, participant_col])) == nrow(subj_pred_pair)
    return return between_participant ? "between_participant" : "within_participant"
  end
end

function shuffle_as!(df, shuffle_type, predictor_cols, participant_col, trial_col)
  if shuffle_type == "between_participant"
    subj_pred_pair = unique(df[!, vcat(participant_col, predictor_cols)])
    shuffle!(subj_pred_pair[!, participant_col])
    select!(df, Not(predictor_cols))
    leftjoin!(df, subj_pred_pair, on = participant_col)
  elseif shuffle_type == "within_participant"
    trial_pred_pair = unique(df[!, vcat(participant_col, trial_col, predictor_cols)])
    combine(groupby(trial_pred_pair, participant_col), sdf -> shuffle!(sdf[!, trial_col]))
    select!(df, Not(predictor_cols))
    leftjoin!(df, trial_pred_pair, on = [participant_col, trial_col])
  end
end

function guess_and_shuffle_as!(df, predictor_cols, participant_col, trial_col)
  shuffle_type = guess_shuffle_as(df, predictor_cols, participant_col, trial_col)
  shuffle_as!(df, predictor_cols, participant_col, trial_col, shuffle_type)
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
