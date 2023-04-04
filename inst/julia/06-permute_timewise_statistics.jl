function permute_timewise_statistics(formula, data, time, family, contrasts, nsim, participant_col, trial_col, term_groups, predictors_subset, is_mem; opts...)

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
  counter_states = zeros(nsims)
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
      counter_states[i] = get_rng_counter()
      shuffle_as!(permute_data, shuffle_type, predictors, participant_col, trial_col)
      if is_mem
        zs = timewise_lme(formula, permute_data, time, family,
                            contrasts, response_var, fixed, grouping_vars, times, n_times, false; opts...)
      else
        zs = timewise_lm(formula, permute_data, time, family,
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

  @info counter_states
  (z_array = res, predictors = predictors, counter_states = counter_states)

end
