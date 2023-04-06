function compute_timewise_statistics(formula, data, time, family, contrasts, statistic, is_mem; opts...)

  response_var = formula.lhs.sym
  times = sort(unique(data[!,time]))
  n_times = length(times)

  if is_mem

    fm_schema = MixedModels.schema(formula, data, contrasts)
    form = MixedModels.apply_schema(formula, fm_schema, MixedModel)
    re_term = [isa(x, MixedModels.AbstractReTerm) for x in form.rhs]
    fixed = String.(Symbol.(form.rhs[.!re_term][1].terms))
    grouping_vars = [String(Symbol(x.rhs)) for x in form.rhs[re_term]]

    timewise_lme(formula, data, time, family, contrasts, response_var, fixed, grouping_vars, times, n_times, true; opts...)

  else

    fm_schema = StatsModels.schema(formula, data)
    form = StatsModels.apply_schema(formula, fm_schema)
    fixed = String.(Symbol.(form.rhs.terms))

    t_matrix = timewise_lm(formula, data, time, family, response_var, fixed, times, n_times)
    (t_matrix = t_matrix, Predictors = fixed, Time = times)

  end

end

function timewise_lme(formula, data, time, family, contrasts, statistic,
                        response_var, fixed, grouping_vars, times, n_times, diagnose;
                        opts...)

  t_matrix = zeros(length(fixed), n_times)
  if diagnose
    singular_fits = zeros(n_times)
    convergence_failures = zeros(Bool, n_times)
    rePCA_95_matrix = zeros(length(grouping_vars), n_times)
  end

  if diagnose
    p = Progress(n_times)
  end

  @suppress begin
    Threads.@threads for i = 1:n_times

      data_at_time = filter(time => ==(times[i]), data)
      response = data_at_time[!, response_var]

      if all(==(response[1]), response)
        t_matrix[:,i] .= response[1] == 1 ? Inf : -Inf
        if diagnose
          convergence_failures[i] = missing
          singular_fits[i] = missing
          rePCA_95_matrix[:,i] .= missing
        end
      else
        try
          time_mod = fit(MixedModel, formula, data_at_time, family; contrasts = contrasts, opts...)

          if statistic == "chisq"
          elseif statistic == "t"
            t_matrix[:,i] = z_value(time_mod)
          end

          if diagnose
            singular_fits[i] = issingular(time_mod)
            rePCA_95_matrix[:,i] = [all(isnan, x) ? 0 : findfirst(>(.95), x) for x in time_mod.rePCA]
          end
        catch e
          convergence_failures[i] = true
          t_matrix[:,i] .= NaN
        end
      end

      if diagnose
        next!(p)
      end
    end
  end

  if diagnose
    (
      singular_fits = singular_fits, convergence_failures = convergence_failures,
      t_matrix = t_matrix, Predictors = fixed, Time = times,
      rePCA_95_matrix = rePCA_95_matrix, Grouping = grouping_vars
    )
  else
    t_matrix
  end

end

function timewise_lm(formula, data, time, family, statistic, response_var, fixed, times, n_times)
  t_matrix = zeros(length(fixed), n_times)
  Threads.@threads for i = 1:n_times
    data_at_time = filter(time => ==(times[i]), data)
    response = data_at_time[!, response_var]
    if all(==(response[1]), response)
      constant = response[1] == 1 ? Inf : -Inf
      t_matrix[:,i] .= constant
    else
      time_mod = try
        glm(formula, data_at_time, family)
      catch e
        t_matrix[:,i] .= NaN
        continue
      end
      t_matrix[:,i] = z_value(time_mod)
    end
  end
  t_matrix
end
