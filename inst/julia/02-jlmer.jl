function jlmer(formula, data, family, contrasts; opts...)
  fit(MixedModel, formula, data, family; contrasts = contrasts, opts...)
end

function _jlmer_by_time(formula, data, time, family, contrasts,
                        response_var, fixed, grouping_vars, times, n_times, diagnose;
                        opts...)

  z_matrix = zeros(length(fixed), n_times)
  if diagnose
    singular_fits = zeros(n_times)
    rePCA_95_matrix = zeros(length(grouping_vars), n_times)
  end

  if diagnose
    p = Progress(n_times)
  end

  Threads.@threads for i = 1:n_times
  # for i in 1:n_times

    data_at_time = filter(time => ==(times[i]), data)
    response = data_at_time[!, response_var]

    if all(==(response[1]), response)
      constant = response[1] == 1 ? Inf : -Inf
      z_matrix[:,i] .= constant
      if diagnose
        singular_fits[i] = constant
        rePCA_95_matrix[:,i] .= constant
      end
    else
      time_mod = try
        fit(MixedModel, formula, data_at_time, family; contrasts = contrasts, opts...)
      catch e
        z_matrix[:,i] .= NaN
        continue
      end
      z_matrix[:,i] = z_value(time_mod)
      if diagnose
        singular_fits[i] = issingular(time_mod)
        rePCA_95_matrix[:,i] = [all(isnan, x) ? 0 : findfirst(>(.95), x) for x in time_mod.rePCA]
      end
    end

    if diagnose
      next!(p)
    end
  end

  if diagnose
    (
      singular_fits = singular_fits,
      z_matrix = z_matrix, Predictors = fixed, Time = times,
      rePCA_95_matrix = rePCA_95_matrix, Grouping = grouping_vars
    )
  else
    z_matrix
  end

end

function jlmer_by_time(formula, data, time, family, contrasts; opts...)

  response_var = formula.lhs.sym
  fm_schema = MixedModels.schema(formula, data, contrasts)
  form = MixedModels.apply_schema(formula, fm_schema, MixedModel)
  re_term = [isa(x, MixedModels.AbstractReTerm) for x in form.rhs]
  fixed = String.(Symbol.(form.rhs[.!re_term][1].terms))
  grouping_vars = [String(Symbol(x.rhs)) for x in form.rhs[re_term]]

  times = sort(unique(data[!,time]))
  n_times = length(times)

  _jlmer_by_time(formula, data, time, family, contrasts, response_var, fixed, grouping_vars, times, n_times, true; opts...)

end
