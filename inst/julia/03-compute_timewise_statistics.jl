function compute_timewise_statistics(formula, data, time, family, contrasts, term_groups, statistic, is_mem; opts...)

  response_var = formula.lhs.sym
  times = sort(unique(data[!,time]))
  n_times = length(times)

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

  if statistic == "chisq"
    drop_terms = filter(x -> x.p != ["(Intercept)"], term_groups)
    reduced_formula = [reduce_formula(Symbol.(x.p), form, is_mem) for x in drop_terms]
    test_opts = (reduced_formula = reduced_formula,)
  elseif statistic == "t"
    test_opts = Nothing
  end

  if is_mem
    timewise_lme(formula, data, time, family, contrasts, statistic, test_opts,
                 response_var, fixed, grouping_vars, times, n_times, true; opts...)
  else
    t_matrix = timewise_lm(formula, data, time, family, statistic, test_opts,
                           response_var, fixed, times, n_times)
    (t_matrix = t_matrix, Predictor = fixed, Time = times)
  end

end

function timewise_lme(formula, data, time, family, contrasts, statistic, test_opts,
                      response_var, fixed, grouping_vars, times, n_times, diagnose;
                      opts...)

  if statistic == "t"
    t_matrix = zeros(length(fixed), n_times)
  elseif statistic == "chisq"
    drop_formula = test_opts.reduced_formula
    if drop_formula isa Vector
      t_matrix = zeros(length(drop_formula), n_times)
    else
      t_matrix = zeros(length(fixed), n_times)
    end
  end

  if diagnose
    singular_fits = zeros(n_times)
    convergence_failures = zeros(Bool, n_times)
    rePCA_95_matrix = zeros(length(grouping_vars), n_times)
  end

  if diagnose
    pg = Progress(n_times, output = pg_io, barlen = pg_width, showspeed = true)
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

          # test statistic
          if statistic == "chisq"
            if drop_formula isa Vector
              drop1_mods = map(fm -> fit(MixedModel, fm, data_at_time, family; contrasts = contrasts, opts...), drop_formula)
              chisq_vals = map(x -> MixedModels.likelihoodratiotest(time_mod, x).tests.deviancediff[1], drop1_mods)
              if size(t_matrix)[1] > length(chisq_vals)
                chisq_vals = [0, chisq_vals...]
              else
                chisq_vals = [chisq_vals...]
              end
              t_matrix[:,i] = chisq_vals # make signed?
            else
              lrtest_obj = MixedModels.likelihoodratiotest(time_mod, fit(MixedModel, drop_formula, data_at_time, family; contrasts = contrasts, opts...))
              t_matrix[:,i] .= lrtest_obj.tests.deviancediff[1] # make signed?
            end
          elseif statistic == "t"
            t_matrix[:,i] = t_value(time_mod)
          end

          if diagnose
            singular_fits[i] = issingular(time_mod)
            rePCA_95_matrix[:,i] = [all(isnan, x) ? 0 : findfirst(>(.95), x) for x in time_mod.rePCA]
          end
        catch e
          if diagnose
            convergence_failures[i] = true
          end
          t_matrix[:,i] .= NaN
        end
      end

      if diagnose
        next!(pg)
      end
    end
  end

  if diagnose
    (
      singular_fits = singular_fits, convergence_failures = convergence_failures,
      t_matrix = t_matrix, Predictor = fixed, Time = times,
      rePCA_95_matrix = rePCA_95_matrix, Grouping = grouping_vars
    )
  else
    t_matrix
  end

end

function timewise_lm(formula, data, time, family, statistic, test_opts,
                     response_var, fixed, times, n_times)

  if statistic == "t"
    t_matrix = zeros(length(fixed), n_times)
  elseif statistic == "chisq"
    drop_formula = test_opts.reduced_formula
    if drop_formula isa Vector
      t_matrix = zeros(length(drop_formula), n_times)
    end
  end

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

      # test statistic
      if statistic == "chisq"
        if drop_formula isa Vector
          drop1_mods = map(fm -> glm(fm, data_at_time, family), drop_formula)
          chisq_vals = map(x -> chisq_value(lrtest(time_mod, x)), drop1_mods)
          if size(t_matrix)[1] > length(chisq_vals)
            chisq_vals = [0, chisq_vals...]
          else
            chisq_vals = [chisq_vals...]
          end
          t_matrix[:,i] = chisq_vals # make signed?
        else
          lrtest_obj = lrtest(time_mod, glm(test_opts.reduced_formula, data_at_time, family))
          t_matrix[:,i] .= chisq_value(lrtest_obj) # make signed?
        end
      elseif statistic == "t"
        t_matrix[:,i] = t_value(time_mod)
      end

    end
  end
  t_matrix
end
