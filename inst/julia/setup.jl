using Statistics
using MixedModels
using DataFrames
using ProgressMeter

function jlmer(formula, data, family, contrasts; opts...)
  fit(MixedModel, formula, data, family; contrasts = contrasts, opts...)
end

function z_value(mod)
  mod.β ./ stderror(mod)
end

function jlmer_by_time(formula, data, time, family, contrasts; opts...)

  response_var = formula.lhs.sym
  times = sort(unique(data[!,time]))
  n_times = length(times)

  mod1 = fit(MixedModel, formula, filter(time => ==(times[5]), data), family; contrasts = contrasts, opts...)
  fixed = mod1.feterm.cnames
  grouping_vars = keys(VarCorr(mod1).σρ)

  z_matrix = zeros(length(fixed), n_times)
  singular_fits = zeros(n_times)
  rePCA_95_matrix = zeros(length(grouping_vars), n_times)

  p = Progress(n_times)
  Threads.@threads for i = 1:n_times

    data_at_time = filter(time => ==(times[i]), data)
    response = data_at_time[!, response_var]

    if all(==(response[1]), response)
      z_matrix[:,i] .= NaN
      singular_fits[i] = NaN
      rePCA_95_matrix[:,i] .= NaN
    else
      time_mod = fit(MixedModel, formula, data_at_time, family; contrasts = contrasts, opts...)
      z_matrix[:,i] = z_value(time_mod)
      singular_fits[i] = issingular(time_mod)
      rePCA_95_matrix[:,i] = [all(isnan, x) ? 0 : findfirst(>(.95), x) for x in time_mod.rePCA]
    end

    next!(p)
  end

  (
    singular_fits = singular_fits,
    z_matrix = z_matrix, Predictors = fixed, Time = times,
    rePCA_95_matrix = rePCA_95_matrix, Grouping = grouping_vars
  )

end
