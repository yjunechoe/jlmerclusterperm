using Statistics
using MixedModels
using DataFrames
using ProgressMeter

function jlmer(formula, data, family, contrasts; opts...)
  fit(MixedModel, formula, data, family; contrasts = contrasts, opts...)
end

function z_value(mod)
  mod.beta ./ mod.stderror
end

function jlmer_by_time(formula, data, time, family, contrasts; opts...)

  global_mod = fit(MixedModel, formula, data, family; contrasts = contrasts, opts...)
  fixed = global_mod.feterm.cnames
  times = sort(unique(data[!,time]))
  n_times = length(times)

  z_matrix = zeros(length(fixed), n_times)
  singular_fits = zeros(n_times)
  grouping_vars = keys(VarCorr(global_mod).σρ)
  rePCA_95_matrix = zeros(length(grouping_vars), n_times)

  p = Progress(n_times)
  Threads.@threads for i = 1:n_times

    data_at_time = filter(time => ==(times[i]), data)
    time_mod = fit(MixedModel, formula, data_at_time, family; contrasts = contrasts, opts...)

    z_matrix[:,i] = z_value(time_mod)
    singular_fits[i] = issingular(time_mod)
    rePCA_95_matrix[:,i] = [all(isnan, x) ? 0 : findfirst(>(.95), x) for x in time_mod.rePCA]

    next!(p)
  end

  (
    singular_rate = mean(singular_fits),
    z_matrix = z_matrix, Predictors = fixed, Time = times,
    rePCA_95_matrix = rePCA_95_matrix, Grouping = grouping_vars
  )

end
