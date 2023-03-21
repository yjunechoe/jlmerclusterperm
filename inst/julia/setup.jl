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

  p = Progress(n_times)
  Threads.@threads for i = 1:n_times
    data_at_time = filter(time => ==(times[i]), data)
    time_mod = fit(MixedModel, formula, data_at_time, family; contrasts = contrasts, opts...)
    singular_fits[i] = issingular(time_mod)
    z_matrix[:,i] = z_value(time_mod)
    next!(p)
  end

  (singular_rate = mean(singular_fits), z_matrix = z_matrix, Predictors = fixed, Time = times)

end
