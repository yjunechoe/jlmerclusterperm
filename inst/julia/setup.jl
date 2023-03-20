using DataFrames
using MixedModels

function jlmer(formula, data, family, contrasts; opts...)
  fit(MixedModel, formula, data, family; contrasts = contrasts, opts...)
end
