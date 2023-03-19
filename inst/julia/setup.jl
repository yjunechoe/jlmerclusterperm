using DataFrames
using MixedModels

function jlmer(formula, data, contrasts)
  fit(MixedModel, formula, data; contrasts = contrasts)
end
