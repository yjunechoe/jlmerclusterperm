"""
    jlmer(formula::FormulaTerm, data::DataFrame, family::Distribution,
          contrasts::Union{Nothing,Dict}, is_mem::Bool; opts...)

Fit a (mixed-effects) regression model using GLM.jl or MixedModels.jl

`opts...` are passed to fit() for mixed models (`is_mem = true`)

!!! note
    Called from R function `jlmerclusterperm::jlmer()` and
    `jlmerclusterperm::to_jlmer()`
"""
function jlmer(
    formula::FormulaTerm,
    data::DataFrame,
    family::Distribution,
    contrasts::Union{Nothing,Dict},
    is_mem::Bool;
    opts...,
)
    if is_mem
        fit(MixedModel, formula, data, family; contrasts=contrasts, opts...)
    else
        glm(formula, data, family)
    end
end
