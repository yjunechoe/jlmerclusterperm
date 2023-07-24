function jlmer(
    formula::FormulaTerm,
    data::DataFrame,
    family::Distribution,
    contrasts::Union{Nothing,Dict},
    is_mem::Bool;
    opts...,
)
    if is_mem
        fit(MixedModel, formula, data, family; contrasts = contrasts, opts...)
    else
        glm(formula, data, family)
    end
end
