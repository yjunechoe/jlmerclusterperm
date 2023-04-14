function jlmer(formula, data, family, contrasts, is_mem; opts...)
    if is_mem
        fit(MixedModel, formula, data, family; contrasts = contrasts, opts...)
    else
        glm(formula, data, family)
    end
end
