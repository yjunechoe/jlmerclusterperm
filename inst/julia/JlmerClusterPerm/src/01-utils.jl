function t_value(mod::RegressionModel)
    return coef(mod) ./ stderror(mod)
end

function chisq_value(lrt::StatsModels.LRTestResult)
    return abs(2 * (lrt.loglikelihood[2] - lrt.loglikelihood[1]))
end

function reduce_formula(
    to_remove::Vector{Symbol},
    enriched_formula::FormulaTerm,
    is_mem::Bool
)
    rhs = enriched_formula.rhs
    if is_mem
        is_fe = [map(x -> x isa MatrixTerm, rhs)...]
        fe_terms = rhs[findfirst(is_fe)].terms
        fe_to_keep = findall(!in(to_remove), Symbol.(fe_terms))
        fe_keep = MixedModels.collect_matrix_terms(fe_terms[fe_to_keep])
        new_rhs = (fe_keep, rhs[.!is_fe]...)
    else
        fe_terms = rhs.terms
        fe_to_keep = findall(!in(to_remove), Symbol.(fe_terms))
        new_rhs = StatsModels.collect_matrix_terms(fe_terms[fe_to_keep])
    end
    return FormulaTerm(enriched_formula.lhs, new_rhs)
end

# backports DF.jl
function insertcolval(df::DataFrame, key::Symbol, val::Any)
    _df = copy(df)
    _df[!, key] .= val
    _df
end
