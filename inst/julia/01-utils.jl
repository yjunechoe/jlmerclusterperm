function t_value(mod)
  coef(mod) ./ stderror(mod)
end

function get_rng_counter()
  Int(rng.ctr1)
end

reduce_formula = function(to_remove, enriched_formula, is_mem)
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
  FormulaTerm(enriched_formula.lhs, new_rhs)
end

chisq_value = function(lrt)
  lrt.pval[2] < 0.05 ? abs.(2 .* (lrt.loglikelihood[2] - lrt.loglikelihood[1])) : 0
end
