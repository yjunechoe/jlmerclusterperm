"""
    compute_timewise_statistics(formula::FormulaTerm, data::DataFrame, time::String,
                                family::Distribution, contrasts::Union{Nothing,Dict},
                                term_groups::Tuple, statistic::String, is_mem::Bool,
                                global_opts::NamedTuple; opts...)

Compute timewise test statistics from regression models fitted to each time point.

`opts...` are passed to fit() for mixed models (`is_mem = true`)

!!! note
    Called from R function `jlmerclusterperm::compute_timewise_statistics()`
"""
function compute_timewise_statistics(
    formula::FormulaTerm,
    data::DataFrame,
    time::String,
    family::Distribution,
    contrasts::Union{Nothing,Dict},
    term_groups::Tuple,
    statistic::String,
    is_mem::Bool,
    global_opts::NamedTuple;
    opts...,
)
    response_var = formula.lhs.sym
    times = sort(unique(data[!, time]))
    n_times = length(times)

    if is_mem
        fm_schema = MixedModels.schema(formula, data, contrasts)
        form = MixedModels.apply_schema(formula, fm_schema, MixedModel)
        re_term = [isa(x, MixedModels.AbstractReTerm) for x in form.rhs]
        fixed = String.(Symbol.(form.rhs[.!re_term][1].terms))
        grouping_vars = [String(Symbol(x.rhs)) for x in form.rhs[re_term]]
    else
        fm_schema = StatsModels.schema(formula, data)
        form = StatsModels.apply_schema(formula, fm_schema)
        fixed = String.(Symbol.(form.rhs.terms))
    end

    if statistic == "chisq"
        drop_terms = filter(x -> x.p != ["(Intercept)"], term_groups)
        reduced_formula = [
            (fm=reduce_formula(Symbol.(x.p), form, is_mem), i=x.i) for x in drop_terms
        ]
        test_opts = (reduced_formula=reduced_formula,)
    elseif statistic == "t"
        test_opts = nothing
    end

    if is_mem
        timewise_stats = timewise_lme(
            formula,
            data,
            time,
            family,
            contrasts,
            statistic,
            test_opts,
            response_var,
            fixed,
            grouping_vars,
            times,
            n_times,
            true,
            global_opts;
            opts...,
        )
        timewise_stats
    else
        timewise_stats = timewise_lm(
            formula,
            data,
            time,
            family,
            statistic,
            test_opts,
            response_var,
            fixed,
            times,
            n_times,
        )
        (
            t_matrix=timewise_stats.t_matrix,
            convergence_failures=timewise_stats.convergence_failures,
            Predictor=fixed,
            Time=times,
        )
    end
end

function timewise_lme(
    formula::FormulaTerm,
    data::DataFrame,
    time::String,
    family::Distribution,
    contrasts::Union{Nothing,Dict},
    statistic::String,
    test_opts::Union{Nothing,NamedTuple},
    response_var::Symbol,
    fixed::NTuple,
    grouping_vars::Vector{String},
    times::Vector{<:Real},
    n_times::Integer,
    is_compute::Bool,
    global_opts::NamedTuple;
    opts...,
)
    if statistic == "t"
        t_matrix = zeros(length(fixed), n_times)
    elseif statistic == "chisq"
        drop_formula = test_opts.reduced_formula
        if drop_formula isa Vector
            t_matrix = zeros(length(drop_formula), n_times)
        else
            t_matrix = zeros(length(fixed), n_times)
        end
    end

    convergence_failures = zeros(Bool, n_times)
    if is_compute
        singular_fits = zeros(Bool, n_times)
        rePCA_95_matrix = zeros(length(grouping_vars), n_times)
    end

    if is_compute
        pg = Progress(
            n_times;
            output=global_opts.pg[:io],
            barlen=global_opts.pg[:width],
            showspeed=true,
        )
    end

    @suppress begin
        Threads.@threads for i in 1:n_times
            data_at_time = filter(time => ==(times[i]), data)
            response = data_at_time[!, response_var]

            if all(==(response[1]), response)
                t_matrix[:, i] .= response[1] == 1 ? Inf : -Inf
                convergence_failures[i] = missing
                if is_compute
                    singular_fits[i] = missing
                    rePCA_95_matrix[:, i] .= missing
                end
            else
                try
                    time_mod = MixedModel(formula, data_at_time, family; contrasts)
                    if !is_compute
                        time_mod.optsum.ftol_rel = 1e-8
                    end
                    fit!(time_mod; opts...)
                    # test statistic
                    if statistic == "chisq"
                        if drop_formula isa Vector
                            drop1_mods = map(
                                x -> fit(
                                    MixedModel,
                                    x.fm,
                                    data_at_time,
                                    family;
                                    contrasts=contrasts,
                                    opts...,
                                ),
                                drop_formula,
                            )
                            betas = map(x -> coef(time_mod)[x.i], drop_formula)
                            chisq_vals = map(
                                x -> MixedModels.likelihoodratiotest(
                                time_mod,
                                x
    ).tests.deviancediff[1],
                                drop1_mods,
                            )
                            signed_chisq = [
                                chisq_vals[i] *
                                (length(betas[i]) == 1 ? sign(betas[i][1]) : 1) for
                                i in 1:length(chisq_vals)
                            ]
                            t_matrix[:, i] = signed_chisq
                        else
                            drop1_mod = fit(
                                MixedModel,
                                drop_formula.fm,
                                data_at_time,
                                family;
                                contrasts=contrasts,
                                opts...,
                            )
                            chisq_val = MixedModels.likelihoodratiotest(
                            time_mod,
                            drop1_mod
).tests.deviancediff[1]
                            betas = coef(time_mod)[drop_formula.i]
                            signed_chisq =
                                chisq_val * (length(betas) == 1 ? sign(betas[1]) : 1)
                            t_matrix[drop_formula.i, i] .= signed_chisq
                        end
                    elseif statistic == "t"
                        t_matrix[:, i] = t_value(time_mod)
                    end

                    if is_compute
                        singular_fits[i] = issingular(time_mod)
                        rePCA_95_matrix[:, i] = [
                            all(isnan, x) ? 0 : findfirst(>(0.95), x) for
                            x in time_mod.rePCA
                        ]
                    end
                catch e
                    convergence_failures[i] = true
                    t_matrix[:, i] .= NaN
                end
            end

            if is_compute
                next!(pg)
            end
        end
    end

    if is_compute
        (
            singular_fits=singular_fits,
            convergence_failures=convergence_failures,
            t_matrix=t_matrix,
            Predictor=fixed,
            Time=times,
            rePCA_95_matrix=rePCA_95_matrix,
            Grouping=grouping_vars,
        )
    else
        (t_matrix=t_matrix, convergence_failures=convergence_failures)
    end
end

function timewise_lm(
    formula::FormulaTerm,
    data::DataFrame,
    time::String,
    family::Distribution,
    statistic::String,
    test_opts::Union{Nothing,NamedTuple},
    response_var::Symbol,
    fixed::NTuple,
    times::Vector{<:Real},
    n_times::Integer,
)
    if statistic == "t"
        t_matrix = zeros(length(fixed), n_times)
    elseif statistic == "chisq"
        drop_formula = test_opts.reduced_formula
        if drop_formula isa Vector
            t_matrix = zeros(length(drop_formula), n_times)
        else
            t_matrix = zeros(length(fixed), n_times)
        end
    end

    convergence_failures = zeros(Bool, n_times)

    Threads.@threads for i in 1:n_times
        data_at_time = filter(time => ==(times[i]), data)
        response = data_at_time[!, response_var]
        if all(==(response[1]), response)
            constant = response[1] == 1 ? Inf : -Inf
            t_matrix[:, i] .= constant
        else
            try
                time_mod = glm(formula, data_at_time, family)
                if statistic == "chisq"
                    if drop_formula isa Vector
                        drop1_mods = map(x -> glm(x.fm, data_at_time, family), drop_formula)
                        betas = map(x -> coef(time_mod)[x.i], drop_formula)
                        chisq_vals = map(x -> chisq_value(lrtest(time_mod, x)), drop1_mods)
                        signed_chisq = [
                            chisq_vals[i] * (length(betas[i]) == 1 ? sign(betas[i][1]) : 1)
                            for
                            i in 1:length(chisq_vals)
                        ]
                        t_matrix[:, i] = signed_chisq
                    else
                        chisq_val = chisq_value(
                            lrtest(time_mod, glm(drop_formula.fm, data_at_time, family))
                        )
                        betas = coef(time_mod)[drop_formula.i]
                        signed_chisq = chisq_val * (length(betas) == 1 ? sign(betas[1]) : 1)
                        t_matrix[drop_formula.i, i] .= signed_chisq
                    end
                elseif statistic == "t"
                    t_matrix[:, i] = t_value(time_mod)
                end
            catch e
                convergence_failures[i] = true
                t_matrix[:, i] .= NaN
                continue
            end
        end
    end
    return (t_matrix=t_matrix, convergence_failures=convergence_failures)
end
