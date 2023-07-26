function permute_timewise_statistics(
    formula::FormulaTerm,
    data::DataFrame,
    time::String,
    family::Distribution,
    contrasts::Union{Nothing,Dict},
    nsim::Integer,
    participant_col::String,
    trial_col::Union{Missing,String},
    term_groups::Tuple,
    predictors_subset::Union{Nothing,Dict},
    statistic::String,
    is_mem::Bool,
    global_opts::NamedTuple;
    opts...,
)
    response_var = formula.lhs.sym
    times = sort(unique(data[!, time]))
    n_times = length(times)

    predictors_exclude = ["(Intercept)"]
    if isnothing(predictors_subset)
        term_groups_est = filter(grp -> !all(in(predictors_exclude), grp.p), term_groups)
    else
        term_groups_est = filter(
            grp -> any(in(predictors_subset), vcat(grp.p, grp.P)), term_groups
        )
    end

    nsims = nsim * length(term_groups_est)
    pg = Progress(
        nsims;
        output=global_opts.pg[:io],
        barlen=global_opts.pg[:width],
        showspeed=true
    )

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

    n_fixed = length(fixed)
    res = zeros(nsim, n_times, n_fixed)

    for term_groups in term_groups_est
        predictors = term_groups.p
        permute_data = copy(data)
        shuffle_type = guess_shuffle_as(
            permute_data,
            predictors,
            participant_col,
            trial_col == "" ? nothing : 3
        )

        if statistic == "chisq"
            reduced_formula = reduce_formula(Symbol.(predictors), form, is_mem)
            test_opts = (reduced_formula=(fm=reduced_formula, i=term_groups.i),)
        elseif statistic == "t"
            test_opts = nothing
        end

        for i in 1:nsim
            shuffle_as!(
                permute_data,
                shuffle_type,
                predictors,
                participant_col,
                trial_col,
                global_opts.rng,
            )
            if is_mem
                timewise_stats = timewise_lme(
                    formula,
                    permute_data,
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
                    false,
                    global_opts;
                    opts...,
                )
                zs = timewise_stats.t_matrix
            else
                timewise_stats = timewise_lm(
                    formula,
                    permute_data,
                    time,
                    family,
                    statistic,
                    test_opts,
                    response_var,
                    fixed,
                    times,
                    n_times,
                )
                zs = timewise_stats.t_matrix
            end
            for term_ind in term_groups.i
                res[i, :, term_ind] = zs[term_ind, :]
            end
            next!(pg)
        end
    end

    predictors = vcat(map(terms -> terms.p, term_groups_est)...)
    res = res[:, :, vcat(map(terms -> terms.i, term_groups_est)...)]

    return (z_array=res, predictors=predictors)
end
