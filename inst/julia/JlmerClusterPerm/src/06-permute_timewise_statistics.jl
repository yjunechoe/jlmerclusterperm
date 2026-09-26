"""
    compute_timewise_statistics(formula::FormulaTerm, data::DataFrame, time::String,
                                family::Distribution, contrasts::Union{Nothing,Dict},
                                term_groups::Tuple, statistic::String, is_mem::Bool,
                                global_opts::NamedTuple; opts...)

Generate permutation of the data and compute timewise test statistics
from regression models fitted to each time point for each permuted sample.

`opts...` are passed to fit() for mixed models (`is_mem = true`)

!!! note
    Called from R function `jlmerclusterperm::permute_timewise_statistics()`
"""
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
    predictors_subset::Union{Nothing,AbstractVector},
    statistic::String,
    is_mem::Bool,
    global_opts::NamedTuple;
    opts...,
)
    response_var = formula.lhs.sym
    times = sort(unique(data[!, time]))
    n_times = length(times)

    term_groups_est = estimable_term_groups(term_groups, predictors_subset)

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
            permute_data, predictors, participant_col, trial_col
        )
        shuffler = UnitShuffler(
            permute_data, shuffle_type, predictors, participant_col, trial_col
        )

        if statistic == "chisq"
            reduced_formula = reduce_formula(Symbol.(predictors), form, is_mem)
            test_opts = (reduced_formula=(fm=reduced_formula, i=term_groups.i),)
        elseif statistic == "t"
            test_opts = nothing
        end

        for i in 1:nsim
            shuffle_units!(permute_data, shuffler, global_opts.rng)
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

function estimable_term_groups(
    term_groups::Tuple, predictors_subset::Union{Nothing,AbstractVector}
)
    predictors_exclude = ["(Intercept)"]
    if isnothing(predictors_subset)
        filter(grp -> !all(in(predictors_exclude), grp.p), term_groups)
    else
        filter(grp -> any(in(predictors_subset), vcat(grp.p, grp.P)), term_groups)
    end
end

"""
    permute_null_cluster_dists(formula::FormulaTerm, data::DataFrame, time::String,
                               family::Distribution, contrasts::Union{Nothing,Dict},
                               nsim::Integer, participant_col::String,
                               trial_col::Union{Missing,String}, term_groups::Tuple,
                               predictors_subset::Union{Nothing,AbstractVector},
                               statistic::String,
                               is_mem::Bool, global_opts::NamedTuple,
                               thresholds::Dict, binned::Bool; opts...)

Fused `permute_timewise_statistics()` and `extract_clusters()` which keeps the
simulation-by-time-by-predictor array in Julia and only returns the largest
cluster from each simulation.

!!! note
    Called from R function `jlmerclusterperm::clusterpermute()`
"""
function permute_null_cluster_dists(
    formula::FormulaTerm,
    data::DataFrame,
    time::String,
    family::Distribution,
    contrasts::Union{Nothing,Dict},
    nsim::Integer,
    participant_col::String,
    trial_col::Union{Missing,String},
    term_groups::Tuple,
    predictors_subset::Union{Nothing,AbstractVector},
    statistic::String,
    is_mem::Bool,
    global_opts::NamedTuple,
    thresholds::Dict,
    binned::Bool;
    opts...,
)
    z_array = permute_timewise_statistics(
        formula,
        data,
        time,
        family,
        contrasts,
        nsim,
        participant_col,
        trial_col,
        term_groups,
        predictors_subset,
        statistic,
        is_mem,
        global_opts;
        opts...,
    ).z_array

    # One slice per term for t, one slice per term group for chisq
    term_groups_est = estimable_term_groups(term_groups, predictors_subset)
    if statistic == "t"
        predictors = vcat(map(grp -> grp.p, term_groups_est)...)
        slices = collect(1:length(predictors))
    else
        predictors = [grp.P for grp in term_groups_est]
        group_sizes = [length(grp.i) for grp in term_groups_est]
        slices = cumsum(group_sizes) .- group_sizes .+ 1
    end
    # JuliaConnectoR hangs translating empty vectors, so avoid returning any
    if isempty(predictors)
        return (clusters=nothing, predictors=nothing, nan_counts=nothing)
    end

    clusters = DataFrame[]
    # Number of simulations with convergence failures per predictor
    nan_counts = zeros(Int, length(predictors))
    for (k, predictor) in enumerate(predictors)
        threshold = abs(thresholds[predictor])
        t_matrix = map(x -> abs(x) <= threshold ? zero(x) : x, z_array[:, :, slices[k]])
        has_nan = vec(any(isnan, t_matrix; dims=2))
        nan_counts[k] = count(has_nan)
        predictor_clusters = extract_clusters(t_matrix[.!has_nan, :], binned, 1)
        predictor_clusters.predictor .= k
        push!(clusters, predictor_clusters)
    end
    clusters_df = vcat(clusters...; cols=:setequal)

    return (
        clusters=(; (Symbol(col) => clusters_df[!, col] for col in names(clusters_df))...),
        predictors=predictors,
        nan_counts=nan_counts,
    )
end
