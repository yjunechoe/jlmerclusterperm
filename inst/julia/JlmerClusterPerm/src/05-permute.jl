"""
    permute_by_predictor(df::DataFrame, shuffle_type::String,
                         predictor_cols::Union{String,Vector{String}},
                         participant_col::String,
                         trial_col::Union{Nothing,String},
                         n::Integer, global_opts::NamedTuple)

Permute data for CPA, respecting the grouping structure(s) of observations.

!!! note
    Called from R function `jlmerclusterperm::permute_by_predictor()`
"""
function permute_by_predictor(
    df::DataFrame,
    shuffle_type::String,
    predictor_cols::Union{String,Vector{String}},
    participant_col::String,
    trial_col::Union{Nothing,String},
    n::Integer,
    global_opts::NamedTuple,
)
    _df = copy(df)
    shuffler = UnitShuffler(_df, shuffle_type, predictor_cols, participant_col, trial_col)
    out = insertcolval(shuffle_units!(_df, shuffler, global_opts.rng), :id, 1)
    for i in 2:n
        append!(out, insertcolval(shuffle_units!(_df, shuffler, global_opts.rng), :id, i))
    end
    return select!(out, :id, Not(:id))
end

"""
    UnitShuffler(df::DataFrame, shuffle_type::String,
                 predictor_cols::Union{String,Vector{String}},
                 participant_col::String, trial_col::Union{Nothing,String})

Precomputed mapping from rows of `df` to the units that predictor values are shuffled
between: participants for `"between_participant"` and trials within participant for
`"within_participant"`. Every row of a unit shares the unit's predictor values, which
preserves the temporal structure of each unit's time series.

Predictors must be constant within each unit.
"""
struct UnitShuffler
    predictor_cols::Vector{String}
    unit_of_row::Vector{Int}
    unit_values::Vector{AbstractVector}
    groups::Vector{Vector{Int}}
end

function UnitShuffler(
    df::DataFrame,
    shuffle_type::String,
    predictor_cols::Union{String,Vector{String}},
    participant_col::String,
    trial_col::Union{Nothing,String},
)
    predictor_cols = vcat(predictor_cols)
    trial_col = trial_col == "" ? nothing : trial_col
    if shuffle_type == "between_participant"
        unit_cols = [participant_col]
    elseif shuffle_type == "within_participant"
        if isnothing(trial_col)
            throw(
                ArgumentError("Shuffling within participant requires a column for `trial`.")
            )
        end
        unit_cols = [participant_col, trial_col]
    else
        throw(ArgumentError("Unknown shuffle type \"$shuffle_type\"."))
    end

    units = unique(df[!, vcat(unit_cols, predictor_cols)])
    unit_keys = Tuple.(eachrow(units[!, unit_cols]))
    if !allunique(unit_keys)
        dup = unit_keys[findfirst(k -> count(==(k), unit_keys) > 1, unit_keys)]
        throw(
            ArgumentError(
                "Cannot shuffle $(join(predictor_cols, ", ")) as $shuffle_type: " *
                "values vary within $(join(unit_cols, " x ")) $(join(dup, " x ")).",
            ),
        )
    end
    unit_index = Dict(k => i for (i, k) in enumerate(unit_keys))
    unit_of_row = [unit_index[k] for k in Tuple.(eachrow(df[!, unit_cols]))]

    # Shuffle among all participants, or among trials within each participant
    if shuffle_type == "between_participant"
        groups = [collect(1:nrow(units))]
    else
        groups = [collect(parentindices(sdf)[1]) for sdf in groupby(units, participant_col)]
    end

    unit_values = AbstractVector[copy(units[!, col]) for col in predictor_cols]
    return UnitShuffler(predictor_cols, unit_of_row, unit_values, groups)
end

"""
    shuffle_units!(df::DataFrame, shuffler::UnitShuffler, rng::AbstractRNG)

Permute predictor values between the units of each group in `shuffler`.

Consumes `rng` identically to the join-based shuffling of earlier versions, which
reassigned unit labels with `shuffle!()` and joined predictor values back onto `df`.
Like the join, predictor columns are moved to the end of `df` and allow missing values.
"""
function shuffle_units!(df::DataFrame, shuffler::UnitShuffler, rng::AbstractRNG)
    for group in shuffler.groups
        # Fisher-Yates draws depend only on length, so this matches shuffling unit labels
        perm = shuffle!(rng, collect(eachindex(group)))
        for values in shuffler.unit_values
            values[group[perm]] = values[group]
        end
    end
    for (col, values) in zip(shuffler.predictor_cols, shuffler.unit_values)
        df[!, col] = allowmissing(values[shuffler.unit_of_row])
    end
    return select!(df, Not(shuffler.predictor_cols), shuffler.predictor_cols)
end

function guess_shuffle_as(
    df::DataFrame,
    predictor_cols::Union{String,Vector{String}},
    participant_col::String,
    trial_col::Union{Nothing,String},
)
    subj_pred_pair = unique(df[!, vcat(participant_col, predictor_cols)])
    unique_combinations = length(unique(df[!, participant_col])) == nrow(subj_pred_pair)
    if unique_combinations
        "between_participant"
    elseif isnothing(trial_col) || trial_col == ""
        throw(
            ArgumentError(
                "Guessed \"within_participant\" but no column for `trial` supplied."
            ),
        )
    else
        "within_participant"
    end
end
