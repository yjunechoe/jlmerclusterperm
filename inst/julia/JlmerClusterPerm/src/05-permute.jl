"""
    permute_by_predictor(df::DataFrame, shuffle_type::String,
                         predictor_cols::Union{String,Vector{String}},
                         participant_col::String,
                         trial_col::Union{Nothing,Integer,String},
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
    trial_col::Union{Nothing,Integer,String},
    n::Integer,
    global_opts::NamedTuple,
)
    _df = copy(df)
    out = insertcols(
        shuffle_as!(
            _df,
            shuffle_type,
            predictor_cols,
            participant_col,
            trial_col,
            global_opts.rng
        ),
        :id => 1,
    )
    if (n > 1)
        for i in 2:n
            append!(
                out,
                insertcols(
                    shuffle_as!(
                        _df,
                        shuffle_type,
                        predictor_cols,
                        participant_col,
                        trial_col,
                        global_opts.rng,
                    ),
                    :id => i,
                ),
            )
        end
    end
    return select!(out, :id, Not(:id))
end

function shuffle_as!(
    df::DataFrame,
    shuffle_type::String,
    predictor_cols::Union{String,Vector{String}},
    participant_col::String,
    trial_col::Union{Nothing,Integer,String},
    rng::AbstractRNG,
)
    if shuffle_type == "between_participant"
        subj_pred_pair = unique(df[!, vcat(participant_col, predictor_cols)])
        shuffle!(rng, subj_pred_pair[!, participant_col])
        select!(df, Not(predictor_cols))
        leftjoin!(df, subj_pred_pair; on=participant_col)
    elseif shuffle_type == "within_participant"
        trial_pred_pair = unique(df[!, vcat(participant_col, trial_col, predictor_cols)])
        combine(
            groupby(trial_pred_pair, participant_col),
            sdf -> shuffle!(rng, sdf[!, trial_col]),
        )
        select!(df, Not(predictor_cols))
        leftjoin!(df, trial_pred_pair; on=[participant_col, trial_col])
    end
end

function guess_shuffle_as(
    df::DataFrame,
    predictor_cols::Union{String,Vector{String}},
    participant_col::String,
    trial_col::Union{Nothing,Integer,String},
)
    subj_pred_pair = unique(df[!, vcat(participant_col, predictor_cols)])
    unique_combinations = length(unique(df[!, participant_col])) == nrow(subj_pred_pair)
    if unique_combinations
        "between_participant"
    else
        if (ismissing(trial_col) || isnothing(trial_col))
            throw("""Guessed "within_participant" but no column for `trial` supplied.""")
        else
            "within_participant"
        end
    end
end
