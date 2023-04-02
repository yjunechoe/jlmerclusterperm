function guess_and_shuffle_as!(df, predictor_cols, participant_col, trial_col)
  shuffle_type = guess_shuffle_as(df, predictor_cols, participant_col, trial_col)
  shuffle_as!(df, predictor_cols, participant_col, trial_col, shuffle_type)
end

function guess_shuffle_as(df, predictor_cols, participant_col, trial_col)
  subj_pred_pair = unique(df[!,vcat(participant_col, predictor_cols)])
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

function shuffle_as!(df, shuffle_type, predictor_cols, participant_col, trial_col)
  if shuffle_type == "between_participant"
    subj_pred_pair = unique(df[!, vcat(participant_col, predictor_cols)])
    shuffle!(rng, subj_pred_pair[!, participant_col])
    select!(df, Not(predictor_cols))
    leftjoin!(df, subj_pred_pair, on = participant_col)
  elseif shuffle_type == "within_participant"
    trial_pred_pair = unique(df[!, vcat(participant_col, trial_col, predictor_cols)])
    combine(groupby(trial_pred_pair, participant_col), sdf -> shuffle!(rng, sdf[!, trial_col]))
    select!(df, Not(predictor_cols))
    leftjoin!(df, trial_pred_pair, on = [participant_col, trial_col])
  end
end

function permute_by_predictor(df, shuffle_type, predictor_cols, participant_col, trial_col, n)
  _df = copy(df)
  out = insertcols(shuffle_as!(_df, shuffle_type, predictor_cols, participant_col, trial_col), :id => 1)
  if (n > 1)
    for i in 2:n
      append!(out, insertcols(shuffle_as!(_df, shuffle_type, predictor_cols, participant_col, trial_col), :id => i))
    end
  end
  select!(out, :id, Not(:id))
end
