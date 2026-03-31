% Start by getting patient data from spreadsheet
T = readtable("aphasia_combined.xlsx");

% Language dominance score (1 = eng dominant, 0 = spa dominant)
% First calculating the individual scores
T.exposure = T.exp_score/10; % 10 = eng dominant, 0 = spa dominant so divide by 10
T.confidence = T.conf_eng / (T.conf_eng + T.conf_spa);
T.pre = T.self_english_pre / (T.self_english_pre + T.self_spanish_pre);
T.post = T.self_english_post / (T.self_english_post + T.self_spanish_post);
% Taking average of variables for overall dominance
T.dominance = mean([T.exposure, T.confidence, T.pre, T.post], 2, 'omitnan')

% Reformatting data
% WAB
WAB = [T.seq_com_eng; T.seq_com_span];
% Language
Language = [repmat({'English'}, height(T), 1); repmat({'Spanish'}, height(T), 1)];
% Rest of variables
Participant = [T.BUID; T.BUID];
Dominance   = [T.dominance; T.dominance];
Age         = [T.age; T.age];
Education   = [T.education; T.education];
Exposure    = [T.exp_score; T.exp_score];
Confidence  = [T.conf_eng; T.conf_spa]; 
% Rebuilding reformatted table
new_table = table(WAB, Language, Participant, Dominance, Age, Education, ...
             Exposure, Confidence);
new_table.Language = categorical(new_table.Language);

% Testing linear mixed effects model
% Linear mixed effects model (no covariates yet)
m = fitlme(new_table, 'WAB ~ Language * Dominance + (1|Participant)');
% Adding age -> p = 0.68565 so NOT significant
m_age = fitlme(new_table, 'WAB ~ Language * Dominance + Age + (1|Participant)');
% Seeing if age improves model
compare(m, m_age);
% esting education -> p = 0.029459 so IS significant
m_edu = fitlme(new_table, 'WAB ~ Language * Dominance + Education + (1|Participant)');
compare(m, m_edu);
% Final with just education (since it is significant)
final = m_edu;
% Display final model results
disp(final)
