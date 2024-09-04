write_full_POMDP <- function(TR_FUNCTION_ECO,
                             TR_FUNCTION_TECH,
                             B_FULL_ECO,
                             B_FULL_TECH,
                             B_PAR,
                             B_PAR_TECH,
                             REW,
                             GAMMA,
                             FILE){
  #POMDP with:
  # factored set of observable states:
  #   #ecosystem_states
  #   #temperature_states
  #   #time_states
  #   #
  # set of non-observable states:
  #   #temperature models over time
  #set of actions:
  #   #U = deploy - BAU

  #The user needs to input:
  # TR_FUNCTION_ECO:

  # TR_FUNCTION_TECH: list of lists
  #     TR_FUNCTION_TECH[[1]]: list of transition functions for model 1: success
  #         TR_FUNCTION_TECH[[1]][[1]]: matrix of transition functions for action 1: BAU
  #         TR_FUNCTION_TECH[[1]][[1]]: matrix of transition functions for action 2: develop

  #     TR_FUNCTION_TECH[[2]]: list of transition functions for model 2: failure
  #         TR_FUNCTION_TECH[[2]][[1]]: matrix of transition functions for action 1: BAU
  #         TR_FUNCTION_TECH[[2]][[1]]: matrix of transition functions for action 2: develop

  # B_FULL_ECO: vector, probability distribution over the fully observable ecosystem states
  # B_FULL_TECH: vector, probability distribution over the fully observable tech states

  # B_PAR : vector, probability distribtution over the non observable states (number of models for ecosystem)
  # B_PAR_TECH : vector, probability distribtution over the non observable states (number of models for tech)

  # REW: matrix of size [s,a]: number of ecosystem states x number of actions

  # GAMMA: number between 0 and 1, the discount factor
  # FILE: string, path to the pomdpx file

  Num_eco <- dim(TR_FUNCTION_ECO[[1]])[1] #number of ecosystem states
  Num_tech <- dim(TR_FUNCTION_TECH[[1]][[1]])[1]#number of tech states

  Num_mod <- length(TR_FUNCTION_ECO) #number of models for temperature dynamics
  Num_mod_tech <- length(TR_FUNCTION_TECH) #number of models for tech dynamics

  Num_a <-dim(TR_FUNCTION_ECO[[1]])[3] #number of actions

  STATES_ECO <- paste0("obs_state_eco", 1:Num_eco)
  STATES_TECH <- paste0("obs_state_tech", 1:Num_tech)

  MODELS <- paste0("model", 1:Num_mod)
  MODELS_TECH <- paste0("model_tech", 1:Num_mod_tech)

  ACTIONS <- paste0("action", 1:Num_a)

  #build header ####
  header <- paste0("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n\n",
                   "<pomdpx version =\"1.0\" id=\"sample\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
                   xsi:noNamespaceSchemaLocation=\"pomdpx.xsd\">\n\n",
                   "<Description>hmMDP model</Description>\n\n",
                   paste0("<Discount>",  GAMMA, "</Discount>"), "\n\n",
                   "<Variable>", "\n\n")

  #header states
  header_state_eco <- paste0("<StateVar vnamePrev=\"eco_0\" vnameCurr=\"eco_1\" fullyObs=\"true\">",
                             "\n", paste0("<ValueEnum>", paste0(STATES_ECO, collapse = " "),
                                          "</ValueEnum>"), "\n", "</StateVar>", "\n\n")

  header_state_tech <- paste0("<StateVar vnamePrev=\"tech_0\" vnameCurr=\"tech_1\" fullyObs=\"true\">",
                              "\n", paste0("<ValueEnum>", paste0(STATES_TECH, collapse = " "),
                                           "</ValueEnum>"), "\n", "</StateVar>", "\n\n")

  # header model
  header_model <- paste0("<StateVar vnamePrev=\"hidden_0\" vnameCurr=\"hidden_1\" fullyObs=\"false\">",
                         "\r\n", paste0("<ValueEnum>", paste0(MODELS, collapse =" "),"</ValueEnum>",
                                        "\r\n</StateVar>\r\n\n"))

  header_model_tech <- paste0("<StateVar vnamePrev=\"hidden_tech_0\" vnameCurr=\"hidden_tech_1\" fullyObs=\"false\">",
                              "\r\n", paste0("<ValueEnum>", paste0(MODELS_TECH, collapse =" "),"</ValueEnum>",
                                             "\r\n</StateVar>\r\n\n"))

  #header actions
  header_action <- paste0("<ActionVar vname=\"action_control\">\r\n",
                          "<ValueEnum>", paste0(ACTIONS, collapse = " "),
                          "</ValueEnum>\r\n</ActionVar>\r\n\n")

  #header obs
  header_obs_rew <- paste0("<ObsVar vname=\"obs\">\r\n<ValueEnum>o</ValueEnum>\r\n</ObsVar>\r\n\n",
                           " <RewardVar vname=\"reward_agent\" />\r\n</Variable>\r\n\n")
  #header_belief
  header_belief <- paste0("<InitialStateBelief>\r\n\n",
                          #states ecosystem
                          "<CondProb>\r\n",
                          "<Var>eco_0</Var>\r\n",
                          "<Parent>null</Parent>\r\n",
                          "<Parameter type=\"TBL\">\r\n",
                          "<Entry>\r\n",
                          "<Instance> - </Instance>\r\n",
                          "<ProbTable>", paste0(B_FULL_ECO, collapse = " "), "</ProbTable>\n",
                          "</Entry>\r\n",
                          "</Parameter>\r\n",
                          "</CondProb>\r\n\n",

                          #states tech
                          "<CondProb>\r\n",
                          "<Var>tech_0</Var>\r\n",
                          "<Parent>null</Parent>\r\n",
                          "<Parameter type=\"TBL\">\r\n",
                          "<Entry>\r\n",
                          "<Instance> - </Instance>\r\n",
                          "<ProbTable>", paste0(B_FULL_TECH, collapse = " "), "</ProbTable>\n",
                          "</Entry>\r\n",
                          "</Parameter>\r\n",
                          "</CondProb>\r\n\n",

                          #hidden model
                          "<CondProb>\r\n",
                          "<Var>hidden_0 </Var>\r\n",
                          "<Parent>null</Parent>\r\n",
                          "<Parameter type=\"TBL\">\r\n",
                          "<Entry>\r\n",
                          "<Instance> - </Instance>\r\n",
                          "<ProbTable>", paste0(B_PAR, collapse = " "), "</ProbTable>\r\n",
                          "</Entry>\r\n",
                          "</Parameter>\r\n",
                          "</CondProb>\r\n\n",

                          #hidden model tech
                          "<CondProb>\r\n",
                          "<Var>hidden_tech_0 </Var>\r\n",
                          "<Parent>null</Parent>\r\n",
                          "<Parameter type=\"TBL\">\r\n",
                          "<Entry>\r\n",
                          "<Instance> - </Instance>\r\n",
                          "<ProbTable>", paste0(B_PAR_TECH, collapse = " "), "</ProbTable>\r\n",
                          "</Entry>\r\n",
                          "</Parameter>\r\n",
                          "</CondProb>\r\n\n",

                          "</InitialStateBelief>\r\n\n\n"
                          , sep = "")

  header <- paste0(header,
                   header_state_eco,
                   header_state_tech,
                   header_model,
                   header_model_tech,
                   header_action,
                   header_obs_rew,
                   header_belief)

  # build transition matrices for the ecosystem####
  #only depends on the action and temperature state
  tr_header_eco = paste(
    "<StateTransitionFunction>\r\n\n",
    "<CondProb>\r\n",
    "<Var>eco_1</Var>\r\n",
    "<Parent>action_control hidden_0 tech_0 eco_0</Parent>\r\n",
    "<Parameter type=\"TBL\">\r\n")

  tr_filling_eco <- ""
  for (mod_id in seq(Num_mod)){
    mod_text <- MODELS[mod_id]
    tr_filling_eco_mod <- ""
    for (act_id in seq(Num_a)){
      action_text <- ACTIONS[act_id]
      for (tech_id in seq(Num_tech)){
        tech_id_text <- STATES_TECH[tech_id]
        if (tech_id ==1){#if tech is idle, same effect as BAU (action 1)
          action_prob_values <- TR_FUNCTION_ECO[[mod_id]][,,1]
        } else {#if tech is ready effect of action
          action_prob_values <- TR_FUNCTION_ECO[[mod_id]][,,act_id]
        }
        tr_filling_eco_mod <- paste(tr_filling_eco_mod,
                                    "<Entry>\r\n",
                                    "<Instance> ",
                                    action_text, " " ,
                                    mod_text, " ",
                                    tech_id_text, " - -",
                                    " </Instance>\r\n",
                                    "<ProbTable>",
                                    paste(c(t(action_prob_values)), sep=" ", collapse = " "),
                                    "</ProbTable>\r\n",
                                    "</Entry>\r\n", sep=" ")
      }
    }
    tr_filling_eco <- paste(tr_filling_eco, "\r\n",
                            tr_filling_eco_mod)
  }

  tr_end_eco <- paste(
    "</Parameter>\r\n",
    "</CondProb>\r\n\n")

  tr_eco <- paste(tr_header_eco, tr_filling_eco, tr_end_eco)

  ## build transition for the technology ####
  tr_tech_header <- paste(
    "<CondProb>\r\n",
    "<Var>tech_1</Var>\r\n",
    "<Parent>action_control hidden_tech_0 tech_0</Parent>\r\n",
    "<Parameter type=\"TBL\">\r\n")

  mod_tr_tech_filling <- ""
  for (mod_tech_id in seq(Num_mod_tech)){
    model_text <- MODELS_TECH[mod_tech_id]
    for (act_id in seq(Num_a)){
      action_text <- ACTIONS[act_id]
      prob_values_tech <- TR_FUNCTION_TECH[[mod_tech_id]][[act_id]]

      mod_tr_tech_filling <- paste(mod_tr_tech_filling,
                                   "<Entry>\r\n",
                                   "<Instance>",
                                   action_text, " ", model_text,
                                   " - -",
                                   " </Instance>\r\n",
                                   "<ProbTable>",
                                   paste(c(t(prob_values_tech)), sep=" ", collapse = " "),
                                   "</ProbTable>\r\n",
                                   "</Entry>\r\n")
    }
  }
  mod_tr_tech_end <- paste(
    "</Parameter>\r\n",
    "</CondProb>\r\n\n")
  tr_tech <- paste(tr_tech_header, mod_tr_tech_filling, mod_tr_tech_end)

  ## transition between models ####
  tr_model <- paste("<CondProb>\r\n",
                    "<Var>hidden_1</Var>\r\n",
                    "<Parent>hidden_0</Parent>\r\n",
                    "<Parameter type=\"TBL\">\r\n",
                    "<Entry>\r\n<Instance> - - </Instance>\r\n",
                    "<ProbTable>",
                    "identity",
                    "</ProbTable>\r\n",
                    "</Entry>\r\n",
                    "</Parameter>\r\n",
                    "</CondProb>\r\n\n")

  tr_model_tech <- paste("<CondProb>\r\n",
                         "<Var>hidden_tech_1</Var>\r\n",
                         "<Parent>hidden_tech_0</Parent>\r\n",
                         "<Parameter type=\"TBL\">\r\n",
                         "<Entry>\r\n<Instance> - - </Instance>\r\n",
                         "<ProbTable>",
                         "identity",
                         "</ProbTable>\r\n",
                         "</Entry>\r\n",
                         "</Parameter>\r\n",
                         "</CondProb>\r\n\n",
                         "</StateTransitionFunction>\r\n\n\n")


  state_tr <- paste(tr_eco,
                    tr_tech,
                    tr_model,
                    tr_model_tech)

  # build div for observations ####
  obs_header <- "<ObsFunction>\r\n\n"
  obs_fill <-paste(
    "<CondProb>\r\n",
    "<Var>obs</Var>\r\n",
    "<Parent>hidden_1</Parent>\r\n",
    "<Parameter type=\"TBL\">\r\n")
  for (mod_id in c(1:Num_mod)){
    model_text <- MODELS[mod_id]
    obs_fill_mod <- paste0(
      "<Entry>\r\n",
      "<Instance> ",
      model_text,
      " o</Instance>\r\n",
      "<ProbTable>1</ProbTable>\r\n",
      "</Entry>\r\n"
    )
    obs_fill <- paste(obs_fill, obs_fill_mod)
  }

  obs_end <-"</Parameter>\r\n</CondProb>\r\n</ObsFunction>\r\n\n\n"

  obs <- paste(obs_header, obs_fill, obs_end)


  # reward ####
  rew_header <- paste0(
    "<RewardFunction>\r\n",
    "<Func>\r\n",
    "<Var>reward_agent</Var>\r\n",
    "<Parent> action_control eco_0  </Parent>\r\n",
    "<Parameter type=\"TBL\">\r\n"
  )

  rew_fill <- paste0(
    "<Entry>\r\n",
    "<Instance> - - </Instance>\r\n",
    "<ValueTable>", paste0(REW, collapse = " "), "</ValueTable>\r\n",
    "</Entry>\r\n"
  )


  rew_end <- "</Parameter>\r\n
    </Func>\r\n
    </RewardFunction>\r\n\n\n"

  rew <- paste(rew_header, rew_fill, rew_end)

  end <- "</pomdpx>"

  # paste everything ####
  a = paste(header, state_tr, obs, rew, end, sep = '')
  writeLines(a,FILE)

}
