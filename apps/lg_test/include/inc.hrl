
-define(DEBUG(MSG, PARAMS),
        {lager:debug(MSG, PARAMS), stats_srv:log_msg()}).
