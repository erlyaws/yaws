

ERL="/usr/local/bin/erl"
YTOP="/Users/klacke/yaws/erlyaws/trunk/yaws"

YAWS=$(YTOP)/bin/yaws

%.beam: %.erl
        "$(ERLC)" -Ddebug $<
