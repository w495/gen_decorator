#!/bin/sh
# ---------------------------------------------------------------------------
    NAME="gen_decorator_examples"
    COOKIE=QKDFVRMXUFKAFWQFMAJA
    CONFIG=$NAME
# ---------------------------------------------------------------------------
    ACTION=$1
    PATH=$PATH:/usr/local/lib/
    export PATH


    IP=`ifconfig  | grep 'inet addr:'| grep -v '127.0.0.1'  --max-count=1 | \
        cut -d: -f2 | awk '{print $1}'`

    if [ "" = "$IP" ]; then
        IP="localhost"
    fi


    MAIN_NODE="$NAME@$IP"
    MAIN_APP=$NAME"_app"

    CTRL_NODE="$NAME`date +_nodeclt_%H_%M_%S_%N`b@$IP"
    ERL_ARGS="+K true +A 128 +P 1000000"
    ERL_MAX_ETS_TABLES=140000
    export ERL_MAX_ETS_TABLES

    OTHERBINPATH=`find ./../../  -path '*.git' -prune -o  -type d -name 'ebin' -printf '%h/%f '`
    LOCALLBINPATH=`find ./ebin -type d -printf '%h/%f '`
    BINPATH=${OTHERBINPATH}" "${LOCALLBINPATH}
    SESSIONDBPATH='"./priv/session-db/'$MAIN_NODE'/"'
# ---------------------------------------------------------------------------
    echo "
    INFO:
        MAIN_NODE:              $MAIN_NODE
        MAIN_APP:               $MAIN_APP
        CTRL_NODE:              $CTRL_NODE
        CONFIG:                 $CONFIG
        ERL_ARGS:               $ERL_ARGS
        ERL_MAX_ETS_TABLES:     $ERL_MAX_ETS_TABLES
        OTHERBINPATH:           $OTHERBINPATH
        LOCALLBINPATH:          $LOCALLBINPATH
        BINPATH:                $BINPATH
        SESSIONDBPATH:          $SESSIONDBPATH
    "

    erl \
        -pa $BINPATH \
        -boot start_sasl \
        -config ${CONFIG} \
        -name ${MAIN_NODE} \
        -setcookie ${COOKIE} \
        -s nodeclt_reloader \
        -s ${MAIN_APP} \
        ${ERL_ARGS} \
    "$@"
