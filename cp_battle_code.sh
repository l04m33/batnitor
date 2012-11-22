#!/bin/bash

SG_SERVER_PATH=$1

if [ "$SG_SERVER_PATH" == "" ]; then
    echo "I need a path to the SG server code"
    exit 1
fi

cp -v $SG_SERVER_PATH/src/system/battle/ai.erl       ./deps/battle_code/src/
cp -v $SG_SERVER_PATH/src/system/battle/battle.erl   ./deps/battle_code/src/
cp -v $SG_SERVER_PATH/src/system/battle/skill.erl    ./deps/battle_code/src/
cp -v $SG_SERVER_PATH/src/data/data_ai.erl           ./deps/battle_code/src/
cp -v $SG_SERVER_PATH/src/data/data_skill.erl        ./deps/battle_code/src/
cp -v $SG_SERVER_PATH/src/data/data_skill_table.erl  ./deps/battle_code/src/

cp -v $SG_SERVER_PATH/include/*       ./deps/battle_code/include/

