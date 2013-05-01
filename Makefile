# See LICENSE for licensing information.

PROJECT = bullet

# Dependencies.

DEPS = cowboy
dep_cowboy = https://github.com/extend/cowboy.git 0.8.0

# Standard targets.

include erlang.mk
