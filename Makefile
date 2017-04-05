PROJECT = abci_counter
PROJECT_DESCRIPTION = An example counter application for ABCI
PROJECT_VERSION = 0.1.0

DEPS = abci_server
dep_abci_server = git https://github.com/KrzysiekJ/abci_server.git

# Whitespace to be used when creating files from templates.
SP = 4

include erlang.mk
