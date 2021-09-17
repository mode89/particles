BUILD_DIR := .build
DOCKER_RUN := docker/run
DOCKER_BUILD := docker/build
PARTICLES3_ALL_JS := $(BUILD_DIR)/dist/build/js-ghcjs/ghcjs-8.10.7/$\
					 particles-0.1.0.0/x/particles3/build/particles3/$\
					 particles3.jsexe/all.js

export HASKELL_DOCKER_CABAL_DIR_VOLUME := $(BUILD_DIR)/.cabal

$(PARTICLES3_ALL_JS): $(BUILD_DIR)/.cabal/packages/hackage.haskell.org
	$(DOCKER_RUN) cabal --ghcjs --builddir=$(BUILD_DIR)/dist v2-build particles3

$(BUILD_DIR)/.cabal/packages/hackage.haskell.org: $(BUILD_DIR)/docker-image
	$(DOCKER_RUN) cabal update
	$(DOCKER_RUN) cabal install happy

$(BUILD_DIR)/docker-image: | $(BUILD_DIR)
	$(DOCKER_BUILD)
	touch $(BUILD_DIR)/docker-image

$(BUILD_DIR):
	mkdir $(BUILD_DIR)
