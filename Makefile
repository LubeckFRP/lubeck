
# Just some shortcuts to the Stack build

GHC	  :=	stack-ghc.yaml
GHCJS	:=	stack-ghc-js.yaml

SPEEDUP_BUILD_FLAGS := -j4 --fast

# Disable if profiling not desired
PROF_FLAGS := --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts"

ghc:
	stack --stack-yaml=$(GHC)   build $(PROF_FLAGS) $(SPEEDUP_BUILD_FLAGS)
ghcjs:
	stack --stack-yaml=$(GHCJS) build $(PROF_FLAGS) $(SPEEDUP_BUILD_FLAGS)
test-run:
	stack --stack-yaml=$(GHC)   test $(PROF_FLAGS) $(SPEEDUP_BUILD_FLAGS)
test-run-watch:
	stack --stack-yaml=$(GHC)   test $(PROF_FLAGS) --file-watch $(SPEEDUP_BUILD_FLAGS)
test-generate:
	stack --stack-yaml=$(GHC)   test --test-arguments --generate $(PROF_FLAGS) $(SPEEDUP_BUILD_FLAGS)
test-report:
	stack --stack-yaml=$(GHC)   test --test-arguments --report $(PROF_FLAGS) $(SPEEDUP_BUILD_FLAGS)

ghc-prof:
	stack --stack-yaml=$(GHC) build $(PROF_FLAGS) $(SPEEDUP_BUILD_FLAGS)


		# && stack exec bench-ad-dashboard-calc -- config.json.local +RTS -p
