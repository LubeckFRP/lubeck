export PATH=/home/vagrant/.cabal/bin:/home/vagrant/.local/bin:/opt/happy/1.19.5/bin:/opt/alex/3.1.4/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.2/bin:$PATH

cd ~

git clone https://github.com/ghcjs/ghcjs.git || true

cd ghcjs

stack setup && stack install -j2

ghcjs-boot --dev

cd ~/lubeck

cabal update

cabal sandbox delete || true

cabal sandbox init

cabal sandbox add-source ../virtual-dom

cabal sandbox add-source ../ghcjs-ffiqq

cabal install --ghcjs
