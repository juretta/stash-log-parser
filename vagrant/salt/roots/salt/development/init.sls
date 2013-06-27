development-packages-haskell:
    pkg:
        - installed
        - names:
            - ghc
            - cabal-install
            - build-essential

cabal-update:
  cmd.run:
    - name: cabal update
    - cwd: /home/vagrant
    - user: vagrant
    - group: vagrant
    - unless: ls /home/vagrant/.cabal/packages/hackage.haskell.org 2&>1 /dev/null
    - shell: /bin/bash

libbz2-dev:
  pkg.installed

vim:
  pkg.installed

git:
  pkg.installed

ruby1.9.1-full:
  pkg.installed

rake:
  pkg.installed

sysinfo:
  gem.installed
