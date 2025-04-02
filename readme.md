# brossa-perf

## Deploying

Just pull and install.

    cd brossa-perf-repo
    git pull && cabal install --overwrite-policy=always
    systemctl restart brossa-perf.service
