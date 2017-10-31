# algegraph

while true; do inotifywait -q src/Main.hs; clear; stack build && stack exec algegraph && dot -Tsvg test > output.svg; sleep 5; done
