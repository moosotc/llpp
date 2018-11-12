#!/bin/sh
# This wrapper provides automatic reloading on file modifications
# to the pdfviewer llpp via inotify.

# Prepare parameters.
cd "$(dirname "$1")"
pdf=$(basename "$1")
shift
passthrough="$@"

# Return with an error if the given file does not exist.
if [ ! -e "$pdf" ]; then
    echo "$pdf: No such file or directory"
    exit 1
fi

# Start llpp with the given file.
llpp "$pdf" $passthrough &

# Track the PID of the llpp instance.
pid_llpp=$!

# Kill the llpp instance if the shell script terminates.
trap "kill ${pid_llpp}" SIGINT SIGTERM SIGQUIT SIGKILL

# Watch for changes in the directory of the given file. This is necessary
# to recieve events after the file was deleted.
inotifywait -m -e close_write "$PWD" -q --format "%f"| while read file; do
    # Only refresh on events of the file in question and if this file exists.
    if [ "$file" = "$pdf" ] && [ -e "$pdf" ]; then
        kill -HUP $pid_llpp
    fi
done &

# If llpp terminates kill the inotifywait process.
wait $pid_llpp
pkill -P $$
cd - > /dev/null
