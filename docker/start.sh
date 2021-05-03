/lesser_evil/bin/lesser_evil foreground &
/le_test_app/bin/le_test_app foreground &
while [ "pong" != "`/lesser_evil/bin/lesser_evil ping`" ]; do
    sleep 1
done
exec /mem.sh