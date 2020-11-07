# DiscordBot
This is just a small and fun bot for discord it supports the following commands:
 * `muteall` - Toggles server mute on everyone in your voice channel.
 * `followmute` - Toggles server mute on everyone in your voice channel when you toggle your self mute.
 * `spam @someone` - Spams DMs to the mentioned someone until they respond to the bot via dm(it might take a few more seconds to stop)
 * `replace something, replacement` - Send a message with something(which is a regex) replaced with the given replacement for all future messages.
 * `stop replacing something` - Stops sending messages with the given replacement.
There a few extra hidden commands.

## How to run
The easiest way is probably to replace the ${TOKEN} in application.conf with your bots token and do `sbt run`

To run this way scala and sbt have to be installed.
