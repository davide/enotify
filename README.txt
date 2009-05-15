enotify - erlang's filesystem monitor wannabe

This is kind of a port of jnotify to erlang...
The Windows parts are taken from jnotify-0.91 (LGPL) and the linux parts
are taken from erlang-inotify-0.2 (copyleft).

I haven't worked out which interface to offer yet since windows and linux differ in
the features they offer:
 - windows has recursive monitoring of directories (we can add that since inotify can detect
   the creation of sub-directories);
 - inotify offers a bunch of monitors that are not available on windows
 
I'd prefer to move away from a common denominator approach (the one taken by
jnotify) so that linux users can get the most out of inotify has to offer. To get there I'm
thinking of enabling the introspection of the app's available features/"mask values", so that
a user can check what's available just asking the server (VS trial and error, or just error).

The code ISN'T PRODUCTION READY, the windows part at least. :)
The binding with the windows part is done via erl_interface but I'd like to change it to
use ei (if I ever get it to work on windows*).
*http://www.trapexit.org/forum/viewtopic.php?t=15186&sid=494ae6d415bdea5025cc0f232d51aadb

One cool thing I'd like to add is the possibility to specify wildcards and have enotify
turn that into the necessary file/directory watches!
One useful example for development:
	enotify:monitor("c:/projects/*/src/*.erl", file_changed) -> notification -> rebuild -> test -> report

There are endless possibilities for something like this. ;)

Enjoy and fork away! :)