# Snowy

![Snowy Owl](https://raw.githubusercontent.com/caolan/snowy/master/doc/snowy.png)

A more [node][nodejs]-like alternative to [spiffy][spiffy].

Implements a HTTP server for [CHICKEN Scheme][chicken] based on Joyent's http-parser C library (used in Node.js). Borrows some ideas and code from the fantastic [intarweb][intarweb] and [spiffy][spiffy] too. The API is fairly basic, the idea being that nicer response authoring and query string parsing etc. can be layered on top.

**NOTE:** This is a work-in-progress, feel free to experiment and give feedback, but don't put to real use yet!

## Example

```scheme
(use snowy)

(http-listen
 (lambda (req res)
   (update-response res
     code: 200
     body: "Hello, world!\n")))
```


[chicken]: http://call-cc.org
[intarweb]: http://wiki.call-cc.org/eggref/4/intarweb
[spiffy]: http://wiki.call-cc.org/eggref/4/spiffy
[nodejs]: http://nodejs.org
