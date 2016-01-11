(use snowy)

(http-listen
 (lambda (req res)
   (update-response res
     code: 200
     body: "Hello, world!\n")))
