#lang racket/base

(require web-server/http
         net/url
         net/head
         racket/contract
         racket/match
         racket/list
         racket/port)

(provide (contract-out
          [make-proxy-request
           (->* {(-> url? url?)}
                {#:preserve-host-header? any/c
                 #:convert-response-headers (-> (listof header?)
                                                (listof header?))
                 #:default-request-scheme (or/c 'https 'http)
                 #:via-name (and/c bytes? (not/c #""))}
                (-> request? response?))]
          ))

;; bindings->post-data and extract-post-data+headers
;; deal with generating multipart/form-data

(module+ main
  (require web-server/servlet-env
           web-server/managers/none)
  (serve/servlet
   (make-proxy-request
    #:preserve-host-header? #f
    (λ (u)
      (struct-copy
       url u
       [scheme "https"]
       [port 443]
       [host "xkcd.com"])))
   #:stateless? #t
   #:manager (create-none-manager #f)
   #:servlet-regexp #rx""
   #:servlet-path ""))


(define ((make-proxy-request
          convert-url
          #:preserve-host-header? [preserve-host-header? #t]
          #:convert-response-headers [convert-response-headers values]
          #:default-request-scheme [default-scheme 'http]
          #:via-name [via-name #"racket"])
         req)
  (define-values {post-data old-headers}
    ;; reconstructs any multipart data,
    ;; updating Content-Length header etc.
    (extract-post-data+headers req))
  (define new-headers
    (remove-hop-by-hop
     (update-forwarding-headers
      req
      #:default-request-scheme default-scheme
      (if preserve-host-header?
          old-headers
          (headers-remove #"Host" old-headers)))))
  (match-define-values
    {(regexp #px"(\\d\\d\\d)\\s+(.+)$"
             (list _
                   (app bytes->string/utf-8
                        (app string->number code))
                   message))
     response-headers-bytes
     body-in}
    (http-sendrecv/url
     (convert-url (request-prepare-url req))
     #:method (request-method req)
     #:headers (map header->bytes new-headers)
     #:data post-data))
  (define response-headers
   (convert-response-headers
    (update-via
     via-name
     (remove-hop-by-hop
      (for/fold ([response-headers
                  (listof-bytes->headers response-headers-bytes)])
                ([h (in-list '(#"Content-Length" #"Content-Encoding"))])
        (headers-remove h response-headers))))))
  (response/output
   #:code code
   #:message message
   #:seconds (current-seconds)
   #:mime-type #f
   #:headers response-headers
   (λ (out)
     (copy-port body-in out))))

;; request-prepare-url : request? -> url?
;; The request-uri field sometimes lacks a host and port.
;; This function tries to fill them in if needed.
(define (request-prepare-url req)
  (define req-url
    (request-uri req))
  (define host
    (or (url-host req-url)
        (match (headers-assq* #"Host" (request-headers/raw req))
          [(header _ (pregexp #px"^(.+):(\\d+)$"
                              (list _ (app bytes->string/utf-8 host) _)))
           ;; drop the port, if present
           host]
          [(header _ host)
           host]
          [#f
           (request-host-ip req)])))
  (struct-copy
   url req-url
   [host host]
   [port (or (url-port req-url)
             (request-host-port req))]))

;; extract-post-data+headers : request?
;;   -> (values (or/c #f bytes?) (listof header?))
(define (extract-post-data+headers req)
  (let* ([hs (request-headers/raw req)]
         [post-data (request-post-data/raw req)]
         [bindings (request-bindings/raw req)]
         [multipart-data? (and (regexp-match? #rx"(?i:POST)" (request-method req))
                               (not post-data)
                               (not (null? bindings))
                               (bindings->post-data bindings))])
    (cond
      [multipart-data?
       (values
        post-data
        (list* (header #"Content-Length"
                       (string->bytes/utf-8
                        (number->string
                         (bytes-length multipart-data?))))
               (header #"Content-Type"
                       #"multipart/form-data; boundary=_myboundary")
               (headers-remove
                #"Content-Type"
                (headers-remove #"Content-Length" hs))))]
      [else
       (values post-data hs)])))

;; update-forwarding-headers :
;; request? (listof header?) [##:default-request-scheme (or/c 'https 'http)]
;;   -> (listof header?)
(define (update-forwarding-headers req hs
                                   #:default-request-scheme [default-scheme 'http])
  (let* ([prepared-url (request-prepare-url req)]
         [host (string->bytes/utf-8 (url-host prepared-url))]
         [real-IP-bytes
          (string->bytes/utf-8 (request-client-ip req))]
         [external-protocol
          (cond
            [(url-scheme prepared-url)
             => string->bytes/utf-8]
            [(eq? 'https default-scheme)
             #"https"]
            [else
             #"http"])]
         [hs (for/fold ([hs hs])
                       ([h (in-list '(#"X-Forwarded-Host"
                                      #"X-Forwarded-Server"
                                      #"X-Real-IP"
                                      #"X-Forwarded-Proto"))])
               (headers-remove h hs))]
         [hs (list* (header #"X-Forwarded-Host" host)
                    (header #"X-Forwarded-Server" host)
                    (header #"X-Real-IP" real-IP-bytes)
                    (header #"X-Forwarded-Proto" external-protocol)
                    hs)])
    (cons (header #"X-Forwarded-For"
                  (match (headers-assq* #"X-Forwarded-For" hs)
                    [(header _ orig)
                     (bytes-append orig #", " real-IP-bytes)]
                    [_
                     real-IP-bytes]))
          (headers-remove #"X-Forwarded-For" hs))))
                  
;; header->bytes : header? -> bytes?
(define header->bytes
  (match-lambda
    [(header field val)
     ;; really should document that these should have no \r\n
     (bytes-append field #": " val)]))

;; listof-bytes->headers : (listof bytes?) -> (listof header?)
(define (listof-bytes->headers l-bytes)
  (for*/list ([bs (in-list l-bytes)]
              [pr (in-list (extract-all-fields bs))])
    (match-define (cons field val) pr)
    (header field val)))
    
;; remove-hop-by-hop : (listof header?) -> (listof header?)
(define (remove-hop-by-hop l-headers)
  (define hop-by-hop
    '(#"Connection"
      #"TE" #"Transfer-Encoding" #"Keep-Alive"
      #"Proxy-Authorization"
      #"Proxy-Authentication"
      #"Trailer" #"Upgrade"))
  (define to-remove
    (match (headers-assq* #"Connection" l-headers)
      [#f hop-by-hop]
      [(header _ val)
       (remove-duplicates
        (append (regexp-split #px",\\s*" val)
                hop-by-hop))]))
  (for/fold ([l-headers l-headers])
            ([name (in-list to-remove)])
    (headers-remove name l-headers)))

;; update-via : bytes?(listof header?) -> (listof header?)
(define (update-via via-name l-headers)
  (define 1.1via-name (bytes-append #"1.1 " via-name))
  (match (headers-assq* #"Via" l-headers)
    [#f
     (cons (header #"Via" 1.1via-name)
           l-headers)]
    [(and old-via (header _ val))
     (cons (header #"Via" (bytes-append val #", " 1.1via-name))
           (remove old-via l-headers))]))
  
;; headers-remove : bytes? (listof header?) -> (listof header?)
(define (headers-remove name l-headers)
  (let loop ([l-headers l-headers])
    (cond
      [(headers-assq* name l-headers)
       => (λ (h) (loop (remove h l-headers)))]
      [else l-headers])))

;; bindings->post-data : (listof binding?) -> bytes?
(define (bindings->post-data bindings)
  (bytes-append
   (for/fold ([so-far #""])
             ([binding (in-list bindings)])
     ;; It would be simple if we could just use
     ;;   Content-Transfer-Encoding: quoted-printable
     ;; for everything, and RFC 7578 has an example of
     ;; doing just that in section 4.5.
     ;; Unfortunately, it goes on to say in section 4.7
     ;; that Content-Transfer-Encoding is deprecated,
     ;; and at least some servers in the wild fail to understand it.
     (match binding
       [(binding:form id value)
        (bytes-append
         so-far
         #"\r\n--=_myboundary\r\n"
         #"Content-Disposition: form-data; name=\"" id #"\"\r\n"
         #"\r\n"
         value)]
       [(binding:file id filename headers content)
        (bytes-append
         so-far
         #"\r\n--=_myboundary\r\n"
         ;; Content-Disposition is included in the headers
         (apply bytes-append
                (map (match-lambda
                       [(header name val)
                        (bytes-append name #": " val #"\r\n")])
                     (headers-remove #"Content-Transfer-Encoding"
                                     headers)))
         #"\r\n"
         content)]))
   #"\r\n--=_myboundary--\r\n"))
