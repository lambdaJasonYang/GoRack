#lang gorack

(package main
  (import "fmt")
  (import "log")
  (import "net/http")

  (defn helloHandler
    (-> ([w http.ResponseWriter] [r (ptr http.Request)]) ())
    (fmt.Fprintf w
      "Hello from gorack! You requested: %s\n"
      r.URL.Path))

  (defn healthHandler
    (-> ([w http.ResponseWriter] [r (ptr http.Request)]) ())
    (fmt.Fprintf w "ok\n"))

  (defn main
    (-> () ())
    (http.HandleFunc "/" helloHandler)
    (http.HandleFunc "/healthz" healthHandler)
    (log.Fatal (http.ListenAndServe ":8080" nil))))
