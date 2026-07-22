#lang gorack

(package main
  (import "fmt")
  (import "log")
  (import "net/http")

  ;; Handler: writes a greeting and echoes the requested path.
  (defn helloHandler ([w http.ResponseWriter] [r (ptr http.Request)])
    (call fmt.Fprintf w
      "Hello from gorack! You requested: %s\n"
      r.URL.Path))

  ;; Optional health-check handler
  (defn healthHandler ([w http.ResponseWriter] [r (ptr http.Request)])
    (call fmt.Fprintf w "ok\n"))

  (defn main ()
    ;; Register handlers
    (call http.HandleFunc "/" helloHandler)
    (call http.HandleFunc "/healthz" healthHandler)

    ;; Start server on :8080 and log fatal errors
    (call log.Fatal
      (call http.ListenAndServe ":8080" nil)))
)
