package main

import "fmt"
import "log"
import "net/http"

func helloHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "Hello from gorack! You requested: %s\n", r.URL.Path)
}
func healthHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "ok\n")
}
func main() {
	http.HandleFunc("/", helloHandler)
	http.HandleFunc("/healthz", healthHandler)
	log.Fatal(http.ListenAndServe(":8080", nil))
}
