#!/usr/bin/env nix-shell
#! nix-shell -p nushell nim ollama
#! nix-shell -i nu

def main [--auto-restart (-r)] {
  print "Starting ollama server..."
  job spawn {ollama serve}

  if not ("./main" | path exists) or (ls "./src/main.nim" | get modified) > (ls "./main" | get modified) {
    print "Compiling code..."
    do --ignore-errors=$auto_restart {
      nim compile -o=main ./src/main.nim
    }
  }

  print "Downloading AI model..."
  ollama pull llama3.2

  print "Starting app..."
  job spawn --tag "app" {./main -d 127.0.0.1:8080}

  if $auto_restart {
    job spawn {
      print "Watching ./src/ folder to recompile and restart the app when there are changes to the source code..."
      watch ./src --quiet {
        job list | where tag? == "app" | each {|job| job kill $job.id}
        let _ = job spawn --tag "app" {
          print "Recompiling code..."
          nim compile -o=./main ./src/main.nim o+e>| print

          print "Restarting app..."
          ./main -d 127.0.0.1:8080
        }
      }
    }
  }

  print "Press enter to exit"
  ^read
  job list | each {|job| job kill $job.id} | ignore
}
