#!/usr/bin/env nix-shell
#! nix-shell -p nushell nim llama-cpp wget
#! nix-shell -i nu

def main [--auto-restart (-r)] {
  if not ("./models/llama-2-7b-chat.Q4_K_M.gguf" | path exists) {
    mkdir ./models/
    print "Downloading AI model..."
    wget https://huggingface.co/TheBloke/Llama-2-7B-Chat-GGUF/resolve/main/llama-2-7b-chat.Q4_K_M.gguf -P ./models/
  }

  if not ("./main" | path exists) or (ls "./src/main.nim" | get modified) > (ls "./main" | get modified) {
    print "Compiling code..."
    do --ignore-errors=$auto_restart {
      nim compile -o=main ./src/main.nim
    }
  }
  
  print "Starting llama.cpp..."
  job spawn {llama-server -m ./models/llama-2-7b-chat.Q4_K_M.gguf -c 4096 --host 0.0.0.0 --port 4000}

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
