betaServer <- function(data) {
  force(data)
  function(input,output,session) {
    flowServer("flow",data)
    originServer("origin",data)
    destinationServer("destination",data)
    odServer("od",data)
  }
}

