
# NOTE that this is a FUNCTION that generates an app
# (so that the app is contains closures that know which server they belong to)

# App that returns empty-string body
# This is NOT designed to serve HTTP requests
# It is ONLY for establishing web socket connection with browser
nullApp <- function(serverID, port) {
    list(
        call = function(req) {
            list(status = 200L,
                 headers = list(
                     'Content-Type' = 'text/html'
                 ),
                 body = "")
        },
        onWSOpen = function(ws) {
            registerServerSocket(serverID, ws)
        }
    )
}
