# dev-server

## server

When server code changes, the server is rebuilt and restarted.

### config params

- serverExeName: name of the server executable as it appears in the cabal file. default = "server"

## client

When client code changes, the client is rebuilt and the browser is sent a notification via websocket
to reload. Add the following to the client initialization to enable the notification:

```
const socket = new WebSocket('ws://localhost:8082');

socket.addEventListener('message', (event) => {
    if (event.data === 'RELOAD') {
        location.reload();
    }
    return false;
});
```

### config params

- clientBuildDir: directory where build command will run. default = "./client"
- clientSrcDir: directory to watch for file changes. default = "./client/src"
- clientFileExtensions: file extensions of source files. changes to files that don't end with these extensions will be ignored. default = ["ts", "tsx"]
- webSocketPort: port on which to run the websocket. default = 8082

