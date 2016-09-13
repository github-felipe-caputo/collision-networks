# collision-networks
An implemention of a RESTful web server in Haskell for manipulation of "collision networks".

## The Solution
To solve the problem of checking for collisions of nodes on a big network I have modeled the solution using an undirected graph data structure and graph search algorithms.

First the data structure. The input of the problem will always be collisions of nodes (in other words, edges), and they are alwyas undirected. To represent this data I decided to create a graph by using adjacency list. In other words something like this:

```shell
1 | [2,4,7,8]
2 | [1,10,4]
3 | [4]
4 | [1,2,3]
...
```

Which simply means "node 1 has edges to nodes 2,4,7,8", and so on. This approach takes O(E) space, as opposed to O(V^2) by using an adjacency matrix. So it's a pretty efficient representation. This adjacency list was implemented by using Haskell's Map data structure, where the 'key' is a node and the 'value' is a list of nodes the key is connected to.

And finally, to check the collision I have simply implemented a Depth First Search algorithm to run on the graph, slightly modified to work on function languages and to use a 'source' looking for a 'goal'. The idea is that, since all the collisions are presented in an adjacency list it's possible we will have a disconnected graphu such as:

```shell
1 | [2,3]
2 | [1]
3 | [1]
4 | [5]
5 | [4]
```

So we have actually two networks ([1,2,3] and [4,5]). So if we are looking to check the collision between nodes 1 and 3, we start on 1 and only walk on the graph [1,2,3] to look for it. DFS is one of the handful of search algorithms that can be implemented for graphs, but they all have relatively close efficiency (and they are all pretty good), so that was the one I chose. It was implemented with a list of visitedNodes represented by a Set for efficiency.

## Files

- `CollisionNetworks.hs`: Here is where all the graph algorithms are implemented.
- `Server.hs`: Here is where the web server is implemented, it uses `CollisionNetworks.ls`.
- `example.txt`: The example input file that was given to me.
- `test1.txt`, `test2.txt`, `test3.txt`: Other input files for tests.

## Requirements

The solution was implemented on Haskell, and by using the web framework library Scotty for the RESTful server. To run it you'll need:

- [Haskell Plataform](https://www.haskell.org/downloads): The GHC compiler plus Cabal for package management.
- [Scotty](https://hackage.haskell.org/package/scotty): A library for web frameworkds.
 
After installing GHC and Cabal just run the following commands to install scotty.

```shell
$ cabal update
$ cabal install scotty
```

Having done that the files can be compiled. Use the terminal to reach the folder with all the files in this repo and run

```shell
$ ghc -o Server Server.hs -Wall
$ ./Server
```

To compile and run the server.

## Usage
### The server

After compiling and running the `Server.hs` file, a web server will start running on `http://localhost:3000`, accessable by the browser normally. Ther server has 4 operations that can be run:

- `GET /printGraph` - `http://localhost:3000/printGraph`

This will print the current graph as a adjacency list. Since it's a GET command, can be seen by just using the browser.

- `GET /checkCollision/:x/:y` - `http://localhost:3000/checkCollision/0/31`

This will check if the nodes 'x' and 'y' collide, and will return True or False. It also checks if 'x' and 'y' are actually integers, if they aren't then an error message is shown. Since it's a GET command, can be seen by just using the browser.

- `POST /readFile/:fileName` - `http://localhost:3000/readFile/example.txt`

This will read the file 'fileName' that should be on the same folder as `Server` and add the collisions on the file to the graph. If the file doesn't exist, or is not formatted as expected (two integers separated by space per line) an error message will appear. If you read a file, then read another one then the collisions will be added on the graph.

Being a POST command, it can be used on the terminal with:

```shell
$ curl -X POST http://localhost:3000/readFile/example.txt
```

- `POST /addCollision/:x/:y` - `http://localhost:3000/addCollision/101/102`

This will add a new edge between nodes 'x' and 'y' in the graph. It also checks if 'x' and 'y' are actually integers, if they aren't then an error message is shown.

Being a POST command, it can be used on the terminal with:

```shell
$ curl -X POST http://localhost:3000/addCollision/101/102
```

### CollisionNetworks.hs

The file `CollisionNetworks.hs` can also be used without the server if you are interested in checking its functions. You can run it on `ghci` for instance, just load it and play around.

```shell
$ ghci
$ Prelude> :l CollisionNetworks.hs
$ *CollisionNetworks> addEdgeToGraph 1 2 (addEdgeToGraph 1 3 (addEdgeToGraph 3 4 Map.empty))
fromList [(1,[2,3]),(2,[1]),(3,[1,4]),(4,[3])]
...
```

## Example of running the code

1. Using the terminal go to the folder with all the files and run

```shell
$ ghc -o Server Server.hs -Wall
$ ./Server
```

This will start the server.

2. In the browser go to `http://localhost:3000` and you will see the message

`Collision Network Server`

3. Now go to `http://localhost:3000/printGraph` and you will see the message

`Graph is empty.`

4. Now open another terminal and run:

```shell
$ curl -X POST http://localhost:3000/readFile/example.txt
File read 'example.txt'.
```

5. In the browser go to `http://localhost:3000/printGraph` again and you'll see the graph.

6. Now in the browser try `http://localhost:3000/checkCollision/0/33` and you will get

`True`

Because there is a path between nodes 0 and 33.

7. On the otherminal you are running the server use the command `Ctrl+C` to end the excecution.