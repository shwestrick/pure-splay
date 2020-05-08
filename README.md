# pure-splay

I wrote this code because I wanted to better understand the splaying algorithm,
but could not find a reasonably concise and simple implementation.

There are two reasonable ways to implement splay trees: bottom-up, and top-down.
Traditionally, the splaying algorithm is described in a bottom-up
manner, but the top-down algorithm is preferable in practice, because it does
not require parent pointers (or, in the purely functional setting, a
[zipper](https://en.wikipedia.org/wiki/Zipper_%28data_structure%29)).

This code currently only contains a bottom-up implementation, but I will
eventually add a top-down implementation too.

You can run it with SML/NJ like this:
```
$ sml splay.cm
- structure S = BottomUpSplay(IntKey);
- S.fromList [9,7,5,3,1,8,6,2,4];
```

## Trees and Keys

The code in this repository operates on binary search trees of the form
```
datatype tree = Empty | Node of tree * key * tree
```

Each node contains a key and pointers to the
left and right children in the tree, which recursively contain all keys
respectively smaller and larger than the key.

Details of keys are in [`keys/`](keys/). All we really need is a comparison
function for a total order. The signature is in [`KEY.sig`](keys/KEY.sig) and
[`IntKey.sml`](keys/IntKey.sml) implements integer keys.

## Bottom-up Splaying

This section describes the code in [`BottomUpSplay.sml`](BottomUpSplay.sml)

Bottom-up splay access (insert or lookup) operates in two phases.
First, we traverse the path to the desired node, and then we perform
rotations on the way back up, bringing the desired node to the root and
encouraging the accessed path to become more balanced. To implement this,
we have two functions, `path` and `splay`, for the two phases. The `path`
function returns an access path, and `splay` consumes an access path to
produce a tree.

Access paths are implemented by recording, at each step, whether the
next access is down-to-the-left or down-to-the-right. This produces a
list of **contexts**, where each context is a parent node with a hole.
```
datatype context = Left of key * tree | Right of tree * key
```

  1. `Left(k,R)`: the parent is `Node(_,k,R)`

                    parent -->   k
                                / \
              you are here --> *   R

  2. `Right(L,k)`: the parent is `Node(L,k,_)`

                                 k   <-- parent
                                / \
                               L   * <-- you are here

By pushing elements onto the context list in the order they were accessed,
the "front" of the list is always the most recently accessed element.
A **path** is then just a list of contexts together with the subtree whose
root is the targeted element. This is essentially the same thing
as a [zipper](https://en.wikipedia.org/wiki/Zipper_%28data_structure%29).

```
type path = tree * (context list)
```

For example, consider searching for `y` in the following tree.

           z
          / \         Resulting path:
         x   D          ( subtree,     context list )
        / \           = ( Node(B,y,C), [Right(A,x), Left(z,D)] )
       A   y
          / \
         B   C

With an access path in hand, we can splay it by performing the various
cases: zig-zig, zig-zag, etc. Each of the cases consumes two elements
off the end of the access path, except for the single zig/zag cases which
finish off an odd-length path. The `splay` function implements this by
accumulating two trees, a left and a right tree, which will contain all
elements smaller and larger (respectively) than the new root key.
