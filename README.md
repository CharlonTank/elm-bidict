# `Janiczek/elm-bidict`

**A dictionary data structure allowing for many-to-one, one-to-many and many-to-many relationships.**

## Goal

This library provides bidirectional and multi-value dictionary data structures that maintain reverse mappings automatically. This fork uses `SeqDict` from `lamdera/containers` instead of the standard `Dict`, which preserves insertion order and works with any type (not just `comparable` types).

These data structures give you all the relationship types you might need:

* one-to-one: `SeqDict`
* many-to-one: `BiDict` - multiple keys can map to the same value, with efficient reverse lookups
* one-to-many: `MultiDict` - one key can map to multiple values
* many-to-many: `MultiBiDict` - keys and values can have multiple mappings in both directions

The `many-to-*` variants allow you to ask for the reverse mapping (from values to keys) - see the `getReverse` functions.

### Many to one - BiDict

```elm
manyToOne : BiDict String Int
manyToOne =
    BiDict.empty
        |> BiDict.insert "A" 1
        |> BiDict.insert "B" 2
        |> BiDict.insert "C" 1
        |> BiDict.insert "D" 4

BiDict.getReverse 1 manyToOne
--> Set.fromList ["A", "C"]
```

### One to many - MultiDict

```elm
oneToMany : MultiDict String Int
oneToMany =
    MultiDict.empty
        |> MultiDict.insert "A" 1
        |> MultiDict.insert "B" 2
        |> MultiDict.insert "A" 3
        |> MultiDict.insert "D" 4

MultiDict.get "A" oneToMany
--> Set.fromList [1, 3]
```

### Many to many - MultiBiDict

```elm
manyToMany : MultiBiDict String Int
manyToMany =
    MultiBiDict.empty
        |> MultiBiDict.insert "A" 1
        |> MultiBiDict.insert "B" 2
        |> MultiBiDict.insert "C" 3
        |> MultiBiDict.insert "A" 2

MultiBiDict.get "A" manyToMany
--> Set.fromList [1, 2]

MultiBiDict.getReverse 2 manyToMany
--> Set.fromList ["A", "B"]
```
