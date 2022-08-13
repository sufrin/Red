import Useful.PrefixMap

val trie = PrefixMap[String]()

trie.update("", "<empty>")
trie.update("foo", "foo")
trie.update("fox", "fox")
trie.update("foxglove", "foxglove")
trie.update("foxgl", "foxgl")
trie.update("fish", "fish")
trie.reverseUpdate("toof", "toof")
trie.show
for { (d, r) <- trie.iterator } println(s"$d->$r")
for { (d, r) <- trie.pathIterator } println(s"$d->$r")
trie.follow("foot")
trie.follow("fox")
trie.follow("foxglo")
trie.followBackwardsFrom(4, "toof")
trie.followBackwardsFrom(3, "oof")


