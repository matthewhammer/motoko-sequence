import Trie "mo:base/Trie";
import Iter "mo:base/Iter";
import List "mo:base/List";

module {
  /// Returns an [`Iter`](Iter.html#type.Iter) over the entries.
  /// 
  /// Each iterator gets a _persistent view_ of the mapping, independent of concurrent updates to the iterated map.
  public func entries<K, V>(trie : Trie.Trie<K, V>) : Iter.Iter<(K,V)> = object {
    var stack = ?(trie, null) : List.List<Trie.Trie<K,V>>;
    public func next() : ?(K,V) {
      switch stack {
      case null { null };
      case (?(trie, stack2)) {
             switch trie {
             case (#empty) {
                    stack := stack2;
                    next()
                  };
             case (#leaf({keyvals=null})) {
                    stack := stack2;
                    next()
                  };
             case (#leaf({size=c; keyvals=?((k,v),kvs)})) {
                    stack := ?(#leaf({size=c-1; keyvals=kvs}), stack2);
                    ?(k.key, v)
                  };
             case (#branch(br)) {
                    stack := ?(br.left, ?(br.right, stack2));
                    next()
                  };
             }
           }
      }
    }
  };
}
