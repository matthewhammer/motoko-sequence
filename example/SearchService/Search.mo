import Buffer "mo:base/Buffer";
import Char "mo:base/Char";
import Debug "mo:base/Debug";
import Hash "mo:base/Hash";
import Iter "mo:base/Iter";
import Nat "mo:base/Nat";
import Result "mo:base/Result";
import Text "mo:base/Text";
import Trie "mo:base/Trie";

import Db "mo:crud/Database";

import Sequence "../../src/Sequence";
import Texts "../../src/Text";
import TrieExt "../../src/TrieExt";
import IterExt "../../src/IterExt";

module {

  // uniquely identify a TextFile within the file database
  public type FileId = Nat;

  // sequence of text
  public type TextSeq = Sequence.Sequence<Text>;

  public type TextFile = {
    name : ?Text;
    meta : ?Text; // e.g., CanCan stores video ID here (or whatever helps render search results visually)
    var content : TextSeq; // e.g., CanCan stores some video-related text here, like a comment, explicit hashtag(s), etc
  };

  // Metric measures things about one file and one word.
  // (both already known from context).
  // at most one Metric each word-file relationship.
  public type Metric = {
    count : Nat; // how many times does the word appear?
    first : Nat; // where does it first appear?
    last : Nat;  // where does it last appear?
  };

  type FileMetric = {
    file : FileId;
    metric : Metric;
  };

  public type SearchResult = {
    file : FileId;
    meta : ?Text;
    metric : ?Metric; // (metric is null for no search query)
  };

  public type SearchResults = Sequence.Sequence<SearchResult>;

  // stores a database of text files, with keyword search
  public class Search() {

    var count = 0;

    let append = Sequence.defaultAppend();

    // database of text
    let db = Db.Database<Nat, TextFile>(
      func (_, last) {
        switch last {
        case null 0;
        case (?last) { last + 1 };
        }},
      Nat.equal,
      #hash(Hash.hash),
    );

    // wordFileMetric stores at most one Metric each word-file relationship
    var wordFileMetric : Trie.Trie2D<Text, Nat, Metric> = Trie.empty();

    // searchIndex relates a word with all its word-file relationships
    var searchIndex : Trie.Trie<Text, SearchResults> = Trie.empty();

    /// create a new text sequence
    public func create(n : ?Text, m : ?Text, c : Text) : FileId {
      db.create({ name = n; meta = m; var content = Sequence.make(c)})
    };

    /// delete a text sequence
    public func delete(id : FileId) : Bool {
      switch (db.delete(id)) {
      case (#ok(_)) true;
      case (#err(_)) false;
      }
    };

    /// append to an existing text sequence
    public func addText(id : FileId, t : Text) : Bool {
      switch (db.read(id)) {
      case (#ok(file)) {
             file.content := append(file.content, Sequence.make(t));
             true
           };
      case (#err(e)) { false };
      }
    };


    /// read a text sequence
    public func readText(id : FileId) : ?Text {
      switch (db.read(id)) {
      case (#ok(file)) { ?Texts.toText(file.content) };
      case (#err(_)) { null };
      }
    };

    /// read a text sequence slice
    public func readSlice(id : FileId, pos : Nat, size : Nat) : ?Text {
      switch (db.read(id)) {
      case (#ok(file)) { ?Texts.toText(Sequence.slice<Text>(file.content, pos, size).1) };
      case (#err(_)) { null };
      }
    };

    public func readMeta (file : FileId) : ?Text {
      switch (db.read(file)) {
      case (#ok(file)) { file.meta };
      case (#err(_)) { null };
      }
    };

    func refreshIndex() {

      func updateFileWordMetric(id : FileId, pos : Nat, word : Text) {
        let wordKey = { key = word; hash = Text.hash(word) };
        let idKey = { key = id; hash = Hash.hash(id) };
        switch (Trie.find(wordFileMetric, wordKey, Text.equal)) {
        case null {
               wordFileMetric
                 := Trie.put2D<Text, Nat, Metric>(
                   wordFileMetric, wordKey, Text.equal, idKey, Nat.equal,
                   { count = 1;
                     first = pos;
                     last = pos; }
                 )
             };
        case (?files) {
               switch (Trie.find(files, idKey, Nat.equal)) {
               case null {
                      wordFileMetric := Trie.put2D(
                        wordFileMetric, wordKey, Text.equal, idKey, Nat.equal,
                        { count = 1;
                          first = pos;
                          last = pos;
                        }
                      )
                    };
               case (?metric) {
                      wordFileMetric := Trie.put2D(
                        wordFileMetric, wordKey, Text.equal, idKey, Nat.equal,
                        { count = metric.count + 1;
                          first = Nat.min(metric.first, pos);
                          last = Nat.max(metric.last, pos);
                        }
                      )
                    }
               }
             };
        }
      };

      func indexFile(id : FileId, f : TextFile) {
        let words = Texts.tokens(f.content, #predicate(Char.isWhitespace));
        for (word in Sequence.iter(words, #fwd)) {
          updateFileWordMetric(id, word.pos, word.text)
        };
      };
      wordFileMetric := Trie.empty();
      for (file in db.entries()) {
        indexFile(file.0, file.1)
      };

      searchIndex :=
        Trie.mapFilter<Text, Trie.Trie<Nat, Metric>, SearchResults>(
          wordFileMetric,
          func (word : Text, metrics : Trie.Trie<Nat, Metric>) : ?SearchResults {
            ?Sequence.fromTrie<Nat, Metric, SearchResult>(
              metrics,
              func (file_, metric_) : SearchResult {
                { file = file_.key;
                  meta = readMeta(file_.key);
                  metric = ?metric_;
                }
              }
            )
          });
    };

    public func search(q : ?Text, maxResults : Nat) : [SearchResult] {
      refreshIndex();
      switch q {
        case (?queryy) {
               let results =
                 // to do -- sort by a requested metric (store in unsorted form, unbiased toward a metric)
                 Sequence.iter<SearchResult>(
                   switch (Trie.find(searchIndex,
                                     { key = queryy ;
                                       hash = Text.hash(queryy) },
                                     Text.equal)) {
               case null #empty;
               case (?t) t;
                   },
                   #fwd
                 );
               Iter.toArray(IterExt.max(results, maxResults))
             };
        case null {
               // No query text.
               // So, dump all wordFileMetric that we have stored
               // (to do : filter by a time window; order by a timestamp)
               let dump = Buffer.Buffer<SearchResult>(0);
               for ((fileId, file_) in db.entries()) {
                 dump.add({ file = fileId ;
                            meta = file_.meta ;
                            metric = null })
               };
               dump.toArray()
             }
      }
    };
  };

}
