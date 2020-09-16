import Debug "mo:base/Debug";
import Hash "mo:base/Hash";
import Nat "mo:base/Nat";
import Char "mo:base/Char";
import Order "mo:base/Order";
import Result "mo:base/Result";
import Text "mo:base/Text";
import Iter "mo:base/Iter";

import Trie "mo:base/Trie";

import Db "mo:crud/Database";

import Sequence "Sequence";
import Texts "Text";

module {

  // uniquely identify a TextFile within the file database
  public type FileId = Nat;

  // sequence of text
  public type TextSeq = Sequence.Sequence<Text>;

  public type TextFile = {
    name : ?Text; // perhaps un-needed?
    meta : ?Text; // e.g., CanCan stores video ID here (or whatever helps render search results visually)
    var content : TextSeq; // e.g., CanCan stores some video-related text here, like a comment, explicit hashtag(s), etc
  };

  // metrics that relate one file and one word (both already known from context)
  public type FileWord = {
    count : Nat; // how many times does the word appear?
    first : Nat; // where does it first appear?
    last : Nat;  // where does it last appear?
  };

  // Like TextFile, SearchResult contains name and meta;
  // instead of full content, uses only the position of search result in that content
  public type SearchResult = {
    file : FileId;
    name : ?Text;
    meta : ?Text;
    metric : FileWord;
  };

  class BigSearch() {

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

    // fileWords stores pairwise word-file relationships
    var fileWords : Trie.Trie2D<Text, Nat, FileWord> = Trie.empty();

    // searchIndex relates a word with all its word-file relationships
    var searchIndex : Trie.Trie<Text, Sequence.Sequence<SearchResult>> = Trie.empty();

    /// create a new text sequence
    public func create(n : ?Text, m : ?Text, c : Text) : async FileId {
      db.create({ name = n; meta = m; var content = Sequence.make(c)})
    };

    /// delete a text sequence
    public func delete(id : FileId) : async Bool {
      switch (db.delete(id)) {
      case (#ok(_)) true;
      case (#err(_)) false;
      }
    };

    /// append to an existing text sequence
    public func addText(id : FileId, t : Text) : async Bool {
      switch (db.read(id)) {
      case (#ok(file)) {
             file.content := append(file.content, Sequence.make(t));
             true
           };
      case (#err(e)) { false };
      }
    };


    /// read a text sequence
    public func readText(id : FileId) : async ?Text {
      switch (db.read(id)) {
      case (#ok(file)) { ?Texts.toText(file.content) };
      case (#err(_)) { null };
      }
    };

    /// read a text sequence slice
    public func readSlice(id : FileId, pos : Nat, size : Nat) : async ?Text {
      switch (db.read(id)) {
      case (#ok(file)) { ?Texts.toText(file.content) }; // to do -- slice!
      case (#err(_)) { null };
      }
    };

    func refreshIndex() {
      func indexFile(f : TextFile) {
        let words = Texts.tokens(f.content, #predicate(Char.isWhitespace));
        for (word in Sequence.iter(words, #fwd)) {
          // to do
        };
      };
      for (file in db.entries()) {
        indexFile(file.1)
      };

    };

    public func search(q : Text, maxResults : Nat) : async [SearchResult] {
      refreshIndex();
      let results =
        Sequence.iter(
          switch (Trie.find(searchIndex,
                           { key = q ;
                             hash = Text.hash(q) },
                           Text.equal)) {
            case null #empty;
            case (?t) t;
          },
          #fwd
        );
      Iter.toArray(results)
    };

  }
}
