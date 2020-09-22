import Search "../src/Search";
import Debug "mo:base/Debug";
import Result "mo:base/Result";

actor {
  public type FileId = Search.FileId;

  flexible var bigSearch = Search.BigSearch();

  // do a keyword search, limited to at most maxResults
  public query func search(q : ?Text, maxResults : Nat) : async [Search.SearchResult] {
    bigSearch.search(q, maxResults)
  };

  /// create a new text sequence
  public func create(n : ?Text, m : ?Text, c : Text) : async FileId {
    bigSearch.create(n, m, c)
  };

  /// delete a text sequence
  public func delete(id : FileId) : async Bool {
    bigSearch.delete(id)
  };

  /// append to an existing text sequence
  public func addText(id : FileId, t : Text) : async Bool {
    bigSearch.addText(id, t)
  };

  /// read a text sequence
  public func readText(id : FileId) : async ?Text {
    bigSearch.readText(id)
  };

  /// read a text sequence slice
  public func readSlice(id : FileId, pos : Nat, size : Nat) : async ?Text {
    bigSearch.readSlice(id, pos, size)
  };

  public func selfTest() {
    // to do?
    Debug.print "hello"
  };

  public func doNextCall() : async Bool {
    // to do -- for testing with BigTest
    false
  };
}
