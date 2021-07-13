import Search "Search";
import Debug "mo:base/Debug";
import Result "mo:base/Result";

actor {
  public type FileId = Search.FileId;

  flexible var search_ = Search.Search();

  // do a keyword search, limited to at most maxResults
  public query func search(q : ?Text, maxResults : Nat) : async [Search.SearchResult] {
    search_.search(q, maxResults)
  };

  /// create a new text sequence
  public func create(n : ?Text, m : ?Text, c : Text) : async FileId {
    search_.create(n, m, c)
  };

  /// delete a text sequence
  public func delete(id : FileId) : async Bool {
    search_.delete(id)
  };

  /// append to an existing text sequence
  public func addText(id : FileId, t : Text) : async Bool {
    search_.addText(id, t)
  };

  /// read a text sequence
  public func readText(id : FileId) : async ?Text {
    search_.readText(id)
  };

  /// read a text sequence slice
  public func readSlice(id : FileId, pos : Nat, size : Nat) : async ?Text {
    search_.readSlice(id, pos, size)
  };
}
