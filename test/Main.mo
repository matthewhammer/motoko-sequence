import Sequence "../src/Sequence";

import Debug "mo:base/Debug";

actor {

  public func selfTest() {
    Debug.print "hello"
  };

  public func doNextCall() : async Bool {
    false
  };
}
