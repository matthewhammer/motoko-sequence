let Package = { name : Text, version : Text, repo : Text, dependencies : List Text }

[ { name = "base"
  , repo = "https://github.com/dfinity/motoko-base"
  , version = "moc-0.8.6"
  , dependencies = [] : List Text
  }
]
