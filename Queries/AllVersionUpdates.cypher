START package=node(*) // Start with any node
MATCH package-[:VERSION]->version1, // one version of a package
      package-[:VERSION]->version2, // a different version of the same package
      //(cypher semantics are strange so version1 and version2 are never equal)
      version1-[:NEXTVERSION]->version2 // version1 is followed by version2
RETURN package.packagename,version1.versionname,version2.versionname // return the interesting parts