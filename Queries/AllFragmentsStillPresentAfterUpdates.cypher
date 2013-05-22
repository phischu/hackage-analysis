START package=node(*)
MATCH package-[:VERSION]->version1,
      package-[:VERSION]->version2,
      version1-[:VARIANT]->()-[:MODULE]->module1,
      version2-[:VARIANT]->()-[:MODULE]->module2,
      module1-[:FRAGMENT]->fragment1,
      module2-[:FRAGMENT]->fragment2
WHERE version1-[:NEXTVERSION]->version2
AND   module1.modulename = module2.modulename
AND   fragment1.functionname = fragment2.functionname
RETURN package.packagename,
       version1.versionname,
       version2.versionname,
       module1.modulename,
       fragment1.functionname