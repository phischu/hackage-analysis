START package=node(*)
MATCH package-[:VERSION]->version1,
      package-[:VERSION]->version2,
      version1-[:VARIANT]->()-[:MODULE]->module1,
      version2-[:VARIANT]->()-[:MODULE]->module2
WHERE version1-[:NEXTVERSION]->version2
AND   module1.modulename = module2.modulename
RETURN package.packagename,
       version1.versionname,
       version2.versionname,
       module1.modulename