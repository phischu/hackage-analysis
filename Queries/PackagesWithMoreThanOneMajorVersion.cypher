START package=node(*) // start with any node
MATCH package-[:VERSION]->version1,package-[:VERSION]->version2 // match a certain package with two of its versions
WHERE not(version1.majorversion = version2.majorversion) // only continue if both majore versions are different
RETURN distinct package.packagename       // return each package name only once
