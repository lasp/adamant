# Database

## Description

This directory contains two implementations of a database.

## Contents

* `database` - A generic Ada database package, meant to be used with static sized types. The database contains a double indexing system where the query index is used to lookup the database location in a secondary table. The benefit of this double lookup system is that the component is more space efficient if you have an ID space that is much larger than the maximum number of items expected to be stored in the database.
* `variable_database` - A generic Ada database whose underlying type is a byte array for storage. This should be used if the type stored on the database is of variable length. For efficiency, this database does not use a double lookup like the database package. The downside is that database entries for every possible lookup index must be instantiated at startup.
