module Database.ArangoDB.Datbase where

data DatabaseError
  = DbErrInvalidRequest
  | DbErrSystemFailure
  | DbErrAlreadyExists

data Database
