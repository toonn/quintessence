module Syntax where

type Id = String

data Module = Module Id [Id] Body

data Body = Body [Impdecl] [Topdecl]

data Impdecl = Impdecl Id Impspec
             | Impdeclq Id Id Impspec

data Impspec = Impspec [Import]
             | Hiding [Import]

data Import = ImportVar Id
            | ImportTyCon Id [Id]
            | ImportTyCls Id [Id]

data Topdecl = TypeDecl Id Type
             | DataDecl Context Id [Constr] [Id]
             | NewTypeDecl Context Id Constr [Id]
             | ClassDecl

