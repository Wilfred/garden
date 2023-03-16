Garden is exploring interactive program development, so the REPL
features are crucial.

## Evaluate Expression

In any context, the user should be able to evaluate small expressions
using the current locals.

## Define Function

In any context, the user should be able to define a named global
function.

## Redefine Function

The user should be able to redefine a function, even if it's in
another namespace, or if it's currently in the call stack.

## Forget

The user should be able to remove locals, globals or data types from
the namespace. This should be a REPL feature rather than
user-denotable syntax.

## Redefine Data Type

The user should be able to redefine data types interactively, and any
existing values of that type should be handled gracefully
(e.g. changed to error sentinel if no corresponding item in the new
data type).

## Continue On Error

The user should be able to redefine definitions or mutate state on
error and continue.

The effective source of the redefined definitions should be saved so
the user can update their code if they wish.
