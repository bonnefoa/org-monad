module OrgMonad.Redmine.Redmine
  where

import OrgMonad.Type
import OrgMonad.Redmine.Type

fetchIssues :: OrgMonadR (IO [RedmineIssue])

