module OrgMonad.Utils
  where


mergeMaybe :: Maybe a -> Maybe a -> Maybe a
mergeMaybe first@(Just _) _ = first
mergeMaybe Nothing second = second


