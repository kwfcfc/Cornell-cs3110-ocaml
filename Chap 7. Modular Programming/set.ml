module InsensitiveString = struct
  type t = string

  let to_lower s = String.lowercase_ascii s

  let compare s1 s2 =
    compare (String.lowercase_ascii s1) (String.lowercase_ascii s2)
end

module InsensitiveSet = Stdlib.Set.Make (InsensitiveString)

type set = InsensitiveSet.t
