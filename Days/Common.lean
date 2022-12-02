namespace Days.Common


def sum [Add α] [Inhabited α] (items: List α) : α :=
  List.foldl (init:=Inhabited.default) (·+·) items
