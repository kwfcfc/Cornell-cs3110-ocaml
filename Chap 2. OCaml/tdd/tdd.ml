type day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

let next_weekday d = 
  match d with
  | Monday -> Tuesday
  | Tuesday -> Wednesday
  | Wednesday -> Thursday
  | Thursday -> Friday
  | Friday -> Monday
  | Saturday -> Monday
  | Sunday -> Monday
