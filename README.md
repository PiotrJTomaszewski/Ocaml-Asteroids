# Luźne notki:

- Jeśli w funkcji jakiś parametr jest określony jako `option`, np. `Sdl.event option` to znaczy po prostu, że wartość ta może być nullem (`None`). Żeby przekazać tam parametr, który jest "normalnego" "nie-nullowalnego" typu, trzeba go owinąć w `Some`, np. `Some (event)`.  