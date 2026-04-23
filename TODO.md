# TODO list

## Next steps

1. **Define `Model.Selection` and `Selection.hs`** — replace the placeholder
   with a `Map`-backed newtype in `Model.hs`. Create a new `Selection.hs` module
   with `empty`, `selected`, and a single `toggle :: Selection -> Focus ->
   Selection` function that selects the focused mode for the focused monitor, or
   removes the selection if that mode was already selected.

2. **Make Focus navigation selection-aware** — `initialMode` should prefer the
   selected mode over the current mode when navigating to a monitor. `first`,
   `nextMonitor`, `previousMonitor` gain a `Selection` parameter. Update tests:
   existing cases pass `Model.empty`, new cases cover selection taking
   precedence.

3. **Wire ENTER in `Application.hs`** — on ENTER, if the focused mode is already
   selected for that monitor → `deselect`; otherwise → `select` (replacing any
   prior selection for that monitor).

4. **Render selections in `UI.hs`** — add a second attribute (e.g. cyan) for
   selected modes; pass `Selection` into `availableModeWidget` to highlight the
   selected mode independently of focus.

5. **Apply** — `WlrRandr.apply :: Selection -> IO ()` calls
   `wlr-randr --output <name> --mode <w>x<h>` for each monitor in the
   selection. Wire to `a` in `Application.hs`. Only monitors with an explicit
   selection are passed (refineable later).

## Further steps

- backend independent data structure for current configuration and status
  - as is (query result)
  - to be applied ()
- support multiple backends
  - sway (swaymsg, sway-ipc, sway-output, get_outputs)
  - wlr-randr

## UI

- move monitor to left or right (ctrl + <- / ->)
- align monitor with bottom/top of monitor to the left (ctrl + ^ / v)
