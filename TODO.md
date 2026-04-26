# TODO list

## Next steps

1. ~~**Define `Model.Selection` and `Selection.hs`**~~ ✓

2. **Render selections in `UI.hs`**
   - Add `aSelected :: Attr.AttrName` (cyan) alongside `aFocus` and register it
     in `attributes`.
   - Thread `Model.Selection` down the call chain:
     `monitorListWidget` extracts `Model.selection model` and passes it to
     `monitorWidget`; `monitorWidget` passes it to `availableModesWidget`;
     `availableModesWidget` passes it to `availableModeWidget`.
   - In `availableModeWidget`, look up whether the mode is selected via
     `Selection.selected selection (Monitor.name monitor)`. Apply `aSelected`
     when selected, `aFocus` when focused; focus takes visual precedence when
     both apply (a selected+focused mode shows as focused).
   - No new test module needed — this is visual-only. Verify by loading fake
     monitors with `d` and checking that a hardcoded selection renders in cyan.

3. **Wire ENTER in `Application.hs`** — on ENTER, call `Selection.onSelect` to
   toggle the focused mode for the focused monitor. Update the `selection` in the
   model.

4. **Make Focus navigation selection-aware** — `initialMode` should prefer the
   selected mode over the current mode when navigating to a monitor. `first`,
   `nextMonitor`, `previousMonitor` gain a `Selection` parameter. Update tests:
   existing cases pass `Selection.empty`, new cases cover selection taking
   precedence.

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
