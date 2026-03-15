# TODO list

## Navigation (next steps)

### Monitor navigation (←/→)

1. Implement `nextMonitor` in `src/Focus.hs` — move focus to the next monitor; if already on the last monitor, keep focus (including mode) unchanged; otherwise set `focusedMode` to the current mode of the new monitor if it appears in its `available` list, else the first available mode
2. Implement `previousMonitor` in `src/Focus.hs` — same, but backwards; if already on the first monitor, keep focus unchanged
3. Add tests in `test/FocusSpec.hs` for both functions (no monitors, one monitor, boundary clamping, mode carry-over vs. fallback to first)

### Mode navigation (↑/↓)

4. Implement `nextMode` in `src/Focus.hs` — move focus to the next mode in `Monitor.available` for the focused monitor; clamp at last mode (no wrap); only moves if a monitor is focused
5. Implement `previousMode` in `src/Focus.hs` — same, but backwards; clamp at first mode
6. Wire `KUp`/`KDown` events in `src/Application.hs` (analogous to existing `KLeft`/`KRight` handlers)
7. Add tests in `test/FocusSpec.hs` for mode navigation

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
