# Diagram TUI

<!--toc:start-->
- [Diagram TUI](#diagram-tui)
  - [Example](#example)
  - [Usage](#usage)
    - [Creating](#creating)
    - [Exporting](#exporting)
<!--toc:end-->

A terminal user interface for quickly creating small flow-diagrams.

## Example

```text

            ┌──────────────┐   ┌────────┐            ┌──────┐
            │              │   │        │            │      │
   Start───►│ Do something │──►│ Happy? │─────Yes───►│ Done │
            │              │   │        │            │      │
            └──────────────┘   └────────┘            └──────┘
                    ▲               │
                    │               │
                    │               │
                    │               │
                    ╰───────────────No

```

The same diagram can also be displayed in a smaller format.

```text

            ┌──────────────┐   ┌────────┐            ┌──────┐
   Start───►│ Do something │──►│ Happy? │─────Yes───►│ Done │
            └──────────────┘   └────────┘            └──────┘
                    ▲               │
                    │               │
                    │               │
                    ╰───────────────No

```

## Usage

In a terminal, run `diagram-tui`. You will get a blank canvas
on which you can create diagrams using your keyboard. The control
scheme is inspired by Vim. A help box will show what commands are
available at any given time. Toggle it with the '?' key.

### Creating

When you first start out, try pressing B. This adds a box and
places you in "Insert mode", which lets you write text in the
box.

```text
 ┌────────────────┐
 │                │
 │ Insert text... │
 │                │
 └────────────────┘
```

You can then extend a connection out of the box by holding
Shift and one of H, J, K or L.

```
 ┌────────┐
 │        │
 │ My box │
 │        │
 └────────┘
      │
      │
      │
      │
```

You can also add labels and connect things to each other,
to create more complex diagrams. See the help box for hints.

> Note: Make sure your terminal window is large enough to
> show the whole help box, as some hints may be cropped if
> the window is not high enough.

### Exporting

When you are happy with your diagram, you can press Ctrl+C to
copy it to the clipboard. To quit, press Q while in "Normal
mode". If you are stuck, you can usually get to normal mode with
Esc from any other mode.
