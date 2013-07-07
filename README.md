# Sublime Moai-Debug

Sublime plug-in for interacting with the Moai .

## Features

- Automatically display scope variables and stack trace
- Debugging layout for stack and variables
- Click variable to inspect value


## Quick start

Use `Shift+f8` to show a list of actions:

- **Start debugger**: Start listening for an XDebug connection
- **Add/Remove Breakpoint**: A marker in the gutter shows the breakpoint

Once the XDebug connection is captured, using the same shortcut shows these
XDebug actions:

- **Continue**: Shows the debugger control menu (see below)
- **Stop debugger**: Stop listening
- **Add/remove breakpoint**
- **Status**: Shows the client status in the status bar

### Debugger control menu

- **Run**: run to the next breakpoint or end of the script
- **Step Over**: steps to the next statement, if there is a function call on the line from which the step_over is issued then the debugger engine will stop at the statement after the function call in the same scope as from where the command was issued
- **Step Out**: steps out of the current scope and breaks on the statement after returning from the current function
- **Step Into**: steps to the next statement, if there is a function call involved it will break on the first statement in that function
- **Stop**: stops script execution immediately
- **Detach**: stops interaction with debugger but allows script to finish

## Shortcut keys

- `Shift+f8`: Open XDebug quick panel
- `f8`: Open XDebug control quick panel when debugger is connected
- `Ctrl+f8`: Toggle breakpoint
- `Ctrl+Shift+f5`: Run to next breakpoint
- `Ctrl+Shift+f6`: Step over
- `Ctrl+Shift+f7`: Step into
- `Ctrl+Shift+f8`: Step out


## Gutter icon color

You can change the color of the gutter icons by adding the following scopes to your theme file: xdebug.breakpoint, xdebug.current. Icons from [Font Awesome](http://fortawesome.github.com/Font-Awesome/).
