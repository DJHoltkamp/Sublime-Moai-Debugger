import sublime
import sublime_plugin
import os
import socket
import base64
import threading
import types
import json
import webbrowser
import subprocess
import sys
import time
from xml.dom.minidom import parseString


moaidebug_current = None
original_layout = None
debug_view = None
protocol = None
buffers = {}
breakpoint_icon = 'breakpoint.png'
current_icon = 'current.png'
current_breakpoint_icon = 'current_breakpoint.png'

is_debugging = False
is_running = False


class DebuggerException(Exception):
    pass


class ProtocolException(DebuggerException):
    pass


class ProtocolConnectionException(ProtocolException):
    pass


class Protocol(object):
    '''
    Represents DBGp Protocol Language
    '''

    read_rate = 1024
    port = 9000

    def __init__(self):
        self.port = get_project_setting('port') or get_setting('port') or self.port
        self.clear()

    def clear(self):
        self.buffer = bytearray()
        self.connected = False
        self.listening = False
        self.server = None
        del self.transaction_id
        try:
            self.sock.close()
        except:
            pass
        self.sock = None

    def transaction_id():
        '''
        The transaction_id property.
        '''

        def fget(self):
            self._transaction_id += 1
            return self._transaction_id

        def fset(self, value):
            self._transaction_id = value

        def fdel(self):
            self._transaction_id = 0
        return locals()

    transaction_id = property(**transaction_id())

    def read_until_null(self):
        if self.connected:
            while self.bytearray_contains_0(self.buffer) == False:
                self.buffer += self.sock.recv(self.read_rate)
                print('Raw Buffer:', self.buffer)
                
            index = 0
            for c in self.buffer:
                if c == 0:
                    # Just a 0 was found
                    if index == 0:
                        self.buffer = bytearray()
                        print ('returning blank')
                        return ''
                    string_to_return = self.buffer[:index].decode("ascii")
                    if len(self.buffer) == index + 1:
                        self.buffer = bytearray()
                    else:
                        self.buffer = self.buffer[index + 1:]
                    print ("returning:'", string_to_return, "'  Leaving:", self.buffer)
                    return string_to_return
                index = index + 1

            return data
        else:
            raise(ProtocolConnectionException, "Not Connected")


    def bytearray_contains_0(self, b_array):
        for c in b_array:
            if c == 0:
                return True
        return False

    def read_data(self):
        length = self.read_until_null()
        message = self.read_until_null()
        if int(length) == len(message):
            return message
        else:
            raise(ProtocolException, "Length mismatch")

    def read(self):
        data = self.read_data()
        #print '<---', data
        document = parseString(data)
        return document

    def send(self, command, *args, **kwargs):
        print("START SEND");
        if 'data' in kwargs:
            data = kwargs['data']
            del kwargs['data']
        else:
            data = None

        tid = self.transaction_id
        parts = [command, '-i %i' % tid]

        if args:
            parts.extend(args)
        if kwargs:
            parts.extend(['-%s %s' % pair for pair in kwargs.items()])
        parts = [part.strip() for part in parts if part.strip()]
        command = ' '.join(parts)
        if data:
            command += ' -- ' + base64.b64encode(data)

        try:
            print(command)
            self.sock.send(bytes(command, 'ascii') + bytes('\x00', 'ascii'))
            #print '--->', command
        except Exception:
            raise(Exception)

    def accept(self):
        serv = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        if serv:
            try:
                serv.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                serv.settimeout(1)
                serv.bind(('', self.port))
                serv.listen(1)
                self.listening = True
                self.sock = None
            except Exception as x:
                raise(ProtocolConnectionException, x)

            while self.listening:
                time.sleep(.1)
                try:
                    self.sock, address = serv.accept()
                    self.listening = False
                except socket.timeout:
                    pass

            if self.sock:
                self.connected = True
                self.sock.settimeout(8)
            else:
                self.connected = False
                self.listening = False

            try:
                serv.close()
                serv = None
            except:
                pass
            return self.sock
        else:
            raise ProtocolConnectionException('Could not create socket')


class MoaidebugView(object):
    '''
    The MoaidebugView is sort of a normal view with some convenience methods.

    See lookup_view.
    '''
    def __init__(self, view):
        self.view = view
        self.current_line = None
        self.context_data = {}
        self.breaks = {}  # line : meta { id: bleh }

    def __getattr__(self, attr):
        if hasattr(self.view, attr):
            return getattr(self.view, attr)
        if attr.startswith('on_'):
            return self
        raise(AttributeError, "%s does not exist" % attr)

    def __call__(self, *args, **kwargs):
        pass

    def center(self, lineno):
        line = self.lines(lineno)[0]
        self.view.show_at_center(line)

    def add_breakpoint(self, row):
        if not row in self.breaks:
            self.breaks[row] = {}
            if protocol and protocol.connected:
                protocol.send('breakpoint_set', t='line', f=self.uri(), n=row)
                res = protocol.read().firstChild
                self.breaks[row]['id'] = res.getAttribute('id')

    def del_breakpoint(self, row):
        if row in self.breaks:
            if protocol and protocol.connected:
                protocol.send('breakpoint_remove', d=self.breaks[row]['id'])
            del self.breaks[row]

    def view_breakpoints(self):
        self.view.add_regions('moaidebug_breakpoint', self.lines(list(self.breaks.keys())), get_setting('breakpoint_scope'), get_icon_path_for_file(breakpoint_icon), sublime.HIDDEN)

    def breakpoint_init(self):
        if not self.breaks:
            return
        uri = self.uri()
        for row in self.breaks:
            print("breakpoint Set ", uri , " ", row)
            protocol.send('breakpoint_set', t='line', f=uri, n=row)
            res = protocol.read().firstChild
            self.breaks[row]['id'] = res.getAttribute('id')
            print ("Breakpoint ID: ", self.breaks[row]['id'])

    def breakpoint_clear(self):
        if not self.breaks:
            return
        key_list = list(self.breaks.keys())
        for row in key_list:
            self.del_breakpoint(row)

    def uri(self):
        return 'file://' + os.path.realpath(self.view.file_name())

    def lines(self, data=None):

        lines = []
        if data is None:
            regions = self.view.sel()
        else:
            if not isinstance(data, list):
                data = [data]
            regions = []
            for item in data:
                if isinstance(item, int) or (isinstance(item, str) and item.isdigit()):
                    regions.append(self.view.line(self.view.text_point(int(item) - 1, 0)))
                else:
                    regions.append(item)
        for region in regions:
            lines.extend(self.view.split_by_newlines(region))
        return [self.view.line(line) for line in lines]

    def rows(self, lines):
        if not isinstance(lines, list):
            lines = [lines]
        return [self.view.rowcol(line.begin())[0] + 1 for line in lines]

    def append(self, content, edit=None, end=False):
        if not edit:
            edit = self.view.begin_edit()
            end = True
        self.view.insert(edit, self.view.size(), content + "\n")
        if end:
            self.view.end_edit(edit)
        return edit

    def on_load(self):
        if self.current_line:
            self.current(self.current_line)
            self.current_line = None

    def current(self, line):
        if self.is_loading():
            self.current_line = line
            return
        region = self.lines(line)
        icon = get_icon_path_for_file(current_icon)

        if line in self.breaks.keys():
            icon = get_icon_path_for_file(current_breakpoint_icon)

        self.add_regions('moaidebug_current_line', region, get_setting('current_line_scope'), icon, sublime.HIDDEN)
        self.center(line)

    def add_context_data(self, propName, propType, propData):
        '''
        Store context data
        '''
        self.context_data[propName] = {'type': propType, 'data': propData}

    def on_selection_modified(self):
        '''
        Show selected variable in an output panel when clicked
        '''
        if protocol and protocol.connected and self.context_data:
            data = ''
            point = self.view.sel()[0].a
            var_name = self.view.substr(self.view.word(point))
            if not var_name.startswith('$'):
                var_name = '$' + var_name
            is_variable = sublime.score_selector(self.view.scope_name(point), 'variable')

            if is_variable and var_name in self.context_data:
                kind = self.context_data[var_name]['type']
                if kind == 'array' or kind == 'object':
                    for key in sorted(self.context_data.keys()):
                        if key.startswith(var_name):
                            data += '{k} ({t}) = {d}\n'.format(k=key, t=self.context_data[key]['type'], d=self.context_data[key]['data'])
                else:
                    data += '{k} ({t}) = {d}\n'.format(k=var_name, t=kind, d=self.context_data[var_name]['data'])

            window = self.view.window()
            if window:
                output = window.get_output_panel('moaidebug_inspect')
                edit = output.begin_edit()
                output.erase(edit, sublime.Region(0, output.size()))
                output.insert(edit, 0, data)
                output.end_edit(edit)
                window.run_command('show_panel', {"panel": 'output.moaidebug_inspect'})


class MoaidebugRunDebugCommand(sublime_plugin.TextCommand):
    '''
    Start listening for Moaidebug connections
    '''
    def run(self, edit):
        global is_debugging
        is_debugging = True
        global protocol
        protocol = Protocol()

        global original_layout
        global debug_view
        window = sublime.active_window()
        original_layout = window.get_layout()
        debug_view = window.active_view()
        window.set_layout({
            "cols": [0.0, 0.5, 1.0],
            "rows": [0.0, 0.7, 1.0],
            "cells": [[0, 0, 2, 1], [0, 1, 1, 2], [1, 1, 2, 2]]
        })


        sublime.active_window().run_command('save_all')
        sublime.active_window().run_command('show_panel', {"panel": "console"})    
        threading.Thread(target=self.thread_callback).start()

        # Start program debugging using included debug library - First we nee to make our lua launch file      
        folders = sublime.active_window().folders()
        if len(folders) == 0:
            print('Unable to find code directory')
            return;

        main_folder = folders[0] + '/'
        launch_folder = sublime.packages_path() + '/Moai Debugger/lua_launchers/'
        launch_file = launch_folder + 'launch.lua'
        out_file = open(launch_file, "w+")
        out_file.write('package.path = "' + launch_folder +  '?.lua;" .. package.path\n')
        out_file.write('require("debugger")("127.0.0.1", 9000, nil, nil, nil, "' + main_folder + '")\n')
        out_file.write('dofile("main.lua")\n')
        out_file.close()
        RunToConsole(['moai', launch_file], main_folder)



    def thread_callback(self):
        protocol.accept()
        if protocol and protocol.connected:
            sublime.set_timeout(self.gui_callback, 0)

    def gui_callback(self):
        sublime.status_message('Moaidebug: Connected')
        init = protocol.read().firstChild
        uri = init.getAttribute('fileuri')
        print("The URI", uri)

        for view in buffers.values():
            view.breakpoint_init()

        self.view.run_command('moaidebug_continue', {'state': 'run'})

    def is_enabled(self):
        global is_debugging
        global is_running
        if protocol or is_debugging or is_running:
            return False
        return True


class MoaidebugClearAllBreakpointsCommand(sublime_plugin.TextCommand):
    '''
    Clear breakpoints in all open buffers
    '''
    def run(self, edit):
        for view in buffers.values():
            view.breakpoint_clear()
            view.view_breakpoints()





class MoaidebugHelpCommand(sublime_plugin.TextCommand):
    '''
    Clear breakpoints in all open buffers
    '''
    def run(self, edit):
        help_message = '''
Welcome to Moai Debug for Sublime:
1. Make sure Moai is in your system PATH 
2. Make sure this is installed under "Packages/Moai Debugger/"
3. Make sure your Moai project has a main.lua in the root folder
4. This is a Sublime 3 Plug-in for OSX. Other platforms may work but are untested
        '''
        sublime.message_dialog(help_message)


class MoaidebugBreakpointCommand(sublime_plugin.TextCommand):
    '''
    Toggle a breakpoint
    '''
    def run(self, edit):
        view = lookup_view(self.view)
        for row in view.rows(view.lines()):
            if row in view.breaks:
                view.del_breakpoint(row)
            else:
                view.add_breakpoint(row)
        view.view_breakpoints()


class MoaidebugCommand(sublime_plugin.TextCommand):
    '''
    The Moaidebug main quick panel menu
    '''
    def run(self, edit):
        mapping = {
            'moaidebug_breakpoint': 'Add/Remove Breakpoint',
            'moaidebug_clear_all_breakpoints': 'Clear all moai Breakpoints',
        }

        if protocol:
            mapping['moaidebug_clear'] = 'Stop debugging moai'
        else:
            mapping['moaidebug_listen'] = 'Start debugging moai'

        if protocol and protocol.connected:
            mapping.update({
                'moaidebug_status': 'Status',
                'moaidebug_execute': 'Execute',
            })

        self.cmds = mapping.keys()
        self.items = mapping.values()
        self.view.window().show_quick_panel(self.items, self.callback)

    def callback(self, index):
        if index == -1:
            return

        command = self.cmds[index]
        self.view.run_command(command)

        if protocol and command == 'moaidebug_listen':
            url = get_project_setting('url')
            if url:
                webbrowser.open(url + '?MOAIDEBUG_SESSION_START=sublime.moaidebug')
            else:
                sublime.status_message('Moaidebug: No URL defined in project settings file.')

            global original_layout
            global debug_view
            window = sublime.active_window()
            original_layout = window.get_layout()
            debug_view = window.active_view()
            window.set_layout({
                "cols": [0.0, 0.5, 1.0],
                "rows": [0.0, 0.7, 1.0],
                "cells": [[0, 0, 2, 1], [0, 1, 1, 2], [1, 1, 2, 2]]
            })

        if command == 'moaidebug_clear':
            url = get_project_setting('url')
            if url:
                webbrowser.open(url + '?MOAIDEBUG_SESSION_STOP=sublime.moaidebug')
            else:
                sublime.status_message('Moaidebug: No URL defined in project settings file.')
            window = sublime.active_window()
            window.run_command('hide_panel', {"panel": 'output.moaidebug_inspect'})
            window.set_layout(original_layout)




class MoaidebugContinueCommand(sublime_plugin.TextCommand):
    '''
    Continue execution menu and commands.

    This command shows the quick panel and executes the selected option.
    '''
    states = {
        'run': 'Run',
        'step_into': 'Step Into',
        'step_over': 'Step Over',
        'step_out': 'Step Out',
        'stop': 'Stop',
        'detach': 'Detach',
    }

    def run(self, edit, state=None):
        if not state or not state in self.states:
            self.view.window().show_quick_panel(self.states.values(), self.callback)
        else:
            self.callback(state)

    def callback(self, state):
        if state == -1:
            return
        if isinstance(state, int):
            state = self.states.keys()[state]

        global moaidebug_current
        reset_current()

        protocol.send(state)
        res = protocol.read().firstChild

        for child in res.childNodes:
            if child.nodeName == 'moaidebug:message':
                #print '>>>break ' + child.getAttribute('filename') + ':' + child.getAttribute('lineno')
                sublime.status_message('Moaidebug: breakpoint')
                moaidebug_current = show_file(self.view.window(), child.getAttribute('filename'))
                moaidebug_current.current(int(child.getAttribute('lineno')))

        if (res.getAttribute('status') == 'break'):
            # TODO stack_get
            protocol.send('context_get')
            res = protocol.read().firstChild
            result = ''

            def getValues(node):
                result = str('')
                for child in node.childNodes:
                    if child.nodeName == 'property':
                        propName = base64.b64decode(child.getAttribute('fullname')).decode('utf-8')
                        print("Name!:", propName)
                        propType = str(child.getAttribute('type'))
                        propValue = None
                        
                        try:

                            propValue = str(' '.join(base64.b64decode(t.data.strip()).decode('utf-8') for t in child.childNodes if t.nodeType == t.TEXT_NODE or t.nodeType == t.CDATA_SECTION_NODE))
                        except:
                            for t in child.childNodes:
                                if t.nodeType == t.TEXT_NODE or t.nodeType == t.CDATA_SECTION_NODE:
                                    print ('MY DATA:"' , t.data.strip(), '"')
                            propValue = str(' '.join(t.data for t in child.childNodes if t.nodeType == t.TEXT_NODE or t.nodeType == t.CDATA_SECTION_NODE))

                        if propName:
                            if propName.lower().find('password') != -1:
                                propValue = str('*****')
                            result = result + str(propName + ' [' + propType + '] = ' + str(propValue) + '\n')
                            result = result + getValues(child)
                            if moaidebug_current:
                                moaidebug_current.add_context_data(propName, propType, propValue)
                return result

            result = getValues(res)
            add_debug_info('context', result)
            if moaidebug_current:
                moaidebug_current.on_selection_modified()

            protocol.send('stack_get')
            res = protocol.read().firstChild
            result = str('')
            for child in res.childNodes:
                if child.nodeName == 'stack':
                    propWhere = child.getAttribute('where')
                    propLevel = child.getAttribute('level')
                    propType = child.getAttribute('type')
                    propFile = child.getAttribute('filename')
                    propLine = child.getAttribute('lineno')
                    result = result + str('{level:>3}: {type:<10} {where:<10} {filename}:{lineno}\n' \
                                              .format(level=propLevel, type=propType, where=propWhere, lineno=propLine, filename=propFile))
            add_debug_info('stack', result)

        if res.getAttribute('status') == 'stopping' or res.getAttribute('status') == 'stopped':
            self.view.run_command('moaidebug_clear')
            self.view.run_command('moaidebug_listen')
            sublime.status_message('Moaidebug: Page finished executing. Reload to continue debugging.')

    def is_enabled(self):
        if protocol and protocol.connected:
            return True
        if protocol:
            sublime.status_message('Moaidebug: Waiting for executing to start')
            return False
        sublime.status_message('Moaidebug: Not running')
        return False


class MoaidebugStopDebugCommand(sublime_plugin.TextCommand):
    '''
    Close the socket and stop listening to moaidebug
    '''
    def run(self, edit):
        global protocol
        try:
            window = sublime.active_window()
            window.run_command('hide_panel', {"panel": 'output.moaidebug_inspect'})
            window.set_layout(original_layout)
            protocol.clear()
            reset_current()
            subprocess.Popen(['killall', 'moai'], stdout=subprocess.PIPE);
            sublime.active_window().run_command('hide_panel', {"panel": "console"})   
            global is_debugging
            is_debugging = False
        except:
            pass
        finally:
            protocol = None

    def is_enabled(self):
        global is_debugging
        if protocol and is_debugging:
            return True
        return False


class MoaidebugStatus(sublime_plugin.TextCommand):
    '''
    DBGp status command
    '''
    def run(self, edit):
        protocol.send('status')
        res = protocol.read().firstChild
        sublime.status_message(res.getAttribute('reason') + ': ' + res.getAttribute('status'))

    def is_enabled(self):
        if protocol and protocol.connected:
            return True
        return False


class MoaidebugExecute(sublime_plugin.TextCommand):
    '''
    Execute arbitrary DBGp command
    '''
    def run(self, edit):
        self.view.window().show_input_panel('Moaidebug Execute', '',
            self.on_done, self.on_change, self.on_cancel)


    def is_enabled(self):
        if protocol and protocol.connected:
            return True
        return False

    def on_done(self, line):
        if ' ' in line:
            command, args = line.split(' ', 1)
        else:
            command, args = line, ''
        protocol.send(command, args)
        res = protocol.read().firstChild

        window = self.view.window()
        output = window.get_output_panel('moaidebug_execute')
        edit = output.begin_edit()
        output.erase(edit, sublime.Region(0, output.size()))
        output.insert(edit, 0, res.toprettyxml())
        output.end_edit(edit)
        window.run_command('show_panel', {"panel": 'output.moaidebug_execute'})

    def on_change(self, line):
        pass

    def on_cancel(self):
        pass

class MoaidebugJustRun(sublime_plugin.TextCommand):


    def run(self, edit):
        global is_running 
        is_running = True;
        sublime.active_window().run_command('show_panel', {"panel": "console"})       
        sublime.active_window().run_command('save_all')
        folders = sublime.active_window().folders()
        if len(folders) > 0:
            folder = folders[0] + '/'
            RunToConsole(['moai', 'main.lua'], folder)


    def is_enabled(self):
        global is_debugging
        global is_running
        return not is_running and not is_debugging
    def on_done(self, line):
        pass
    def on_change(self, line):
        pass
    def on_cancel(self):
        pass

class MoaidebugJustStop(sublime_plugin.TextCommand):
    '''
    Execute arbitrary DBGp command
    '''
    def run(self, edit):
        global is_running
        is_running = False
        subprocess.Popen(['killall', 'moai'], stdout=subprocess.PIPE);
        sublime.active_window().run_command('hide_panel', {"panel": "console"})     
    def is_enabled(self):
        return is_running
    def on_done(self, line):
        pass
    def on_change(self, line):
        pass
    def on_cancel(self):
        pass



class MoaidebugTest(sublime_plugin.TextCommand):
    '''
    Execute arbitrary DBGp command
    '''
    def run(self, edit):
        

        folders = sublime.active_window().folders()
        if len(folders) == 0:
            print('Unable to find code directory')
            return;

        main_folder = folders[0] + '/'

        launch_folder = sublime.packages_path() + '/Moai Debugger/lua_launchers/'
        launch_file = launch_folder + 'launch.lua'
        out_file = open(launch_file, "w+")
        out_file.write('package.path = "' + launch_folder +  '?.lua;" .. package.path\n')
        out_file.write('require("debugger")("127.0.0.1", 9000)\n')
        out_file.write('dofile("main.lua")\n')
        out_file.close()
        RunToConsole(['moai', launch_file], main_folder)
       # sublime.message_dialog(sublime.packages_path() + '/lua_launchers/')

        '''
        sublime.active_window().run_command('save_all');
        
        subprocess.Popen(['moai', 'main.lua'], shell=True, cwd='/Users/davidholtkamp/Dropbox/Deimos/Lua/lua/')
        
        
        '''
    def is_enabled(self):
        return True
    def on_done(self, line):
        pass
    def on_change(self, line):
        pass
    def on_cancel(self):
        pass


class EventListener(sublime_plugin.EventListener):
    def on_new(self, view):
        lookup_view(view).on_new()

    def on_clone(self, view):
        lookup_view(view).on_clone()

    def on_load(self, view):
        lookup_view(view).on_load()

    def on_close(self, view):
        lookup_view(view).on_close()

    def on_pre_save(self, view):
        lookup_view(view).on_pre_save()

    def on_post_save(self, view):
        lookup_view(view).on_post_save()

    def on_modified(self, view):
        lookup_view(view).on_modified()

    def on_selection_modified(self, view):
        lookup_view(view).on_selection_modified()

    def on_activated(self, view):
        lookup_view(view).on_activated()

    def on_deactivated(self, view):
        lookup_view(view).on_deactivated()

    def on_query_context(self, view, key, operator, operand, match_all):
        lookup_view(view).on_query_context(key, operator, operand, match_all)


def lookup_view(v):
    '''
    Convert a Sublime View into an MoaidebugView
    '''
    if isinstance(v, MoaidebugView):
        return v
    if isinstance(v, sublime.View):
        id = v.buffer_id()
        if id in buffers:
            buffers[id].view = v
        else:
            buffers[id] = MoaidebugView(v)
        return buffers[id]
    return None


def show_file(window, uri):
    '''
    Open or focus a window
    '''
    if window:
        window.focus_group(0)
    if sublime.platform() == 'windows':
        transport, filename = uri.split(':///', 1)  # scheme:///C:/path/file => scheme, C:/path/file
    else:
        transport, filename = uri.split('://', 1)  # scheme:///path/file => scheme, /path/file
    if transport == 'file' and os.path.exists(filename):
        window = sublime.active_window()
        views = window.views()
        found = False
        for v in views:
            if v.file_name():
                path = os.path.realpath(v.file_name())
                if path == os.path.realpath(filename):
                    view = v
                    window.focus_view(v)
                    found = True
                    break
        if not found:
            #view = window.open_file(filename, sublime.TRANSIENT)
            view = window.open_file(filename)
        return lookup_view(view)


def reset_current():
    '''
    Reset the current line marker
    '''
    global moaidebug_current
    if moaidebug_current:
        moaidebug_current.erase_regions('moaidebug_current_line')
        moaidebug_current = None


def get_project_setting(key):
    '''
    Get a project setting.

    Moaidebug project settings are stored in the sublime project file
    as a dictionary:

        "settings":
        {
            "moaidebug": { "key": "value", ... }
        }
    '''
    try:
        s = sublime.active_window().active_view().settings()
        moaidebug = s.get('moaidebug')
        if moaidebug:
            if key in moaidebug:
                return moaidebug[key]
    except:
        pass


def get_setting(key):
    '''
    Get Moaidebug setting
    '''
    s = sublime.load_settings("Moaidebug.sublime-settings")
    if s and s.has(key):
        return s.get(key)


def get_icon_path_for_file(file_name):
    path = 'Packages/Moai Debugger/icons/' + file_name 
    return path



def RunToConsole(args, current_dir = None):
    sublime.set_timeout_async(lambda: run_in_background(args, current_dir), 0)
    

def run_in_background(args, current_dir):
    proc = None
    if current_dir is None:
        proc = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    else:
        proc = subprocess.Popen(args,  cwd=current_dir, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    while proc.poll() is None:
        try:
            data = proc.stdout.readline().decode(encoding='UTF-8')
            print(data, end="")
        except:
            print("process ended...")
            proc = None
            return;
    print("process ended...")
    proc = None
    return


class MoaidebugAddDebugInfoCommand(sublime_plugin.TextCommand):
    def run(self, edit, data = None, name = 'none'):
        window = sublime.active_window()
        v = None
        for v in window.views():
            if v.name() == name:
                found = True
                break

        if found:
            v.erase(edit, sublime.Region(0, v.size()))
            v.insert(edit, 0, data)    

    def is_enabled(self):
        return True
    def on_done(self, line):
        pass
    def on_change(self, line):
        pass
    def on_cancel(self):
        pass




def add_debug_info(name, data):
    '''
    Adds data to the debug output windows
    '''
    found = False
    v = None
    window = sublime.active_window()

    if name == 'context':
        group = 1
        fullName = "Moaidebug Context"
    if name == 'stack':
        group = 2
        fullName = "Moaidebug Stack"

    for v in window.views():
        if v.name() == fullName:
            found = True
            break

    if not found:
        v = window.new_file()
        v.set_scratch(True)
        v.set_read_only(True)
        v.set_name(fullName)
        v.settings().set('word_wrap', False)
        found = True

    if found:
        v.set_read_only(False)
        window.set_view_index(v, group, 0)
        print(type(data))
        v.run_command('moaidebug_add_debug_info', {'name':fullName, 'data':data})
        v.set_read_only(True)

    window.focus_group(0)
