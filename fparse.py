#!/usr/bin/env python
# -*- coding: utf-8 -*-

import subprocess
import sys
import re
from os import path
from pprint import pprint
from itertools import chain

#===============================================================================
def main():
    if(len(sys.argv) != 3):
        print("Usage: fparse.py <input.F> <output.ast>")
        sys.exit(1)

    fn_in, fn_out = sys.argv[1:]
    assert(fn_in.endswith(".F"))
    assert(fn_out.endswith(".ast"))

    ast = parse_file(fn_in)
    f = open(fn_out, "w")
    pprint(ast, stream=f)
    f.close()

    print "Wrote: "+fn_out

#===============================================================================
def parse_file(fn):
    stream = InputStream(fn)
    line = stream.peek_next_fortran_line()
    if(line.startswith("MODULE ")):
        return parse_module(stream)
    else:
        raise ParserException(line, stream.locus())

    #TODO: ensure nothing comes after module

#===============================================================================
def parse_module(stream):
    doxygen = parse_doxygen(stream)

    # parse opening line
    kw, name = stream.next_fortran_line().split()
    beg_locus = stream.locus()
    assert(kw == "MODULE")

    ast = {'tag':'module', 'name':name, 'descr':doxygen['brief'], 'uses':[], 'publics':[], 'types':[], 'subroutines':[], 'functions':[], 'interfaces':[], 'variables':[]}

    # parse stuff before CONTAINS
    private, save = False, False
    private_syms = []
    while(True):
        line = stream.peek_next_fortran_line()
        if(line.startswith("USE ")):
            u = parse_use_statement(stream)
            ast['uses'].append(u)
        elif(line == "IMPLICIT NONE"):
            stream.next_fortran_line() # skip line
        elif(line == "PRIVATE"):
            private = True
            stream.next_fortran_line() # skip line
        elif(line == "SAVE"):
            save = True
            stream.next_fortran_line() # skip line
        elif(line.startswith("DATA ")):
            # TODO!!!!
            stream.next_fortran_line() # skip line
        elif(match_var_decl(line)):
            # these could be public if PRIVATE was not (yet) found
            vlist = parse_var_decl(line)
            assert(vlist) # No executable statements allowed here!
            ast['variables'].extend(vlist)
            stream.next_fortran_line() # skip line
            commit_locus_inplace(vlist, stream.locus())
        elif(line.startswith("TYPE")):
            assert(not line.startswith("TYPE("))
            t = parse_type(stream)
            ast['types'].append(t)
        elif(line.startswith("PUBLIC")):
            syms = parse_pubpriv_statement(stream)
            ast['publics'].extend(syms)
        elif(line.startswith("PRIVATE")):
            syms = parse_pubpriv_statement(stream)
            # only the name of these symbols is retained here
            private_syms.extend([sym['name'] for sym in syms])
        elif(line.startswith("INTERFACE") or line.startswith("ABSTRACT INTERFACE")):
            a =  parse_interface(stream)
            ast['interfaces'].append(a)
        elif(line == "CONTAINS"):
            stream.next_fortran_line() # skip line
            break
        elif(re.match("^END ?MODULE", line)):
            break # not every module has a CONTAINS
        else:
            raise ParserException(line, stream.locus())

    # here ...
    #   ...all the USE..ONLY statements/attributes should have been collected!
    ast['multiple_imports'] = multiple_imports(ast['uses'])

    #   ...all the INTERFACES should have been collected!
    merge_interfaces_inplace(ast['interfaces'])

    #   ...all the PUBLIC/PRIVATE/SAVE/... statements/attributes should have been set!
    set_visibility(ast['variables'], ast['publics'], private, private_syms)
    set_staticness(ast['variables'], save)

    # parse stuff after CONTAINS
    while(True):
        line = stream.peek_next_fortran_line()
        if(line.split(" ",1)[0] in ("SUBROUTINE", "FUNCTION", "ELEMENTAL", "PURE", "RECURSIVE")):
            s = parse_routine(stream)
            ast[s['tag']+'s'].append(s)
        elif(match_var_decl(line)):
            # when a variable declaration is found here it is actually a
            # function with the inline declaration of the returned value type
            assert("FUNCTION" in line)
            s = parse_routine(stream)
            assert(s['tag']=='function')
            ast['functions'].append(s)
        elif(re.match("^END ?MODULE", line)):
            ast['beg_end_loci'] = (beg_locus, stream.locus())
            break # found module's closing line
        else:
            raise ParserException(line, stream.locus())

    return(ast)

#===============================================================================
def merge_interfaces_inplace(interfaces):

    todo = {}
    for i in interfaces:
        if i['task']=='overloading':
            todo.setdefault(i['name'], []).append(i)

    for k, v in todo.iteritems():
        if (len(v)>1):
            print "MULTI:", k
            assert all(i['name']==k for i in v)
            merged_i = {'name':k, 'tag':'interface', 'task': 'overloading'}
            for i in v:
                merged_i.setdefault('procedures', []).extend(i['procedures'])
                merged_i.setdefault('beg_end_loci', []).append(i['beg_end_loci']) # TODO: this could be tricky!
                interfaces.remove(i)
            interfaces.append(merged_i)

#===============================================================================
def multiple_imports(uses):

    mult_imports = {}
    for u in uses:
        if 'only' in u:
            mod_from = u['from']
            for k, v in u['only'].iteritems():
                mult_imports.setdefault(k, []).append(":".join([mod_from, v]))
        else:
            pass # TODO: cannot check in this case...

    keys_to_delete = [k for k, v in mult_imports.iteritems() if (len(v)==1)]
    for k in keys_to_delete:
        del mult_imports[k]

    return(mult_imports)

#===============================================================================
def commit_locus_inplace(vlist, locus):
    for v in vlist:
        assert(not 'locus' in v)
        v['locus'] = locus

#===============================================================================
def parse_interface(stream):
    raw_line = stream.next_fortran_line()
    beg_locus = stream.locus()
    prefix, line = re.match("(ABSTRACT )?(INTERFACE.*)", raw_line).groups()
    assert(line.startswith("INTERFACE"))
    name = line.split(" ",1)[1] if(" " in line) else ""
    ast = {'tag':'interface', 'name':name, 'procedures':[]}
    while(True):
        # The line is only peeked here and the stream advanced later on
        #   in order to be able to call parse_routine() afterwards.
        line = stream.peek_next_fortran_line()
        if(re.match("^END ?INTERFACE", line)):
            stream.next_fortran_line()
            ast['beg_end_loci'] = (beg_locus, stream.locus())
            break
        m = re.match("^MODULE PROCEDURE (.+)$", line)
        if(m):
            assert(name)
            if('task' in ast):
                assert(ast['task'] == 'overloading')
            else:
                ast['task'] = 'overloading'
            ast['procedures'].extend( m.group(1).split(",") )
            stream.next_fortran_line()
        else:
            if('task' in ast):
                assert(ast['task'] == 'explicit_interface')
            else:
                ast['task'] = 'explicit_interface'
            f = parse_routine(stream)
            assert(f['tag'] in ("subroutine", "function"))
            ast['procedures'].append(f)
    # abstract interfaces
    if prefix:
        assert(prefix.strip() == 'ABSTRACT' and len(ast['procedures']) == 1)
        assert(not name)
        name = ast['procedures'][0]['name']
        ast.update( {'name':name, 'task':'abstract_interface'} )
    return(ast)

#===============================================================================
def parse_type(stream):
    doxygen = parse_doxygen(stream)

    line = stream.next_fortran_line()
    beg_locus = stream.locus()
    assert(line.startswith("TYPE"))
    attrlist, sep, name = re.match("TYPE(,BIND\(.*\))?( |::)(.+)", line).groups()
    ast = {'tag':'type', 'name':name, 'descr':doxygen['brief'], 'variables':[]}

    if attrlist:
        assert(sep=='::')
        ast['attrs'] = attrlist[1:]

    private = False
    while(True):
        line = stream.next_fortran_line()
        if(re.match("^END ?TYPE", line)):
            ast['beg_end_loci'] = (beg_locus, stream.locus())
            break
        elif(line == "PRIVATE"):
            private = True
        elif(match_var_decl(line)):
            vlist = parse_var_decl(line)
            assert(vlist) # No executable statements allowed here!
            ast['variables'].extend(vlist)
        else:
            raise ParserException(line, stream.locus())

    set_visibility(ast['variables'], [], private)
    commit_type_members_descr(ast, doxygen)

    return(ast)

#===============================================================================
def parse_pubpriv_statement(stream):
    ast = []
    line = stream.next_fortran_line() # skip line
    m = re.match("(PUBLIC|PRIVATE)(::| )(.*)$",line)
    vis, sep, syms = m.groups()
    is_api = stream.peek_prev_line().replace(" ","").startswith("!API")
    ast = [{'tag':vis.lower(), 'name':s, 'is_api':is_api} for s in syms.split(",")]
    return(ast)

#===============================================================================
def set_visibility(variables, publics, private, privlist=[]):
    publist = [item['name'] for item in publics]
    default = 'PRIVATE' if private else 'PUBLIC'
    for v in variables:

        vis = set(v['attrs']).intersection(('PRIVATE','PUBLIC'))
        if(vis):
            # visibility already set via the variable declaration line!
            assert(not v['name'] in publist)
            assert(not v['name'] in privlist)
            assert(len(vis)==1)
            v['visibility'] = vis.pop()
            if v['visibility'] == 'PUBLIC':
                publics.append({'tag':'public', 'name':v['name'], 'is_api':False})

        elif(v['name'] in publist):
            v['visibility'] = 'PUBLIC'

        elif(v['name'] in privlist):
            v['visibility'] = 'PRIVATE'

        else:
            # fallback to default visibility
            v['visibility'] = default

#===============================================================================
def set_staticness(variables, save):
    if(save):
        for v in variables:
            assert( not 'SAVE' in v['attrs'] )
            if( not 'PARAMETER' in v['attrs'] ):    # SAVE conflicts with
                v['attrs'].append('SAVE')           # the PARAMETER attribute!

#===============================================================================
def match_var_decl(line):
    for t in ("CHARACTER", "INTEGER", "REAL", "COMPLEX", "LOGICAL", "TYPE(", "PROCEDURE("):
        if(line.startswith(t)):
            return(True)
    return(False)

#===============================================================================
def parse_var_decl(line):

    # Obtain the common info for all the variables declared here
    vtype = get_var_type(line)
    if not vtype:
        # The 1st executable statement could begin with a pattern that is
        #   misleading for match_var_decl().
        #   E.g.: "real_file_name = ..." [FUNCTION:file_exists, common/cp_files.F]
        #   Need this check to escape in such a case.
        return
    assert(line.startswith(vtype))

    if "::" in line:
        m = re.match(re.escape(vtype)+"(,.+)?::(.*)",line)
        attrlist, varlist = m.groups()

    else:
        # simpler case: there is no attribute!
        attrlist = None
        m = re.match(re.escape(vtype)+"( )?(.*)",line)
        sep, varlist = m.groups()
        if sep:
            assert(sep==' ')
            assert(re.match("[A-Z]+$",vtype))
        else:
            assert(re.match("[A-Z]+\(.+\)$",vtype))

    # Now deal with attributes
    attrs = get_attributes(attrlist)

    # Process variables
    variables = get_variables(varlist, vtype, attrs)

    return(variables)

#===============================================================================
def get_attributes(attrlist):
    attrs = {'keywd_attrs':[], 'dimension':None, 'intent':None, 'bind':None, 'raw':[]}
    while(attrlist):
        k, v = get_next_attribute(attrlist)
        if(k in ("ALLOCATABLE", "EXTERNAL", "OPTIONAL",
                 "PARAMETER", "POINTER", "PRIVATE", "PUBLIC", "PROTECTED",
                 "SAVE", "TARGET", "VALUE", "VOLATILE")):
            assert(not v)
            attrs['keywd_attrs'].append(k)
        elif(k ==  "INTENT"):
            m = re.match("\(([A-Z]+)\)$", v)
            intent = m.group(1)
            assert(intent in ("IN", "OUT", "INOUT"))
            attrs['intent'] = intent
        elif(k ==  "DIMENSION" or k == "BIND"):
            m = re.match("(\(.+\))$", v)
            assert(m)
            attrs[k.lower()] = v
        else:
            raise Exception('problem with input: "%s" (attr key: "%s")'%(attrlist,k))
        raw = k + v
        attrs['raw'].append(raw)
        to_skip = "," + raw
        assert(attrlist.startswith(to_skip))
        attrlist = attrlist[ len(to_skip) : ]  # skip the successfully processed attribute
    return attrs['raw']

#===============================================================================
def get_next_attribute(string):
    """Decode variable attributes: return the first one found in the input string
       that is in the form: ',<1st attribute>[(...)],<2nd attr.>,...'
       A State-machine is needed to deal with nested parentheses. """
    m = re.match(",([A-Z]+)(.*)",string)
    kw, postfix = m.groups()
    ppattern = ""
    state = "start"
    for c in postfix:  # process char by char what is beyond the keyword kw

        if(state=="start"):
            if(c == ','):
                break
            elif(c == '('):
                n_opened = 1
                ppattern = c
                state = "p_open"
            else:
                raise SM_UnknownCharException(c,state,string)

        elif(state=="p_open"):
            ppattern += c
            if(c == "("):
                n_opened += 1
            elif(c == ")"):
                n_opened -= 1
                if(n_opened==0):
                    state = "start"
            else:
                pass

        else:
            assert(False) # Unknown state

    assert(state=="start")  # allowed final state
    return ( kw, ppattern )

#===============================================================================
def get_var_type(string):
    my_re = re.compile("([A-Z]+)(.*)")
    m = my_re.match(string)
    kw, postfix = m.groups()

    # kw must match the whole identifier at the beginning of the string
    #   if not: this is not a variable declaration!
    if kw != re.match("\w+", string).group(0):
        return

    ppattern = ""
    state = "start"
    for i, c in enumerate(postfix):
        if(state=="start"):
            if(c == ','):
                # an attribute is coming: var_type is complete!
                break
            elif(c == ':'):
                # the variables list is coming: var_type is complete ("::")!
                assert(postfix[i:].startswith("::"))
                assert(my_re.match(postfix[i+2:]))
                break
            elif(c == ' '):
                # the variables list is coming: var_type is complete (" ")!
                assert(my_re.match(postfix[i+1:]))
                break
            elif(my_re.match(postfix[i:])):
                assert(ppattern)
                break
            elif(c == '('):
                n_opened = 1
                ppattern = c
                state = "p_open"
            else:
                raise SM_UnknownCharException(c,state,string)
        elif(state=="p_open"):
            ppattern += c
            if(c == "("):
                n_opened += 1
            elif(c == ")"):
                n_opened -= 1
                if(n_opened==0):
                    state = "start"
            else:
                pass
        else:
            assert(False) # Unknown state

    assert(state=="start")
    return kw + ppattern

#===============================================================================
def get_variables(varlist, vtype, attrs):
    """Return a list. Each list item is a dictionary related to a variable found in the input string.
       Each variable comes with its name and eventually attributes (dimension, intent, ...) or initialization."""
    assert(varlist)
    dim_from_attrs = next((re.match("DIMENSION(\(.+\))$",a).group(1) for a in attrs if a.startswith("DIMENSION(")), None)
    variables = []
    varlist += ","
    while(varlist):
        v = get_next_variable(varlist)
        to_skip = v.pop('raw') + ","
        assert(varlist.startswith(to_skip))
        varlist = varlist[ len(to_skip) : ]  # skip the successfully processed variable

        # update the variable with the common info
        v['type'] = vtype
        v['attrs'] = attrs[:]
        if(dim_from_attrs):
            if(v['dim']):
                assert(v['dim']==dim_from_attrs)
            else:
                v['dim'] = dim_from_attrs

        variables.append(v)
    return variables

#===============================================================================
def get_next_variable(string):
    """Decode variable list: return the first variable found in the input string.
       A State-machine is used to deal with nested parentheses or inline
       initialization (eventually with quoted strings)."""
    m = re.match("(\w+)(.*)",string)
    name, postfix = m.groups()
    v = {'tag':'variable', 'name':name, 'dim':''}
    n_opened = 0
    n_square = 0
    state = "start"
    ichar = 0
    while(True):  # process char by char what is beyond the identifier
        c = postfix[ichar]

        if(state=="start"):
            if(c == ','):
                break
            elif(c == '='):
                v['init'] = c
                state = "init"
            elif(c == '('):
                assert(n_opened==0)
                n_opened = 1
                ppattern = c
                state = "p_open%" + state
            else:
                raise SM_UnknownCharException(c,state,string[:-1])

        elif(state=="init"):
            if(c == ','):
                break
            elif(c == "'" or c == '"'):
                v['init'] += c
                state = "str:"+c
            elif(re.match('\w', c)):
                v['init'] += c
            elif(c in ('+','-','*','/','.','>','%','=')):
                v['init'] += c
            elif(c == '('):
                assert(n_opened==0)
                n_opened = 1
                ppattern = c
                state = "p_open%" + state
            elif(c == '['):
                assert(n_square==0)
                n_square = 1
                sqpattern = c
                state = "sq_open%" + state
            else:
                raise SM_UnknownCharException(c,state,string[:-1])

        elif(state.startswith("str:")):  # this state can only be reached from "init"
            v['init'] += c
            if(state.split(":",1)[1] == c):
                state = "init"

        elif(state.startswith("p_open%")):
            ppattern += c
            if(c == "("):
                n_opened += 1
            elif(c == ")"):
                n_opened -= 1
                if(n_opened==0):
                    prev_state = state.split("%",1)[1]
                    if(prev_state == "start"):
                        v['dim'] = ppattern
                    elif(prev_state == "init"):
                        v['init'] += ppattern
                    else:
                        raise Exception('unknown previous state: "%s"'%prev_state)
                    state = prev_state

        elif(state.startswith("sq_open%")):
            sqpattern  += c
            if(c == "["):
                n_square += 1
            elif(c == "]"):
                n_square -= 1
                if(n_square==0):
                    prev_state = state.split("%",1)[1]
                    if(prev_state == "init"):
                        v['init'] += sqpattern
                    else:
                        raise Exception('unknown previous state: "%s"'%prev_state)
                    state = prev_state

        else:
            raise Exception('unknown state: "%s"'%state)

        ichar += 1

    assert(state=="start" or state=="init")  # allowed final states
    v['raw'] = "".join( [v[k] if k in v else "" for k in ('name', 'dim', 'init')] )
    return v

#===============================================================================
def parse_routine(stream):
    doxygen = parse_doxygen(stream)

    line1 = stream.next_fortran_line()
    beg_locus = stream.locus()
    regex = re.compile('(.*)(FUNCTION|SUBROUTINE) (\w+)(?:\((.*?)\))?(.*)') # it is used afterwards too...
    #                   |   |                     |    |    |        |
    #                   |   |                     |    |    |        .
    #                   |   |                     |    |    |         \..postfix: RESULT(..) | BIND(..)
    #                   |   |                     |    |    .
    #                   |   |                     |    |     \..arguments list
    #                   |   |                     |    .
    #                   |   |                     |     \..[non-capturing group: the "()" empty list of arguments can be omitted]
    #                   |   .                     .
    #                   |    \..whatis             \..name
    #                   .
    #                    \..prefix: RECURSIVE | PURE | ...

    m = regex.match(line1)
    prefix, whatis, name, args, postfix = m.groups()
    ast = {
     'tag':whatis.lower(),
     'name':name,
     'descr':doxygen['brief'],
     'args':[], 'attrs':[], 'uses':[], 'types':[],
     'retval':{'tag':'return_value', 'name':name, 'descr':None} if whatis=='FUNCTION' else None
    }

    # update the ast according to info found in args, prefix and postfix
    decode_args(ast, args)
    decode_prefix(ast, prefix)
    decode_postfix(ast, postfix)
    commit_args_descr(ast, doxygen)

    # parse variable declarations: fetch info on arguments type and attributes
    var_decl_list = []
    dimensions = {}
    while(True):
        line = stream.peek_next_fortran_line()

        # variable declarations
        if(match_var_decl(line)):
            vlist = parse_var_decl(line)
            if(not vlist):
                break  # this line isn't actually a variable declaration!
            var_decl_list.extend( vlist )
            stream.next_fortran_line() # advance stream

        # we could have a function/subroutine as argument!
        elif(line.startswith("INTERFACE")):
            intfc = parse_interface(stream)
            assert(not intfc['name'] and len(intfc['procedures'])==1)
            f = intfc['procedures'].pop()
            var_decl_list.append( {'name':f['name'], 'type':'PROCEDURE', 'attrs':[f['tag']], 'dim':None} )

        # explicitly handle the following cases, if we don't, the scan for arguments type declaration will end prematurely!
        #
        #   ..."USE" statements or TYPE definitions not in the module header
        elif(line.startswith("USE ")):
            u = parse_use_statement(stream)
            ast['uses'].append(u)
        elif(line.startswith("TYPE")):
            assert(not line.startswith("TYPE("))
            t = parse_type(stream)
            ast['types'].append(t)
        #
        #   ...deferred attributes
        elif(line.startswith("DIMENSION")):
            vlist = get_variables(re.match("DIMENSION( |::)(.+)",line).group(2), vtype='UNKNOWN', attrs=[] )
            dimensions.update(dict( (v['name'], v['dim']) for v in vlist ))
            stream.next_fortran_line()
        elif(re.split('\W+',line,1)[0] in ('ALLOCATABLE', 'EXTERNAL')):
            kw, sep, vlist = re.match("(ALLOCATABLE|EXTERNAL)( |::)(.+)", line).groups()
            assert(not set(vlist.split(",")).intersection(a['name'] for a in ast['args'])) # TODO
            stream.next_fortran_line()
        #
        #   ...these attributes conflict with the "DUMMY" attribute of an argument, they can be safely ignored
        elif(re.match("PARAMETER\((.+)\)", line)):
            stream.next_fortran_line()
        elif(re.match("SAVE( |::)(.+)", line)):
            stream.next_fortran_line()
        #
        #   ...simply skip these lines
        elif(line.startswith("DATA") or
             line.startswith("IMPORT") or
             line == "IMPLICIT NONE"):
            stream.next_fortran_line() # skip line

        elif(re.match("END ?"+whatis, line)):
            break

        # this should be the first executable statement...
        else:
            break

    # here ...
    #   ...all the USE..ONLY statements/attributes should have been collected!
    ast['multiple_imports'] = multiple_imports(ast['uses'])

    #   ...the declaration must have been found for each argument!
    #       (eventually for the return value too)
    commit_arg_type(ast, var_decl_list, dimensions)
    commit_retval_type(ast, var_decl_list, dimensions)

    # skip over subroutines body and ignore nested subroutines
    stack = [whatis]
    while(True):
        line = stream.next_fortran_line()
        m = regex.match(line)
        if(re.match("^END ?SUBROUTINE", line)):
            assert(stack.pop() == "SUBROUTINE")
        elif(re.match("^END ?FUNCTION", line)):
            assert(stack.pop() == "FUNCTION")
        elif(m):
            prefix, inner_whatis, name, args, postfix = m.groups()
            if(
               all((a in ("ELEMENTAL", "PURE", "RECURSIVE") or match_var_decl(a)) for a in prefix.split())
            ):
                stack.append( inner_whatis )

        if(not stack):
            ast['beg_end_loci'] = (beg_locus, stream.locus())
            break

    return(ast)

#===============================================================================
def decode_args(ast, args):
    if(args):
        for a in args.split(","):
            ast['args'].append({'tag':'argument', 'name':a, 'descr':None})

#===============================================================================
def decode_prefix(ast, prefix):
    for a in prefix.split():
        if(match_var_decl(a)):
            assert(ast['tag']=="function")
            assert(get_var_type(a) and get_var_type(a)==a)
            ast['retval']['type'] = a
        else:
            assert(a in ("ELEMENTAL", "PURE", "RECURSIVE"))
            ast['attrs'].append(a)

#===============================================================================
def decode_postfix(ast, postfix):
    for item, what, content in re.findall("((RESULT|BIND)\((.+?)\))", postfix.strip()):
        if( what == "RESULT" ):
            # update the return value name
            ast['retval']['name'] = content
        elif( what == "BIND" ):
            ast.setdefault('post_attrs',[]).append(item)
        else:
            raise Exception("Unknown postfix item: "+item)

#===============================================================================
def commit_arg_type(ast, var_decl_list, dimensions):

    # support list of variables names
    vnamelist = [v['name'] for v in var_decl_list]

    for a in ast['args']:
        # set type and attributes
        v = var_decl_list[ vnamelist.index(a['name']) ]
        a.update( {'type':v['type'], 'attrs':v['attrs'], 'dim':v['dim']} )
        # optionally update dimension
        if( a['name'] in dimensions ):
            if( a['dim'] ):
                assert(a['dim'] == dimensions[a['name']])
            else:
                a['dim'] = dimensions[a['name']]

    # final check
    assert( all(a.has_key('type') for a in ast['args']) )

#===============================================================================
def commit_retval_type(ast, var_decl_list, dimensions):

    # support list of variables names
    vnamelist = [v['name'] for v in var_decl_list]

    a = ast['retval']
    if(a):
        assert(ast['tag'] == 'function')
        assert(not a['name'] in dimensions) # TODO
        if(a['name'] in vnamelist):
            v = var_decl_list[ vnamelist.index(a['name']) ]
            a.update( {'type':v['type'], 'attrs':v['attrs'], 'dim':v['dim']} )
        else:
            # it should have been assigned in decode_prefix()
            assert(a['type'])

#===============================================================================
def commit_args_descr(ast, doxygen):

    assert(not doxygen['var'])

    anames = [arg['name'] for arg in ast['args']]
    for var, descr in doxygen['param'].iteritems():
        if(isinstance(var, basestring) and var in anames):
            ast['args'][anames.index(var)]['descr'] = descr
        elif(isinstance(var, tuple) and all(vv in anames for vv in var)):
            ast.setdefault('__grouped_args_descr__',[]).append( {'grouped_args':var, 'descr':descr} )

    if(ast['retval']):
        doxytag = doxygen['retval']
        if doxytag:
            assert(len(doxytag)==1)
            doxyretvalname = doxytag.keys()[0]
            assert(isinstance(doxyretvalname, basestring))

            a = ast['retval']['name']
            descr = doxytag[a] if(doxyretvalname == a) else ""
            if(not descr):
                # try with plain function name
                a = ast['name']
                descr = doxytag[a] if(doxyretvalname == a) else ""

            ast['retval']['descr'] = descr

#===============================================================================
def commit_type_members_descr(ast, doxygen):
    names = [v['name'] for v in ast['variables']]
    for var, descr in chain(doxygen['var'].iteritems(), doxygen['param'].iteritems()):
        if(isinstance(var, basestring) and var in names):
            ast['variables'][names.index(var)]['descr'] = descr
        elif(isinstance(var, tuple) and all(vv in names for vv in var)):
            ast.setdefault('__grouped_vars_descr__',[]).append( {'grouped_args':var, 'descr':descr} )
    for v in ast['variables']:
        v.setdefault('descr')

#===============================================================================
def parse_doxygen(stream):
    # Initialize
    doxygen = {'author':[], 'brief':[], 'param':{}, 'retval':{}, 'var':{}}

    # Save the beginning line of the subroutine/function
    #   If there is no valid doxygen block before the current subroutine or
    #   function the next *forward* "while(True)" loop will eat the entire
    #   stream till the end! So we save here a checkpoint that we later enforce
    #   not to be crossed.
    checkpoint, line1 = stream.peek_next_fortran_line(give_pos=True)
    assert("MODULE" in line1 or "SUBROUTINE" in line1 or "FUNCTION" in line1 or "TYPE" in line1)
    # advance stream position
    stream.next_fortran_line()

    # go backwards until a non-comment line is found
    while(True):
        try:
            if(not stream.prev_line().startswith("!")):
                break
        except BackwardEndOfFileException:
            break

    # now go forward again until first doxygen line is found
    while(True):
        line_at, line = stream.peek_next_line(give_pos=True)
        if( line.startswith("!>") ):
            break
        else:
            if( line_at==checkpoint ):
                # No doxygen block (base/machine_posix.f90)
               #raise(Exception("no valid doxygen block for SUBR./FUNC. at: "+line1))
                return(doxygen)
            stream.next_line()

    # parse doxygen lines
    entries = []
    while(True):
        line = stream.peek_next_line()
        if(not line.startswith("!>")):
            break
        m = re.search(r"\\([a-z]+)(.*)", line, re.I) # the 2nd group could be an empty string (cf. \note)
        if(m):
            entries.append(list(m.groups()))
        elif line == "!>":
            # Doxygen cmd documentation states:
            #   "A brief description ends when a blank line
            #    or another sectioning command is encountered."
            # so let's start a new fake entry
            entries.append([None, ''])
        else:
            assert(not line.startswith("!> \\")) # are we missing some doxygen tag?
            if(entries):
                entries[-1][1] += " " + line.split("!>",1)[1].strip()
            else: # the Doxygen comment but with no tag
                print '*** Error location: Doxygen block above', stream.locus()
                assert False # Doxy with no tag??
        line = stream.next_line() # advance stream

    # interpret doxygen tags
    for k, v in entries:
        if(k == "brief" or k == "author"):
            if(v.strip() != "..."):
                # Doxygen cmd documentation says:
                #   "If multiple \brief (\author) commands are present they will be joined"
                doxygen[k].append(v.strip())
        elif(k == "param" or k == "retval" or k == "var"):
            # Filter out [in], [out], ... and (optional) right after the tag,
            #   they otherwise will hide the argument...
            v = re.sub("^\[(int|in|out|in[.,]? ?out)\]", "", v, count=1).strip()
            v = re.sub("^\((optinal|optional)\)", "", v, count=1).strip()
            if(v):
                try:
                    doxyvar = parse_doxyvar(v)
                except (SM_UnknownCharException, SM_InvalidStateException):
                    print '*** Error location: Doxygen block above', stream.locus()
                else:
                    doxygen[k].update( doxyvar )
        else:
            pass # ignore all other tags
            #raise(Exception("Strange Doxygen tag:"+k))

    return(doxygen)

#===============================================================================
def parse_doxyvar(tag_content):
    varlist = []
    n_opened, n_square = 0, 0
    state = "start"
    for i, c in enumerate(tag_content):
        my_string = tag_content[:i] + "{" + tag_content[i] + "}" + tag_content[i+1:]

        if(state=="start"):
            if(re.match("\w", c)):
                varlist.append(c)  # 1st char of the variable name
                state = "ini_varname"
            elif(c=='!'):
                # TODO: issue with SUBROUTINE pbe_lda_calc (src/xc/xc_pbe.F):
                #   there is a commented line in between the arguments
                #   so doxify.sh gets confused and adds a "!" \param...
                return(dict())
            else:
                raise SM_UnknownCharException(c,state,my_string)

        elif(state=="ini_varname"):
            if(re.match("\w", c)):
                varlist[-1] += c  # completion of the variable name
            elif(c==' '):
                state = "waiting_for_nextvar_or_descr"
            elif(c==','):
                state = "waiting_for_next_var_name"
            elif(c==':'):
                # TODO: colon used as a separator between
                #   the var name(s) and the description... Tolerated?
                state = "got_varnames"
            elif(c=='('):
                assert(n_opened==0)
                i_round_beg = i
                n_opened = 1
                ppattern = c
                state = "p_open%" + state
            else:
                raise SM_UnknownCharException(c,state,my_string)

        elif(state=="waiting_for_nextvar_or_descr" or state=="got_varnames"):
            if(c==' '):
                # go on stripping consecutive blanks
                pass
            elif(c==','):
                state = "waiting_for_next_var_name"
            elif(c==':'):
                # TODO: colon used as a separator between
                #   the var name(s) and the description... Tolerated?
                state = "got_varnames"
            elif(c=='.'):
                descr = tag_content[i:]
                if(descr=='...'):
                    # could be either ellipsis: "..." (missing description)
                    return(dict())
                elif(re.match("\.(true|false)\.", descr, re.I)):
                    # ...or a description can be started via ".TRUE." / ".FALSE."
                    state = "got_descr"
                    break
                else:
                    raise SM_UnknownCharException(c,state,my_string)
            elif(
              re.match("\w", c) or  # a word here starts the description
              c in (

                "*",    # used to emphasize something, used by doxygen, not yet by ast2doc, see:
                        #   pint_staging.F:123, pint_normalmode.F:217

                "'",    # the description can be started via a quoted string, doxygen seems to handle it
                '"',    # see:
                        #   kpoint_types.F:115, kpoint_types.F:286, kpoint_types.F:394, dbcsr_types.F:419,
                        #   dbcsr_work_operations.F:164, dbcsr_operations.F:1504

                "|",    # is a shourtcut for norm (e.g.: || grad rho ||);
                        # doxygen seems to handle it!
                        # see:
                        #   xc_b97.F:538, xc_b97.F:1559, xc_lyp.F:901,
                        #   xc_xbecke88.F:821, xc_xbecke88_long_range.F:259,
                        #   xc_xbecke88_long_range.F:1033, xc_xbecke88_lr_adiabatic.F:265,
                        #   xc_xbecke88_lr_adiabatic.F:3043, xc_xbeef.F:419

                "=",    # there are some formulas... TODO: how to handle them?

              )
            ):
                descr = tag_content[i:]
                state = "got_descr"
                break
            elif(c=='('):
                # we'll ignore info like (optional): we rely on the proper attribute!
                assert(n_opened==0)
                i_round_beg = i
                n_opened = 1
                ppattern = c
                state = "p_open%" + state
            elif(c=='['):
                # we'll ignore info like [output], ...: we rely on "INTENT" attribute!
                assert(n_square==0)
                i_square_beg = i
                n_square = 1
                sqpattern = c
                state = "sq_open%" + state
            else:
                raise SM_UnknownCharException(c,state,my_string)

        elif(state=="waiting_for_next_var_name"):
            if(c==' '):
                pass
            elif(re.match("\w", c)):
                varlist.append(c)
                state = "ini_varname"
            else:
                raise SM_UnknownCharException(c,state,my_string)

        elif(state.startswith("p_open%")):
            ppattern += c
            if(c == "("):
                n_opened += 1
            elif(c == ")"):
                n_opened -= 1
                if(n_opened==0):
                    # check on previous state
                    prev_state = state.split("%",1)[1]
                    if(not prev_state in ("ini_varname", "waiting_for_nextvar_or_descr")):
                        raise SM_InvalidStateException('previous', prev_state, tag_content)
                    state = prev_state
                    # check on the round braket content
                    if(not ppattern in ("(optional)", )):
                        descr = tag_content[i_round_beg:]
                        state = "got_descr"
                        break

        elif(state.startswith("sq_open%")):
            sqpattern += c
            if(c == "["):
                n_square += 1
            elif(c == "]"):
                n_square -= 1
                if(n_square==0):
                    # check on previous state
                    prev_state = state.split("%",1)[1]
                    if(not prev_state in ("waiting_for_nextvar_or_descr", "got_varnames")):
                        raise SM_InvalidStateException('previous', prev_state, tag_content)
                    state = prev_state
                    # check on the square braket content
                    if(not sqpattern in ("[output]", "[input]", "[OPTIONAL]")):
                        descr = tag_content[i_square_beg:]
                        state = "got_descr"
                        break

        else:
            assert(False) # Unknown state

    # check final state
    if(state != "got_descr"):
        # final state is not allowed
        raise SM_InvalidStateException('final', state, tag_content)

    if len(varlist)==1:
        return {varlist.pop().upper():descr}
    else:
        return {tuple(v.upper() for v in varlist):descr}

#===============================================================================
def parse_use_statement(stream):
    line = stream.next_fortran_line()
    assert(line.startswith("USE "))
    if("ONLY:" in line):
        mod_from, rest = line[4:].split(",", 1)
        only = rest.split("ONLY:",1)[1].split(",")
        only_map = dict([tuple(x.split('=>',1)) if '=>' in x else (x,x) for x in only])
        assert( all(re.match('\w+$',k) for k in only_map) )
        ast = {'tag':'use_stm', 'from':mod_from, 'only':only_map}
    else:
        ast = {'tag':'use_stm', 'from':line[4:].strip()}
    return(ast)


#===============================================================================
class EndOfFileException(Exception):
    def __init__(self):
        pass
class BackwardEndOfFileException(Exception):
    def __init__(self):
        pass

#===============================================================================
class SM_UnknownCharException(Exception):
    def __init__(self, c, state, string):
        print 'SM_Error: char "%c" unknown for state "%s" [%s]' % (c, state, string)
class SM_InvalidStateException(Exception):
    def __init__(self, spec, state, string):
        print 'SM_Error: invalid %s state: "%s" [%s]' % (spec, state, string)

#===============================================================================
class ParserException(Exception):
    def __init__(self, line, locus):
        print 'Strange line: "%s" [%s]' % (line, locus)

#===============================================================================
class InputStream(object):
    def __init__(self, filename):
        cmd = ["cpp", "-nostdinc", "-traditional-cpp", "-D__parallel", filename]
        self.buffer = check_output(cmd)
        self.filename = filename
        self.pos1 = -1
        self.pos2 = -1
        self._cpp_line_index, self._cpp_file_name, self._cpp_beg_pos1, self._cpp_cur_pos1, self._cpp_cur_pos2 = [None]*5

    def next_raw_line(self):
        """Return next line including CPP-comments and advance stream's position"""
        self.pos1 = self.pos2 + 1 # skip over '\n' or ';'
        if(self.pos1>=len(self.buffer)):
            # cannot go beyond the stream's end
            raise EndOfFileException
        self.pos2 = self.buffer.find("\n", self.pos1)
        assert(self.pos2>0)
        line = self.buffer[ self.pos1 : self.pos2 ]
        return(line)

    def prev_raw_line(self):
        """Return next line including CPP-comments and advance stream's position"""
        self.pos2 = self.pos1 - 1  # skip over '\n' or ';'
        assert( self.buffer[self.pos2] == '\n' )
        self.pos1 = self.buffer.rfind("\n", 0, self.pos2) + 1
        if(self.pos1<=0):
            # going on and on backwards, we reached the stream's begin!
            raise BackwardEndOfFileException
        line = self.buffer[ self.pos1 : self.pos2 ]
        return(line)

    def next_line(self):
        """Return next source code line and advance stream's position. Skips CPP's comments."""
        # skip over empty lines or preprocessor lines
        while(True):
            line = self.next_raw_line()
            line_stripped = line.strip()
            if line_stripped.startswith("#"):
                # preprocessor lines are used to get the info needed by locus()
                line_index, file_name = line_stripped.split()[1:3]
                self._cpp_line_index = int(line_index)
                self._cpp_file_name  = file_name[1:-1]
                self._cpp_beg_pos1   = self.pos1
            elif line_stripped:
                return(line)

    def peek_next_line(self, give_pos=False):
        pos1, pos2 = self.pos1, self.pos2
        line = self.next_line()
        next_line_at = self.pos1
        self.pos1, self.pos2 = pos1, pos2
        if( give_pos ): return (next_line_at, line)
        return(line)

    def prev_line(self):
        # skip over preprocessor lines
        while(True):
            line = self.prev_raw_line().strip()
            if line and not line.startswith("#"):
                return(line)

    def peek_prev_line(self):
        pos1, pos2 = self.pos1, self.pos2
        line = self.prev_line()
        self.pos1, self.pos2 = pos1, pos2
        return(line)

    def next_fortran_line(self):
        """Return next logical fortran line and advance stream's position
           Any spaces are removed, except between words.
           Chars and chars are upper cased.
           String are preserved."""
        fortran_line = []
        state = "start"
        line = self.next_line()
        pos1 = self.pos1 # save in case fortran line spans multiple raw line
        i = -1 # current index within line
        while(i+1 < len(line)):
            i += 1; c = line[i]
            if(state == "start"):
                if(c == "'"):
                    state = "str1"
                    fortran_line.append(c)
                elif(c == '"'):
                    state = "str2"
                    fortran_line.append(c)
                elif(c == " "):
                    if(fortran_line and fortran_line[-1].isalpha()): # remove double spaces
                        fortran_line.append(c)
                elif(c == "!"):
                    if(len(fortran_line)==0):
                        i, line = -1, self.next_line() # prefix comment lines
                        pos1 = self.pos1
                    else:
                        break # fortran line ends here
                elif(c == "&"):
                    # skip prefix comment lines after ampersand
                    while(self.peek_next_line().strip().startswith("!")):
                        line = self.next_line()
                    i, line = -1, self.next_line() # line continuation
                    if(line.strip().startswith("&")):
                        # next (continued) line could begin with another echoed ampersand
                        line = line.replace("&"," ",1)
                elif(c==";"):
                    self.pos2 = self.pos1 + i
                    break # end of fortran line
                    #TODO: put some of the line back into stream
                else:
                    if(fortran_line and fortran_line[-1]==" " and not c.isalpha()):
                        fortran_line.pop() # remove spaces except between words
                    fortran_line.append(c.upper())

            elif(state.startswith("str")):
                fortran_line.append(c)
                if((state=="str1" and c=="'") or (state=="str2" and c=='"')):
                    state = "start"
            else:
                assert(False) # unkown state

        # needed to make prev_line() work properly
        self.pos1 = pos1

        # pos[12] saved here to make locus() give the right location when used
        #   after both peek_next_fortran_line() and next_fortran_line()
        self._cpp_cur_pos1, self._cpp_cur_pos2 = self.pos1, self.pos2

        return("".join(fortran_line).strip())

    def peek_next_fortran_line(self, give_pos=False):
        """Peek at next fortran line"""
        pos1, pos2 = self.pos1, self.pos2
        line = self.next_fortran_line()
        self.pos1, self.pos2 = pos1, pos2
        if( give_pos ): return (self._cpp_cur_pos1, line)
        return(line)

    def locus(self):
        """Convert position index into nice location string
           The output location is that of the line returned by the last call of [peek_]next_fortran_line()"""
        fn = path.basename(self._cpp_file_name)
        line_index = self.buffer[ self._cpp_beg_pos1 : self._cpp_cur_pos1 ].count('\n') + self._cpp_line_index -1
        return("%s:%d"%(fn,line_index))


#=============================================================================
def check_output(*popenargs, **kwargs):
    """ backport for Python 2.4 """
    p = subprocess.Popen(stdout=subprocess.PIPE, *popenargs, **kwargs)
    output = p.communicate()[0]
    assert(p.wait() == 0)
    return output

#===============================================================================
if __name__ == '__main__':
    main()

#EOF
