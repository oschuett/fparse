#!/usr/bin/env python
# -*- coding: utf-8 -*-

import subprocess
import sys
import re
from os import path
from pprint import pprint

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
#    doxygen = parse_doxygen(stream)
#    print doxygen

    # parse opening line
    kw, name = stream.next_fortran_line().split()
    assert(kw == "MODULE")

    ast = {'tag':'module', 'name':name, 'uses':[], 'publics':[], 'types':[], 'subroutines':[], 'functions':[], 'interfaces':[]}

    # parse stuff before CONTAINS
    private = False
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
        elif(match_var_decl(line)):
            # these could be public if PRIVATE was not (yet) found
            #print "Found variable declaration"
            stream.next_fortran_line() # skip line
        elif(line.startswith("TYPE ")):
            t = parse_type(stream)
            ast['types'].append(t)
        elif(line.startswith("PUBLIC::")):
            syms = parse_public_statement(stream)
            ast['publics'].extend(syms)
        elif(line.startswith("INTERFACE ")):
            a =  parse_interface(stream)
            ast['interfaces'].append(a)
        elif(line == "CONTAINS"):
            stream.next_fortran_line() # skip line
            break
        elif(re.match("^END ?MODULE", line)):
            break # not every module has a CONTAINS
        else:
            raise ParserException(line, stream.locus())

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
            break # found module's closing line
        else:
            raise ParserException(line, stream.locus())

    return(ast)

#===============================================================================
def parse_interface(stream):
    line = stream.next_fortran_line()
    assert(line.startswith("INTERFACE"))
    name = line.split(" ",1)[1]
    ast = {'tag':'interface', 'name':name}
    while(not re.match("^END ?INTERFACE", stream.next_fortran_line())):
        pass
    return(ast)

#===============================================================================
def parse_type(stream):
    line = stream.next_fortran_line()
    assert(line.startswith("TYPE"))
    name = line.split(" ",1)[1]
    ast = {'tag':'type', 'name':name}
    while(not re.match("^END ?TYPE", stream.next_fortran_line())):
        pass
    return(ast)

#===============================================================================
def parse_public_statement(stream):
    ast = []
    line = stream.next_fortran_line() # skip line
    assert(line.startswith("PUBLIC::"))
    is_api = stream.peek_prev_line().replace(" ","").startswith("!API")
    syms = line.split("::",1)[1].split(",")
    ast = [{'tag':'public', 'name':s, 'is_api':is_api} for s in syms]
    return(ast)

#===============================================================================
def match_var_decl(line):
    for t in ("CHARACTER", "INTEGER", "REAL", "COMPLEX", "LOGICAL", "TYPE("):
        if(line.startswith(t)):
            return(True)
    return(False)

#===============================================================================
def parse_routine(stream):
    doxygen = parse_doxygen(stream)

    line1 = stream.next_fortran_line()
    regex = '(.*)(FUNCTION|SUBROUTINE) (\w+)(?:\((.*?)\))?(.*)' # it is used afterwards too...
    """      |   |                     |    |    |        |
             |   |                     |    |    |        .
             |   |                     |    |    |         \..postfix: RESULT(..) | BIND(..)
             |   |                     |    |    .
             |   |                     |    |     \..arguments list
             |   |                     |    .
             |   |                     |     \..[non-capturing group: the "()" empty list of arguments can be omitted]
             |   .                     .
             |    \..whatis             \..name
             .
              \..prefix: RECURSIVE | PURE | ...
    """

    m = re.match(regex, line1)
    prefix, whatis, name, args, postfix = m.groups()
    ast = {'tag':whatis.lower(), 'name':name, 'descr':doxygen['brief'], 'args':[], 'attrs':[]}

    if(args):
        for a in args.split(","):
            descr = doxygen['param'][a] if(doxygen['param'].has_key(a)) else ""
            ast['args'].append({'tag':'argument', 'name':a, 'descr':descr})

    if(postfix):
        for item, what, content in re.findall("((RESULT|BIND)\((.+?)\))", postfix.strip()):
            if( what == "RESULT" ):
                descr = doxygen['retval'][content] if(doxygen['retval'].has_key(content)) else ""
                ast['retval'] = {'tag':'return_value', 'name':content, 'descr':descr}
            elif( what == "BIND" ):
                ast['attrs'].append(item)
            else:
                raise Exception("Unknown postfix item: "+item)

    if(prefix):
        retval_type = None
        for a in prefix.split():
            if(match_var_decl(a)):
                assert(not retval_type and whatis=="FUNCTION")
                retval_type = a
                continue
            assert(a in ("ELEMENTAL", "PURE", "RECURSIVE"))
            ast['attrs'].append(a)
        if(retval_type):
            assert(not 'retval' in ast) # check meaningful if prefix is processed after postfix
            retval = name.lower()
            descr = doxygen['retval'][retval] if(doxygen['retval'].has_key(retval)) else ""
            ast['retval'] = {'tag':'return_value', 'name':name, 'descr':descr, 'type':retval_type}

    # parse variable declarations
    while(True):
        line = stream.peek_next_fortran_line()
        if(match_var_decl(line)):
            stream.next_fortran_line() # skip line
        else:
            break

    # skip over subroutines body and ignore nested subroutines
    stack = [ast["tag"].upper()]
    while(True):
        line = stream.next_fortran_line()
        m = re.match(regex, line)
        if(re.match("^END ?SUBROUTINE", line)):
            assert(stack.pop() == "SUBROUTINE")
        elif(re.match("^END ?FUNCTION", line)):
            assert(stack.pop() == "FUNCTION")
        elif(m):
            prefix, whatis, name, args, postfix = m.groups()
            if(
               all(
                 [(a in ("ELEMENTAL", "PURE", "RECURSIVE") or match_var_decl(a)) for a in prefix.split()]
               )
            ):
                stack.append( whatis )

        if(not stack):
            break

    return(ast)


#===============================================================================
def parse_doxygen(stream):
    # Save the beginning line of the subroutine/function
    checkpoint, line1 = stream.peek_next_fortran_line(give_pos=True)

    # go backwards until a non-comment line is found
    while(stream.prev_line().startswith("!")):
        pass

    # now go forward again until first doxygen line is found
    while(True):
        line_at, line = stream.peek_next_line(give_pos=True)
        if( line.startswith("!>") ):
            break
        else:
            if( line_at==checkpoint ):
                # No doxygen block (base/machine_posix.f90)
               #raise(Exception("no valid doxygen block for SUBR./FUNC. at: "+line1))
                return {'brief':'', 'param':{}, 'retval':{}}
            stream.next_line()

    # parse doxygen lines
    entries = []
    while(True):
        line = stream.peek_next_line()
        if(not line.startswith("!>")):
            break
        m = re.search(r"\\(\w+) (.*)", line)
        if(m):
            entries.append(list(m.groups()))
        else:
            if(entries):
                entries[-1][1] += " " + line.split("!>",1)[1].strip()
        line = stream.next_line() # advance stream

    # interpret doxygen tags
    doxygen = {'brief':'', 'param':{}, 'retval':{}}
    for k, v in entries:
        if(k == "brief" or k == "author"):
            doxygen[k] = v
        elif(k == "param"):
            if(not v): continue # some tmc/tmc_*.F files have "!> \param ", skipped by Doxygen
            if(not " " in v): continue # common/cp_files.F: "!> \retval exist"
            a, b = v.split(" ", 1)
            if(b != "..."):
                doxygen[k][a] = b
        else:
            pass # ignore all other tags
            #raise(Exception("Strange Doxygen tag:"+k))

    return(doxygen)

#===============================================================================
def parse_use_statement(stream):
    line = stream.next_fortran_line()
    assert(line.startswith("USE "))
    if("ONLY:" in line):
        mod_from, rest = line[4:].split(",", 1)
        only = rest.split("ONLY:",1)[1].split(",")
        only_map = dict([(x,x) for x in only])
        ast = {'tag':'use_stm', 'from':mod_from, 'only':only_map}
    else:
        ast = {'tag':'use_stm', 'from':line[4:].strip()}
    return(ast)


#===============================================================================
class ParserException(Exception):
    def __init__(self, line, locus):
        print 'Strange line: "%s"' % line


#===============================================================================
class InputStream(object):
    def __init__(self, filename):
        cmd = ["cpp", "-nostdinc", "-traditional-cpp", "-D__parallel", filename]
        self.buffer = check_output(cmd)
        self.filename = filename
        self.pos1 = -1
        self.pos2 = -1

    def next_raw_line(self):
        """Return next line including CPP-comments and advance stream's position"""
        self.pos1 = self.pos2 + 1 # skip over '\n' or ';'
        assert(self.pos1<len(self.buffer)) # cannot go beyond the stream's end
        self.pos2 = self.buffer.find("\n", self.pos1)
        assert(self.pos2>0)
        line = self.buffer[ self.pos1 : self.pos2 ]
        return(line)

    def prev_raw_line(self):
        """Return next line including CPP-comments and advance stream's position"""
        self.pos2 = self.pos1 - 1  # skip over '\n' or ';'
        self.pos1 = self.buffer.rfind("\n", 0, self.pos2)
        assert(self.pos1>0)
        line = self.buffer[ self.pos1 : self.pos2 ]
        return(line)

    def next_line(self):
        """Return next source code line and advance stream's position. Skips CPP's comments."""
        # skip over preprocessor lines
        while(True):
            line = self.next_raw_line()
            line_stripped = line.strip()
            if line_stripped and not line_stripped.startswith("#"):
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
           String are preserverd."""
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
                    i, line = -1, self.next_line() # line continuation
                elif(c==";"):
                    self.pos2 = self.pos1 + i
                    break # end of fortan line
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

        self.pos1 = pos1  # needed to make prev_line() work properly
        return("".join(fortran_line).strip())

    def peek_next_fortran_line(self, give_pos=False):
        """Peek at next fortran line"""
        pos1, pos2 = self.pos1, self.pos2
        line = self.next_fortran_line()
        next_line_at = self.pos1
        self.pos1, self.pos2 = pos1, pos2
        if( give_pos ): return (next_line_at, line)
        return(line)

    def locus(self):
        """Convert position index into nice location string"""
        fn = path.basename(self.filename)
        return("%s:%d"%(fn,self.pos1))


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
