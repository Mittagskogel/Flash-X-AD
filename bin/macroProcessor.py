#!/usr/local/bin python3
import os
import re
import argparse

try:
    from configparser import ConfigParser  # Python 3
except ImportError:
    pass

# standardMacroDefLibrary = ['macroDefsTemplate.ini']
string_char = '"'
comment_char = "!"
macro_keyword = "M"
macro_regex = (
    r"\s*@\s*"
    + re.escape(macro_keyword)
    + r"\s*\w*(?:\s*\(\s*[\w\[\]+-.]*\s*(?:,\s*[\w\[\]+-.]*\s*)*\))?"
)
invocation_regex = (
    r"(?P<indent>\s*)@\s*"
    + re.escape(macro_keyword)
    + r"\s*(?P<key>\w*)(?:\s*\((?P<arglist>\s*[\w\[\]+-.]*\s*(?:,\s*[\w\[\]+-.]*\s*)*)\))?"
)
LINE_CONT_CHARS = ["\\\\", "&&"]


class macroProcessor:
    def __init__(self):
        self.mdict = {}
        self.argdict = {}
        self.typedict = {}
        self.indentdict = {}
        self.sourcedict = {}
        self.keylist = []
        # self.loadDefsList(standardMacroDefLibrary)

    ######### LOADING DEFS ###########
    # Read a file and add contained macro definitions
    # to the macro dictionary.
    def loadDefs(self, filename):
        parser = ConfigParser(comment_prefixes=("#!", "#!!"))
        parser.optionxform = lambda option: option  # read keys case sensitively
        if filename is not None:
            dirName = os.path.dirname(os.path.abspath(filename))
            parser.read(filename)
            for section in parser.sections():
                if section not in self.keylist:
                    self.keylist.append(section)
                    self.sourcedict[section] = dirName
                else:
                    previousPath = self.sourcedict[section]
                    if not (
                        previousPath.startswith(dirName)
                        or dirName.startswith(previousPath)
                    ):
                        if not (
                            "source/Simulation" in dirName or "/bin" in previousPath
                        ):
                            raise SyntaxError(
                                "{} defined in parellel directories, can't inherit properly".format(
                                    section
                                )
                            )
                    self.sourcedict[
                        section
                    ] = dirName  # overwrite keyword with def from new location

                self.mdict[section] = parser.get(section, "definition").strip()

                try:
                    args = parser.get(section, "args")
                    self.argdict[section] = [
                        item.strip() for item in args.strip().split(",")
                    ]
                except:
                    self.argdict[section] = []

                try:
                    self.typedict[section] = parser.get(section, "type")
                except:
                    self.typedict[section] = ""
                try:
                    indents = parser.get(section, "line_indents").strip().split(",")
                    self.indentdict[section] = [int(i) for i in indents]
                except:
                    self.indentdict[section] = [0]

    # Load a whole list of files.
    def loadDefsList(self, filenames):
        for filename in filenames:
            self.loadDefs(filename)

    ####### EXPAND INVOCATIONS OF MACROS #############

    def getMacroDef(self, key, args=[], indent=""):
        # get definition and replace args
        definition = self.mdict[key]
        arglist = self.argdict[key]
        if len(arglist) != len(args):
            msg = f"Error: macro: {key} needs {len(arglist)} argument(s), but {len(args)} given."
            raise SyntaxError(msg)
        for i, arg in enumerate(args):
            if i < len(arglist):
                arg_re = r"\b" + arglist[i] + r"\b"  # don't substitute substrings
                if (
                    "[" in arglist[i] and "]" in arglist[i]
                ):  # if arglist[i] is a [listitem]
                    arg_re = r"\B" + re.escape(arglist[i]) + r"\B"
                definition = re.sub(arg_re, arg, definition)

        # add appropriate indent to all lines
        def_lines = definition.split("\n")
        if len(def_lines[0].strip()) > 0:
            if def_lines[0].strip()[0] == "#":
                # ensure preprocessor directives are not inserted inline
                def_lines[0] = "\n" + def_lines[0]
        j = 0  # track place in per-line indent list
        n = len(self.indentdict[key])
        for i in range(len(def_lines)):
            lineStripped = def_lines[i].strip()
            lead = indent + " " * self.indentdict[key][j]
            if len(lineStripped) > 0:
                if lineStripped[0] == "#":
                    lead = ""
            def_lines[i] = lead + def_lines[i]
            if j < n - 1:
                j = j + 1
        definition = "\n".join(def_lines)

        return definition

    def _argListItemHandler(self, arglist):
        # if arglist is empty, return empty list
        if len(arglist) == 1 and arglist[0] == "":
            return []

        newArgList = []
        listitem = []
        # if arglist contains args enclosed with [ and ],
        # merge them into a single list element
        for arg in arglist:
            arg = arg.strip()
            if listitem:
                listitem.append(arg)
            elif arg.startswith("["):
                listitem = [arg.lstrip("[")]
            else:
                newArgList.append(arg)

            if arg.endswith("]"):
                listitem[-1] = listitem[-1].rstrip("]")
                newArgList.append(", ".join(listitem))
                listitem = []

        return newArgList

    def expandMacro(self, invocation, macroStack):
        expansion = invocation
        keymatch = False

        # use regex to get parts of invocation
        invocation_parts = re.match(invocation_regex, invocation)
        macroName = invocation_parts.group("key")
        indent = invocation_parts.group("indent")

        # check to make sure recursive calls don't cause infinite loops
        if macroName in macroStack:
            mlist = ", ".join(macroStack)
            msg = "Error: macro(s): (%s) causing a loop via macro recursion." % mlist
            raise SyntaxError(msg)

        for key in self.keylist:
            if macroName == key:
                args = []
                if len(self.argdict[key]) > 0:
                    argtext = invocation_parts.group("arglist")
                    if argtext is None:
                        msg = "Error: argument list expected for macro %s" % macroName
                        raise SyntaxError(msg)
                    args = argtext.split(",")
                    args = self._argListItemHandler(args)

                expansion = self.getMacroDef(macroName, args, indent)
                keymatch = True
                break

        # if expansion has a recursive macro, process lines again
        recursion = len(re.findall(macro_regex, expansion)) > 0
        if recursion and keymatch:
            macroStack.append(macroName)
            expansionLines = expansion.split("\n")
            for i, line in enumerate(expansionLines):
                expansionLines[i] = self.processLine(line, macroStack)
            expansion = "\n".join(expansionLines)

        return expansion

    ######### PROCESSING FILES  #################

    # Process a single line
    def processLine(self, lineIn, macroStack=None):
        if macroStack is None:
            macroStack = []

        lineOut = lineIn

        # scan line for comments and strip them
        lineStripped = lineIn
        inString = False
        for i, ch in enumerate(lineIn):
            if ch == string_char:
                inString = not inString
            if ch == comment_char and not inString:
                lineStripped = lineIn[0:i]
                break

        if len(lineStripped.split()) >= 1:
            # find matches for macro invocation regex
            invocation_list = re.findall(macro_regex, lineStripped)

            # expand each invocation and replace
            for invocation in invocation_list:
                expansion = self.expandMacro(invocation, macroStack)
                lineOut = lineOut.replace(invocation, expansion, 1)
                # if the macro-processed line has only whitespaces
                # or it just has one &,
                # return a blank line
                if lineOut.strip() == "&" or lineOut.strip() == "":
                    lineOut = ""

        return lineOut

    # strip macro lines with line continuation
    def _continuationLine(self, fin, cont_chars):
        lines = []
        cont_regex = rf"(?:{'|'.join(cont_chars)})\s*?\n"
        for line in fin:
            if line.lstrip().startswith("@M"):
                while re.search(cont_regex, line):
                    line = re.sub(cont_regex, "", line) + next(fin).lstrip()
            lines.append(line)
        return lines


    # delete trailing "&" in compiler directives
    def _removeTrailingAmpersand(self, filename):
        directive_pattern = re.compile(r"^\s*!\$(acc|omp)")

        with open(filename, "r") as file:
            lines = file.readlines()

        modified_lines = []
        for i in range(len(lines)):
            line = lines[i].rstrip()  # Remove trailing whitespace
            is_directive = directive_pattern.match(line) is not None

            # Process only if the line is a compiler directive
            if is_directive and line.endswith("&") and i + 1 < len(lines):
                next_line = lines[i + 1].lstrip()
                next_line_is_not_directive = (
                    directive_pattern.match(next_line) is None
                )

                if next_line_is_not_directive:
                    line = line[:-1].rstrip()  # Remove the '&'

            modified_lines.append(line)

        # Write the modified lines back to the file
        with open(filename, "w") as file:
            for line in modified_lines:
                file.write(f"{line}\n")


    # Process a whole file
    def convertFile(self, filename, output):
        with open(output, "w") as f:
            # lines = open(filename).readlines()
            fin = open(filename)
            lines = self._continuationLine(fin, LINE_CONT_CHARS)
            for line in lines:
                f.write(self.processLine(line))
        # post processing
        self._removeTrailingAmpersand(output)


###########################################################
# Assuming ext starts with `.`
def makeVariantName(base, var, ext):
    if var == "" or var.lower() == "null":
        outfile = base + ext
    else:
        outfile = base + "_" + var + ext
    return outfile


# Can run directly to process some file.
def main():
    parser = argparse.ArgumentParser(description="Macro Preprocessor for FLASH")
    parser.add_argument(
        "--filename",
        "-f",
        type=str,
        help="file to convert, if none is specified do all in current dir",
    )
    parser.add_argument("--output", "-o", type=str, help="output filename")
    parser.add_argument(
        "--macroDefs", "-m",
        type=str, action="append",
        help="file with list of extra macro definitions"
    )
    args = parser.parse_args()

    m = macroProcessor()
    if args.macroDefs is not None:
        m.loadDefsList(args.macroDefs)

    # if args.filename is None:
    #     processFilesInCurrentDir()

    else:
        if args.macroDefs is None:
            defs_in_dir = [
                f for f in os.listdir(".") if (os.path.isfile(f) and (".ini" in f))
            ]
            m.loadDefsList(defs_in_dir)
    # Convert just the given file
    if args.output is None:
        if args.filename.count(".F90-mc") >= 1:
            args.output = args.filename.replace(".F90-mc", ".F90")
        else:
            args.output = args.filename + ".out"
    m.convertFile(args.filename, args.output)


if __name__ == "__main__":
    main()
