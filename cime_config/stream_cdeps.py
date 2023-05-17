"""
Interface to the streams.xml style files.  This class inherits from GenericXML.py

stream files predate cime and so do not conform to entry id format
"""
import datetime
import re
import hashlib

from CIME.XML.standard_module_setup import *
from CIME.XML.generic_xml import GenericXML
from CIME.utils import expect

# pragma pylint: disable=undefined-variable
logger = logging.getLogger(__name__)

_var_ref_re = re.compile(r"\$(\{)?(?P<name>\w+)(?(1)\})")

_ymd_re = re.compile(r"%(?P<digits>[1-9][0-9]*)?y(?P<month>m(?P<day>d)?)?")

_stream_file_template = """
  <stream_info name="{streamname}">
   <taxmode>{stream_taxmode}</taxmode>
   <tintalgo>{stream_tintalgo}</tintalgo>
   <readmode>{stream_readmode}</readmode>
   <mapalgo>{stream_mapalgo}</mapalgo>
   <dtlimit>{stream_dtlimit}</dtlimit>
   <year_first>{stream_year_first}</year_first>
   <year_last>{stream_year_last}</year_last>
   <year_align>{stream_year_align}</year_align>
   <vectors>{stream_vectors}</vectors>
   <meshfile>{stream_meshfile}</meshfile>
   <lev_dimname>{stream_lev_dimname}</lev_dimname>
   <datafiles>
      {stream_datafiles}
   </datafiles>
   <datavars>
      {stream_datavars}
   </datavars>
   <offset>{stream_offset}</offset>
  </stream_info>

"""

valid_values = {}
valid_values["mapalgo"] = ["bilinear", "nn", "redist", "mapconsd", "mapconf", "none"]
valid_values["tintalgo"] = ["lower", "upper", "nearest", "linear", "coszen"]
valid_values["taxmode"] = ["cycle", "extend", "limit"]

xml_scalar_names = [
    "stream_meshfile",
    "stream_mapalgo",
    "stream_tintalgo",
    "stream_taxmode",
    "stream_dtlimit",
]


class StreamCDEPS(GenericXML):
    def __init__(self, infile, schema):
        """
        Initialize a CDEPS stream object
        """
        logger.debug("Verifying using schema {}".format(schema))
        GenericXML.__init__(self, infile, schema)
        self.stream_nodes = None

        if os.path.exists(infile):
            GenericXML.read(self, infile, schema)

    def create_stream_xml(
        self,
        stream_names,
        case,
        streams_xml_file,
        data_list_file,
        user_mods_file,
        available_neon_data=None,
    ):
        """
        Create the stream xml file and append the required stream input data to the input data list file
        available_neon_data is an optional list of NEON tower data available for the given case, if provided
        this data will be used to populate the NEON streamdata list
        """

        # determine if there are user mods
        lines_input = []
        expect(
            os.path.isfile(user_mods_file),
            "No file {} found in case directory".format(user_mods_file),
        )
        with open(user_mods_file, "r", encoding="utf-8") as stream_mods_file:
            lines_input = stream_mods_file.readlines()
        stream_mod_dict = {}
        n = len(lines_input)

        index = 0
        lines_input_new = []
        while index < n:
            line = lines_input[index].strip()
            if line.startswith("!") or (not line):
                index = index + 1
                continue
            while line[-1] == "\\":
                index += 1
                if index < n:
                    line = line[:-1].strip() + " " + lines_input[index].strip()
                else:
                    line = line.replace("\\", "").strip()
                    break
                # endif
            # end while
            index += 1
            line = case.get_resolved_value(line)
            lines_input_new.append(line)
        # end while

        for line in lines_input_new:
            # read in a single line in user_nl_xxx_streams and parse it if it is not a comment
            stream_mods = [x.strip() for x in line.strip().split(":", maxsplit=1) if x]
            expect(
                len(stream_mods) == 2,
                "input stream mod can only be of the form streamname:var=value(s)",
            )
            stream, varmod = stream_mods
            expect(
                stream in stream_names,
                "{} contains a streamname '{}' that is not part of valid streamnames {}".format(
                    user_mods_file, stream, stream_names
                ),
            )
            if stream not in stream_mod_dict:
                stream_mod_dict[stream] = {}
            # var=value and check the validity
            varmod_args = [x.strip() for x in varmod.split("=") if x]
            expect(
                len(varmod_args) == 2,
                "input stream mod can only be of the form streamname:var=value(s)",
            )
            # allow multiple entries for varmod_args, most recent wins
            varname, varval = varmod_args
            if varname in stream_mod_dict[stream]:
                logger.warning(
                    "varname {} is already in stream mod dictionary".format(varname)
                )

            if varname == "datavars" or varname == "datafiles":
                if varname == "datavars":
                    varvals = [
                        "<var>{}</var>".format(x.strip())
                        for x in varval.split(",")
                        if x
                    ]
                if varname == "datafiles":
                    varvals = [
                        "<file>{}</file>".format(x.strip())
                        for x in varval.split(",")
                        if x
                    ]
                varval = "      " + "\n      ".join(varvals)
                varval = varval.strip()
            stream_mod_dict[stream][varname] = varval

        # write header of stream file
        with open(streams_xml_file, "w", encoding="utf-8") as stream_file:
            stream_file.write('<?xml version="1.0"?>\n')
            stream_file.write('<file id="stream" version="2.0">\n')

        # write contents of stream file
        for stream_name in stream_names:
            # include NEON.$NEONSITE non-precipitation data streams whether use PRISM or NEON precip
            if stream_name.startswith("NEON.") and ("PRECIP" not in stream_name):
                self.stream_nodes = super(StreamCDEPS, self).get_child(
                    "stream_entry",
                    {"name": "NEON.$NEONSITE"},
                    err_msg="No stream_entry {} found".format(stream_name),
                )
            elif stream_name.startswith("NEON.PRISM_PRECIP"):
                self.stream_nodes = super(StreamCDEPS, self).get_child(
                    "stream_entry",
                    {"name": "NEON.PRISM_PRECIP.$NEONSITE"},
                    err_msg="No stream_entry {} found".format(stream_name),
                )
            elif stream_name.startswith("NEON.NEON_PRECIP"):
                self.stream_nodes = super(StreamCDEPS, self).get_child(
                    "stream_entry",
                    {"name": "NEON.NEON_PRECIP.$NEONSITE"},
                    err_msg="No stream_entry {} found".format(stream_name),
                )
            elif stream_name.startswith("CLM_USRDAT."):
                self.stream_nodes = super(StreamCDEPS, self).get_child(
                    "stream_entry",
                    {"name": "CLM_USRDAT.$CLM_USRDAT_NAME"},
                    err_msg="No stream_entry {} found".format(stream_name),
                )
            elif stream_name:
                self.stream_nodes = super(StreamCDEPS, self).get_child(
                    "stream_entry",
                    {"name": stream_name},
                    err_msg="No stream_entry {} found".format(stream_name),
                )

            # determine stream_year_first and stream_year_list
            data_year_first, data_year_last = self._get_stream_first_and_last_dates(
                self.stream_nodes, case
            )

            # now write the data model streams xml file
            stream_vars = {}
            stream_vars["streamname"] = stream_name
            attributes = {}
            for node in self.get_children(root=self.stream_nodes):
                node_name = node.xml_element.tag.strip()

                if node_name == "stream_datavars":
                    # Get the resolved stream data variables
                    stream_vars[node_name] = None
                    for child in self.get_children(root=node):
                        datavars = child.xml_element.text.strip()
                        datavars = self._resolve_values(case, datavars)
                        datavars = self._sub_glc_fields(datavars, case)
                        datavars = self._add_xml_delimiter(datavars.split("\n"), "var")
                        if stream_vars[node_name]:
                            stream_vars[node_name] = (
                                stream_vars[node_name] + "\n      " + datavars.strip()
                            )
                        else:
                            stream_vars[node_name] = datavars.strip()
                        # endif

                elif node_name == "stream_datafiles":
                    # Get the resolved stream data files
                    stream_vars[node_name] = ""
                    stream_datafiles = ""
                    for child in self.get_children(root=node):
                        if (
                            available_neon_data
                            and stream_name.startswith("NEON")
                            and ("PRISM" not in stream_name)
                        ):
                            rundir = case.get_value("RUNDIR")
                            for neon in available_neon_data:
                                stream_datafiles += (
                                    os.path.join(rundir, "inputdata", "atm", neon)
                                    + "\n"
                                )
                        else:
                            stream_datafiles = child.xml_element.text
                            stream_datafiles = self._resolve_values(
                                case, stream_datafiles
                            )
                        # endif neon
                        if (
                            "first_year" in child.xml_element.attrib
                            and "last_year" in child.xml_element.attrib
                        ):
                            value = child.xml_element.get("first_year")
                            value = self._resolve_values(case, value)
                            stream_year_first = int(value)
                            value = child.xml_element.get("last_year")
                            value = self._resolve_values(case, value)
                            stream_year_last = int(value)
                            year_first = max(stream_year_first, data_year_first)
                            year_last = min(stream_year_last, data_year_last)
                            if "filename_advance_days" in child.xml_element.attrib:
                                filename_advance_days = int(
                                    child.xml_element.get("filename_advance_days")
                                )
                            else:
                                filename_advance_days = 0
                            stream_datafiles = self._sub_paths(
                                stream_name,
                                stream_datafiles,
                                year_first,
                                year_last,
                                filename_advance_days,
                            )
                            stream_datafiles = stream_datafiles.strip()
                        # endif
                        if stream_vars[node_name]:
                            stream_vars[
                                node_name
                            ] += "\n      " + self._add_xml_delimiter(
                                stream_datafiles.split("\n"), "file"
                            )
                        else:
                            stream_vars[node_name] = self._add_xml_delimiter(
                                stream_datafiles.split("\n"), "file"
                            )
                        # endif
                elif node_name in xml_scalar_names:
                    attributes["model_grid"] = case.get_value("GRID")
                    attributes["compset"] = case.get_value("COMPSET")
                    value = self._get_value_match(
                        node, node_name[7:], attributes=attributes
                    )
                    if value:
                        value = self._resolve_values(case, value)
                        value = value.strip()
                    stream_vars[node_name] = value

                elif node_name.strip():
                    # Get the other dependencies
                    self._add_value_to_dict(stream_vars, case, node)

            # substitute user_mods in generated stream file (i.e. stream_vars)
            mod_dict = {}
            if stream_vars["streamname"] in stream_mod_dict:
                mod_dict = stream_mod_dict[stream_vars["streamname"]]
                for var_key in mod_dict:
                    expect(
                        "stream_" + var_key in stream_vars,
                        "stream mod {} is not a valid name in {}".format(
                            var_key, user_mods_file
                        ),
                    )
                    if var_key in valid_values:
                        expect(
                            mod_dict[var_key] in valid_values[var_key],
                            "{} can only have values of {} for stream {} in file {}".format(
                                var_key,
                                valid_values[var_key],
                                stream_name,
                                user_mods_file,
                            ),
                        )
                    stream_vars["stream_" + var_key] = mod_dict[var_key]
                    if var_key == "datafiles":
                        stream_datafiles = mod_dict[var_key]
                        stream_datafiles = stream_datafiles.replace(
                            "<file>", ""
                        ).replace("</file>", "")

            # append to stream xml file
            stream_file_text = _stream_file_template.format(**stream_vars)
            with open(streams_xml_file, "a", encoding="utf-8") as stream_file:
                stream_file.write(case.get_resolved_value(stream_file_text))

            # append to input_data_list
            if stream_vars["stream_meshfile"]:
                stream_meshfile = stream_vars["stream_meshfile"].strip()
                self._add_entries_to_inputdata_list(
                    stream_meshfile, stream_datafiles.split("\n"), data_list_file
                )

        # write close of stream xml file
        with open(streams_xml_file, "a", encoding="utf-8") as stream_file:
            stream_file.write("</file>\n")

    def _get_stream_first_and_last_dates(self, stream, case):
        """
        Get first and last dates for data for the stream file
        """
        for node in self.get_children(root=stream):
            if node.xml_element.tag == "stream_year_first":
                data_year_first = node.xml_element.text.strip()
                data_year_first = int(self._resolve_values(case, data_year_first))
            if node.xml_element.tag == "stream_year_last":
                data_year_last = node.xml_element.text.strip()
                data_year_last = int(self._resolve_values(case, data_year_last))
        return data_year_first, data_year_last

    def _add_entries_to_inputdata_list(
        self, stream_meshfile, stream_datafiles, data_list_file
    ):
        """
        Appends input data information entries to input data list file
        and writes out the new file
        """
        lines_hash = self._get_input_file_hash(data_list_file)
        with open(data_list_file, "a", encoding="utf-8") as input_data_list:
            # write out the mesh file separately
            string = "mesh = {}\n".format(stream_meshfile)
            hashValue = hashlib.md5(string.rstrip().encode("utf-8")).hexdigest()
            if hashValue not in lines_hash:
                input_data_list.write(string)
            # now append the stream_datafile entries
            for i, filename in enumerate(stream_datafiles):
                if filename.strip() == "":
                    continue
                string = "file{:d} = {}\n".format(i + 1, filename.strip())
                hashValue = hashlib.md5(string.rstrip().encode("utf-8")).hexdigest()
                if hashValue not in lines_hash:
                    input_data_list.write(string)

    def _get_input_file_hash(self, data_list_file):
        """
        Determine a hash for the input data file
        """
        lines_hash = set()
        if os.path.isfile(data_list_file):
            with open(data_list_file, "r", encoding="utf-8") as input_data_list:
                for line in input_data_list:
                    hashValue = hashlib.md5(line.rstrip().encode("utf-8")).hexdigest()
                    logger.debug("Found line {} with hash {}".format(line, hashValue))
                    lines_hash.add(hashValue)
        return lines_hash

    def _get_value_match(self, node, child_name, attributes=None, exact_match=False):
        """
        Get the first best match for multiple tags in child_name based on the
        attributes input

        <values...>
          <value A="a1">X</value>
          <value A="a2">Y</value>
          <value A="a3" B="b1">Z</value>
         </values>
        </values>
        """
        # Store nodes that match the attributes and their scores.
        matches = []
        nodes = self.get_children(child_name, root=node)
        for vnode in nodes:
            # For each node in the list start a score.
            score = 0
            if attributes:
                for attribute in self.attrib(vnode).keys():
                    # For each attribute, add to the score.
                    score += 1
                    # If some attribute is specified that we don't know about,
                    # or the values don't match, it's not a match we want.
                    if exact_match:
                        if attribute not in attributes or attributes[
                            attribute
                        ] != self.get(vnode, attribute):
                            score = -1
                            break
                    else:
                        if attribute not in attributes or not re.search(
                            self.get(vnode, attribute), attributes[attribute]
                        ):
                            score = -1
                            break

            # Add valid matches to the list.
            if score >= 0:
                matches.append((score, vnode))

        if not matches:
            return None

        # Get maximum score using either a "last" or "first" match in case of a tie
        max_score = -1
        mnode = None
        for score, node in matches:
            # take the *first* best match
            if score > max_score:
                max_score = score
                mnode = node

        return self.text(mnode)

    def _add_value_to_dict(self, stream_dict, case, node):
        """
        Adds a value to the input stream dictionary needed for the
        stream file output Returns the uppdated stream_dict
        """
        name = node.xml_element.tag
        value = node.xml_element.text
        value = self._resolve_values(case, value)
        stream_dict[name] = value
        return stream_dict

    def _resolve_values(self, case, value):
        """
        Substitues $CASEROOT env_xxx.xml variables if they appear in "value"
        Returns a string
        """
        match = _var_ref_re.search(value)
        while match:
            env_val = case.get_value(match.group("name"))
            expect(
                env_val is not None,
                "Namelist default for variable {} refers to unknown XML variable {}.".format(
                    value, match.group("name")
                ),
            )
            value = value.replace(match.group(0), str(env_val), 1)
            match = _var_ref_re.search(value)
        return value

    def _sub_glc_fields(self, datavars, case):
        """Substitute indicators with given values in a list of fields.

        Replace any instance of the following substring indicators with the
        appropriate values:
            %glc = two-digit GLC elevation class from 00 through glc_nec

        The difference between this function and `_sub_paths` is that this
        function is intended to be used for variable names (especially from the
        `strm_datvar` defaults), whereas `_sub_paths` is intended for use on
        input data file paths.

        Returns a string.

        Example: If `_sub_fields` is called with an array containing two
        elements, each of which contains two strings, and glc_nec=3:
             foo               bar
             s2x_Ss_tsrf%glc   tsrf%glc
         then the returned array will be:
             foo               bar
             s2x_Ss_tsrf00     tsrf00
             s2x_Ss_tsrf01     tsrf01
             s2x_Ss_tsrf02     tsrf02
             s2x_Ss_tsrf03     tsrf03
        """
        lines = datavars.split("\n")
        new_lines = []
        for line in lines:
            if not line:
                continue
            if "%glc" in line:
                if case.get_value("GLC_NEC") == 0:
                    glc_nec_indices = []
                else:
                    glc_nec_indices = range(case.get_value("GLC_NEC") + 1)
                for i in glc_nec_indices:
                    new_lines.append(line.replace("%glc", "{:02d}".format(i)))
            else:
                new_lines.append(line)
        return "\n".join(new_lines)

    @staticmethod
    def _days_in_month(month, year=1):
        """Number of days in the given month (specified as an int, 1-12).

        The `year` argument gives the year for which to request the number of
        days, in a Gregorian calendar. Defaults to `1` (not a leap year).
        """
        month_start = datetime.date(year, month, 1)
        if month == 12:
            next_year = year + 1
            next_month = 1
        else:
            next_year = year
            next_month = month + 1
        next_month_start = datetime.date(next_year, next_month, 1)
        return (next_month_start - month_start).days

    @classmethod
    def _add_day(cls, year, month, day):
        """Given a year, month and day, add 1 day

        Returns a new tuple, (adjusted_year, adjusted_month, adjusted_day)

        Assumes a no-leap calendar

        >>> StreamCDEPS._add_day(1999, 1, 1)
        (1999, 1, 2)
        >>> StreamCDEPS._add_day(1999, 1, 31)
        (1999, 2, 1)
        >>> StreamCDEPS._add_day(1999, 12, 31)
        (2000, 1, 1)
        """
        adjusted_year = year
        adjusted_month = month
        adjusted_day = day + 1
        if adjusted_day > cls._days_in_month(month):
            adjusted_day = 1
            adjusted_month = adjusted_month + 1
        if adjusted_month > 12:
            adjusted_month = 1
            adjusted_year = adjusted_year + 1
        return (adjusted_year, adjusted_month, adjusted_day)

    def _sub_paths(
        self, stream_name, filenames, year_start, year_end, filename_advance_days
    ):
        """Substitute indicators with given values in a list of filenames.

        Replace any instance of the following substring indicators with the
        appropriate values:
            %y    = year from the range year_start to year_end
            %ym   = year-month from the range year_start to year_end with all 12
                    months
            %ymd  = year-month-day from the range year_start to year_end with
                    all 12 months

        For the date indicators, the year may be prefixed with a number of
        digits to use (the default is 4). E.g. `%2ymd` can be used to change the
        number of year digits from 4 to 2.

        Note that we assume that there is no mixing and matching of date
        indicators, i.e. you cannot use `%4ymd` and `%2y` in the same line. Note
        also that we use a no-leap calendar, i.e. every month has the same
        number of days every year.

        filename_advance_days is an integer specifying the number of days to add to the date
        portion of the file name when using %ymd. Currently only values of 0 or 1 are
        supported. This is typically 0 but can be 1 to indicate that the dates have a
        one-day offset, starting with day 2 in the first year and ending with day 1 in the
        year following the last year. This can be the case, for example, for daily coupler
        history files.

        The difference between this function and `_sub_fields` is that this
        function is intended to be used for file names (especially from the
        `strm_datfil` defaults), whereas `_sub_fields` is intended for use on
        variable names.

        Returns a string (filenames separated by newlines).
        """
        expect(
            filename_advance_days == 0 or filename_advance_days == 1,
            "Bad filename_advance_days attribute ({}) for {}: must be 0 or 1".format(
                filename_advance_days, stream_name
            ),
        )

        lines = [line for line in filenames.split("\n") if line]
        new_lines = []
        for line in lines:
            match = _ymd_re.search(filenames)
            if match is None:
                new_lines.append(line)
                continue
            if match.group("digits"):
                year_format = "{:0" + match.group("digits") + "d}"
            else:
                year_format = "{:04d}"
            for year in range(year_start, year_end + 1):
                if match.group("day"):
                    for month in range(1, 13):
                        days = self._days_in_month(month)
                        for day in range(1, days + 1):
                            if filename_advance_days == 1:
                                (
                                    adjusted_year,
                                    adjusted_month,
                                    adjusted_day,
                                ) = self._add_day(year, month, day)
                            else:
                                (adjusted_year, adjusted_month, adjusted_day) = (
                                    year,
                                    month,
                                    day,
                                )
                            date_string = (year_format + "-{:02d}-{:02d}").format(
                                adjusted_year, adjusted_month, adjusted_day
                            )
                            new_line = line.replace(match.group(0), date_string)
                            new_lines.append(new_line)
                elif match.group("month"):
                    for month in range(1, 13):
                        date_string = (year_format + "-{:02d}").format(year, month)
                        new_line = line.replace(match.group(0), date_string)
                        new_lines.append(new_line)
                else:
                    date_string = year_format.format(year)
                    new_line = line.replace(match.group(0), date_string)
                    new_lines.append(new_line)
        return "\n".join(new_lines)

    @staticmethod
    def _add_xml_delimiter(list_to_deliminate, delimiter):
        expect(delimiter and not " " in delimiter, "Missing or badly formed delimiter")
        pred = "<{}>".format(delimiter)
        postd = "</{}>".format(delimiter)
        for n, item in enumerate(list_to_deliminate):
            if item.strip():
                list_to_deliminate[n] = pred + item.strip() + postd
            # endif
        # endfor
        return "\n      ".join(list_to_deliminate)

    def update_input_data_list(self, data_list_file):
        """From the stream object parse out and list required input files"""
        sinodes = self.scan_children("stream_info")
        for node in sinodes:
            meshnode = self.scan_child("stream_mesh_file", root=node)
            stream_meshfile = self.text(meshnode)
            data_file_node = self.scan_child("stream_data_files", root=node)
            filenodes = self.scan_children("file", root=data_file_node)
            stream_datafiles = []
            for fnode in filenodes:
                stream_datafiles.append(self.text(fnode))
            self._add_entries_to_inputdata_list(
                stream_meshfile, stream_datafiles, data_list_file
            )
