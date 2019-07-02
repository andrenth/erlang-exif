-module(exif).
-export([read/1]).

-ifdef(debug).
-define(DEBUG(Fmt, Args), io:format(Fmt, Args)).
-else.
-define(DEBUG(_Fmt, _Args), ok).
-endif.

-define(MAX_EXIF_LEN,        65536).
-define(JPEG_MARKER,       16#ffd8).
-define(EXIF_MARKER,       16#ffe1).
-define(FIRST_EXIF_OFFSET,       8).
-define(FIELD_LEN,              12).
-define(START_POSITION,         22).
-define(BYTE_SIZE,               1).
-define(SHORT_SIZE,              2).
-define(LONG_SIZE,               4).
-define(RATIO_SIZE,              8).
-define(MAX_INT32,      2147483647).
-define(MAX_UINT32,     4294967295).

-define(TYPE_BYTE,          1).
-define(TYPE_ASCII,         2).
-define(TYPE_SHORT,         3).
-define(TYPE_LONG,          4).
-define(TYPE_RATIO,         5).
-define(TYPE_SIGNED_BYTE,   6).
-define(TYPE_UNDEFINED,     7).
-define(TYPE_SIGNED_SHORT,  8).
-define(TYPE_SIGNED_LONG,   9).
-define(TYPE_SIGNED_RATIO, 10).


-define(EXIF_PATTERN,
  << "Exif",
     0         : 16,
     ByteOrder : 16,
     FortyTwo  : 2/binary,
     Offset    : 4/binary,
     Rest/binary
  >>).

-define(TAG_VALUE_PATTERN,
  << ValueType   : 2/binary,
     ValueNum    : 4/binary,
     Rest/binary
  >>).

read(File) when is_list(File) ->
  {ok, Fd} = file:open(File, [read, binary, raw]),
  {ok, Img} = file:read(Fd, ?MAX_EXIF_LEN),
  ok = file:close(Fd),
  read(Img);

read(<< ?JPEG_MARKER:16, ?EXIF_MARKER:16, _Len:16, Rest/binary >>) ->
  read_exif(Rest);
read(<< ?JPEG_MARKER:16, _Rest/binary >>) ->
  {error, invalid_jpeg};
read(_) ->
  {error, unsupported_image}.

read_exif(?EXIF_PATTERN) ->
  End = case ByteOrder of
    16#4949 -> little;
    16#4d4d -> big
  end,
  42 = uread(FortyTwo, End),
  ?FIRST_EXIF_OFFSET = uread(Offset, End),
  read_tags(Rest, ?START_POSITION, End, fun image_tag/1);

read_exif(_) ->
  {error, invalid_exif}.

read_tags(<< NumEntries:2/binary, Rest/binary >>, StartPos, End, TagFun) ->
  N = uread(NumEntries, End),
  read_tags(Rest, N, StartPos, End, TagFun).

read_tags(Bin, NumEntries, StartPos, End, TagFun) ->
  read_tags(Bin, NumEntries, StartPos, End, TagFun, #{}).

read_tags(_Bin, 0, _StartPos, _End, _TagFun, Tags) ->
  Tags;
read_tags(Bin, NumEntries, StartPos, End, TagFun, Tags) ->
  {NewTags, Rest} = add_tag(Bin, StartPos, End, Tags, TagFun),
  read_tags(Rest, NumEntries - 1, StartPos + ?FIELD_LEN, End, TagFun, NewTags).

add_tag(<< Tag:2/binary, Rest/binary >>, StartPos, End, Tags, TagFun) ->
  Name = TagFun(uread(Tag, End)),
  Value = tag_value(Name, read_tag_value(Rest, StartPos, End)),
  ?DEBUG("Tag: ~p: ~p~n", [Name, Value]),
  NewTags = case Name of
    unknown ->
      Tags;
    exif ->
      ExifTags = read_subtags(Rest, Value, StartPos, End, fun exif_tag/1),
      maps:merge(Tags, ExifTags);
    gps ->
      GpsTags = read_subtags(Rest, Value, StartPos, End, fun gps_tag/1),
      maps:merge(Tags, GpsTags);
    _ ->
      Tags#{Name => Value}
  end,
  Len = ?FIELD_LEN - 2, % 2 bytes for the tag above.
  << _:Len/binary, NewRest/binary >> = Rest,
  {NewTags, NewRest}.

read_subtags(Bin, Offset, StartPos, End, TagFun) ->
  RelOffset = ?FIELD_LEN + Offset - StartPos - 2,
  RelStartPos = ?FIELD_LEN + Offset + 2,
  << _:RelOffset/binary, Rest/binary >> = Bin,
  read_tags(Rest, RelStartPos, End, TagFun).

read_tag_value(?TAG_VALUE_PATTERN, StartPos, End) ->
  Type = uread(ValueType, End),
  NumValues = uread(ValueNum, End),
  case decode_tag(Type, Rest, NumValues, StartPos, End) of
    [Value] -> Value;
    Values  -> Values
  end.

%% Tag decoding by type.

% Byte
decode_tag(?TYPE_BYTE, Bin, NumValues, StartPos, End) ->
  ?DEBUG("> Byte(~p)~n", [NumValues]),
  decode_numeric(Bin, NumValues, StartPos, ?BYTE_SIZE, End);

% ASCII
decode_tag(?TYPE_ASCII, Bin, NumBytes, StartPos, End) ->
  ?DEBUG("> ASCII(~p)~n", [NumBytes]),
  << ValueOffset:4/binary, Rest/binary >> = Bin,
  case NumBytes > 4 of
    true ->
      Offset = uread(ValueOffset, End) - StartPos,
      Len = NumBytes - 1,  % ignore null-byte termination
      << _:Offset/binary, Value:Len/binary, _/binary >> = Rest,
      Value;
    false ->
      ValueOffset
  end;

% Short
decode_tag(?TYPE_SHORT, Bin, NumValues, StartPos, End) ->
  ?DEBUG("> Short(~p)~n", [NumValues]),
  decode_numeric(Bin, NumValues, StartPos, ?SHORT_SIZE, End);

% Signed short
decode_tag(?TYPE_SIGNED_SHORT, Bin, NumValues, StartPos, End) ->
  ?DEBUG("> Signed Short(~p)~n", [NumValues]),
  Ushorts = decode_numeric(Bin, NumValues, StartPos, ?SHORT_SIZE, End),
  lists:map(fun as_signed/1, Ushorts);

% Long
decode_tag(?TYPE_LONG, Bin, NumValues, StartPos, End) ->
  ?DEBUG("> Long(~p)~n", [NumValues]),
  decode_numeric(Bin, NumValues, StartPos, ?LONG_SIZE, End);

% Signed long
decode_tag(?TYPE_SIGNED_LONG, Bin, NumValues, StartPos, End) ->
  ?DEBUG("> Signed Long(~p)~n", [NumValues]),
  Ulongs = decode_numeric(Bin, NumValues, StartPos, ?LONG_SIZE, End),
  lists:map(fun as_signed/1, Ulongs);

% Rational
decode_tag(?TYPE_RATIO, Bin, NumValues, StartPos, End) ->
  ?DEBUG("> Rational(~p)~n", [NumValues]),
  decode_ratio(Bin, NumValues, StartPos, End);

% Signed rational
decode_tag(?TYPE_SIGNED_RATIO, Bin, NumValues, StartPos, End) ->
  ?DEBUG("> Signed Rational(~p)~n", [NumValues]),
  Uratios = decode_ratio(Bin, NumValues, StartPos, End),
  lists:map(fun({ratio, Num, Den}) ->
    {ratio, as_signed(Num), as_signed(Den)}
  end, Uratios);

% Undefined
decode_tag(?TYPE_UNDEFINED, Bin, NumValues, StartPos, End) ->
  ?DEBUG("> Undefined(~p)~n", [NumValues]),
  decode_numeric(Bin, NumValues, StartPos, ?BYTE_SIZE, End).

decode_numeric(Bin, NumValues, StartPos, Size, End) ->
  Len = NumValues * Size,
  Values = case Len > 4 of
    true ->
      << Offset:4/binary, Rest/binary >> = Bin,
      RelOffset = uread(Offset, End) - StartPos,
      << _:RelOffset/binary, Data:Len/binary, _/binary >> = Rest,
      Data;
    false ->
      << Value:Len/binary, _/binary >> = Bin,
      Value
  end,
  uread_many(Values, Size, End).

decode_ratio(Bin, NumValues, StartPos, End) ->
  << ValueOffset:4/binary, Rest/binary >> = Bin,
  Offset = uread(ValueOffset, End) - StartPos,
  decode_ratios(Rest, NumValues, Offset, End).

decode_ratios(_Bin, 0, _Offset, _End) ->
  [];
decode_ratios(Bin, NumValues, Offset, End) ->
  << _ : Offset/binary,
     N : ?LONG_SIZE/binary,
     D : ?LONG_SIZE/binary,
     Rest/binary >> = Bin,
  NewOffset = Offset + ?RATIO_SIZE,
  Num = uread(N, End),
  Den = uread(D, End),
  [{ratio, Num, Den} | decode_ratios(Rest, NumValues - 1, NewOffset, End)].

as_signed(X) when X > ?MAX_INT32 -> X - ?MAX_UINT32 - 1;
as_signed(X)                     -> X.

image_tag(16#00fe) -> new_subfile_type;
image_tag(16#00ff) -> subfile_type;
image_tag(16#0100) -> image_width;
image_tag(16#0101) -> image_length;
image_tag(16#0102) -> bits_per_sample;
image_tag(16#0103) -> compression;
image_tag(16#0106) -> photometric_interpretation;
image_tag(16#0107) -> threshholding;
image_tag(16#0108) -> cell_width;
image_tag(16#0109) -> cell_length;
image_tag(16#010a) -> fill_order;
image_tag(16#010d) -> document_name;
image_tag(16#010e) -> image_description;
image_tag(16#010f) -> make;
image_tag(16#0110) -> model;
image_tag(16#0111) -> strip_offsets;
image_tag(16#0112) -> orientation;
image_tag(16#0115) -> samples_per_pixel;
image_tag(16#0116) -> rows_per_strip;
image_tag(16#0117) -> strip_byte_counts;
image_tag(16#0118) -> min_sample_value;
image_tag(16#0119) -> max_sample_value;
image_tag(16#011a) -> x_resolution;
image_tag(16#011b) -> y_resolution;
image_tag(16#011c) -> planar_configuration;
image_tag(16#011d) -> page_name;
image_tag(16#011e) -> x_position;
image_tag(16#011f) -> y_position;
image_tag(16#0120) -> free_offsets;
image_tag(16#0121) -> free_byte_counts;
image_tag(16#0122) -> gray_response_unit;
image_tag(16#0123) -> gray_response_curve;
image_tag(16#0124) -> t4_options;
image_tag(16#0125) -> t6_options;
image_tag(16#0128) -> resolution_unit;
image_tag(16#012d) -> transfer_function;
image_tag(16#0131) -> software;
image_tag(16#0132) -> date_time;
image_tag(16#013b) -> artist;
image_tag(16#013c) -> host_computer;
image_tag(16#013a) -> predictor;
image_tag(16#013e) -> white_point;
image_tag(16#013f) -> primary_chromaticities;
image_tag(16#0140) -> color_map;
image_tag(16#0141) -> halftone_hints;
image_tag(16#0142) -> tile_width;
image_tag(16#0143) -> tile_length;
image_tag(16#0144) -> tile_offsets;
image_tag(16#0145) -> tile_byte_counts;
image_tag(16#0146) -> bad_fax_lines;
image_tag(16#0147) -> clean_fax_data;
image_tag(16#0148) -> consecutive_bad_fax_lines;
image_tag(16#014a) -> sub_ifds;
image_tag(16#014c) -> ink_set;
image_tag(16#014d) -> ink_names;
image_tag(16#014e) -> number_of_inks;
image_tag(16#0150) -> dot_range;
image_tag(16#0151) -> target_printer;
image_tag(16#0152) -> extra_samples;
image_tag(16#0156) -> transfer_range;
image_tag(16#0157) -> clip_path;
image_tag(16#0158) -> x_clip_path_units;
image_tag(16#0159) -> y_clip_path_units;
image_tag(16#015a) -> indexed;
image_tag(16#015b) -> jpeg_tables;
image_tag(16#015f) -> opi_proxy;
image_tag(16#0190) -> global_parameters_ifd;
image_tag(16#0191) -> profile_type;
image_tag(16#0192) -> fax_profile;
image_tag(16#0193) -> coding_methods;
image_tag(16#0194) -> version_year;
image_tag(16#0195) -> mode_number;
image_tag(16#01B1) -> decode;
image_tag(16#01B2) -> default_image_color;
image_tag(16#0200) -> jpegproc;
image_tag(16#0201) -> jpeg_interchange_format;
image_tag(16#0202) -> jpeg_interchange_format_length;
image_tag(16#0203) -> jpeg_restart_interval;
image_tag(16#0205) -> jpeg_lossless_predictors;
image_tag(16#0206) -> jpeg_point_transforms;
image_tag(16#0207) -> jpeg_q_tables;
image_tag(16#0208) -> jpeg_dc_tables;
image_tag(16#0209) -> jpeg_ac_tables;
image_tag(16#0211) -> ycb_cr_coefficients;
image_tag(16#0212) -> ycb_cr_sub_sampling;
image_tag(16#0213) -> ycb_cr_positioning;
image_tag(16#0214) -> reference_black_white;
image_tag(16#022f) -> strip_row_counts;
image_tag(16#02bc) -> xmp;
image_tag(16#800d) -> image_id;
image_tag(16#87ac) -> image_layer;
image_tag(16#8298) -> copyright;
image_tag(16#83bb) -> iptc;
image_tag(16#8769) -> exif;
image_tag(16#8825) -> gps;
image_tag(_)       -> unknown.

exif_tag(16#829a) -> exposure_time;
exif_tag(16#829d) -> f_number;
exif_tag(16#8822) -> exposure_program;
exif_tag(16#8824) -> spectral_sensitivity;
exif_tag(16#8827) -> iso_speed_ratings;
exif_tag(16#8828) -> oecf;
exif_tag(16#9000) -> exif_version;
exif_tag(16#9003) -> date_time_original;
exif_tag(16#9004) -> date_time_digitized;
exif_tag(16#9101) -> components_configuration;
exif_tag(16#9102) -> compressed_bits_per_pixel;
exif_tag(16#9201) -> shutter_speed_value;
exif_tag(16#9202) -> aperture_value;
exif_tag(16#9203) -> brightness_value;
exif_tag(16#9204) -> exposure_bias_value;
exif_tag(16#9205) -> max_aperture_value;
exif_tag(16#9206) -> subject_distance;
exif_tag(16#9207) -> metering_mode;
exif_tag(16#9208) -> light_source;
exif_tag(16#9209) -> flash;
exif_tag(16#920a) -> focal_length;
exif_tag(16#9214) -> subject_area;
exif_tag(16#927c) -> maker_note;
exif_tag(16#9286) -> user_comment;
exif_tag(16#9290) -> subsec_time;
exif_tag(16#9291) -> subsec_time_orginal;
exif_tag(16#9292) -> subsec_time_digitized;
exif_tag(16#a000) -> flashpix_version;
exif_tag(16#a001) -> color_space;
exif_tag(16#a002) -> pixel_x_dimension;
exif_tag(16#a003) -> pixel_y_dimension;
exif_tag(16#a004) -> related_sound_file;
exif_tag(16#a20b) -> flash_energy;
exif_tag(16#a20c) -> spatial_frequency_response;
exif_tag(16#a20e) -> focal_plane_x_resolution;
exif_tag(16#a20f) -> focal_plane_y_resolution;
exif_tag(16#a210) -> focal_plane_resolution_unit;
exif_tag(16#a214) -> subject_location;
exif_tag(16#a215) -> exposure_index;
exif_tag(16#a217) -> sensing_method;
exif_tag(16#a300) -> file_source;
exif_tag(16#a301) -> scene_type;
exif_tag(16#a302) -> cfa_pattern;
exif_tag(16#a401) -> custom_rendered;
exif_tag(16#a402) -> exposure_mode;
exif_tag(16#a403) -> white_balance;
exif_tag(16#a404) -> digital_zoom_ratio;
exif_tag(16#a405) -> focal_length_in_35mm_film;
exif_tag(16#a406) -> scene_capture_type;
exif_tag(16#a407) -> gain_control;
exif_tag(16#a408) -> contrast;
exif_tag(16#a409) -> saturation;
exif_tag(16#a40a) -> sharpness;
exif_tag(16#a40b) -> device_setting_description;
exif_tag(16#a40c) -> subject_distance_range;
exif_tag(16#a420) -> image_unique_id;
exif_tag(_)       -> unknown.

gps_tag(16#0000) -> gps_version_id;
gps_tag(16#0001) -> gps_latitude_ref;
gps_tag(16#0002) -> gps_latitude;
gps_tag(16#0003) -> gps_longitude_ref;
gps_tag(16#0004) -> gps_longitude;
gps_tag(16#0005) -> gps_altitude_ref;
gps_tag(16#0006) -> gps_altitude  ;
gps_tag(16#0007) -> gps_time_stamp;
gps_tag(16#0008) -> gps_satellites;
gps_tag(16#0009) -> gps_status;
gps_tag(16#000a) -> gps_measure_mode;
gps_tag(16#000b) -> gps_dop;
gps_tag(16#000c) -> gps_speed_ref;
gps_tag(16#000d) -> gps_speed;
gps_tag(16#000e) -> gps_track_ref;
gps_tag(16#000f) -> gps_track;
gps_tag(16#0010) -> gps_img_direction_ref;
gps_tag(16#0011) -> gps_img_direction;
gps_tag(16#0012) -> gps_map_datum;
gps_tag(16#0013) -> gps_dest_latitude_ref;
gps_tag(16#0014) -> gps_dest_latitude;
gps_tag(16#0015) -> gps_dest_longitude_ref;
gps_tag(16#0016) -> gps_dest_longitude;
gps_tag(16#0017) -> gps_dest_bearing_ref;
gps_tag(16#0018) -> gps_dest_bearing;
gps_tag(16#0019) -> gps_dest_distance_ref;
gps_tag(16#001a) -> gps_dest_distance;
gps_tag(16#001b) -> gps_processing_method;
gps_tag(16#001c) -> gps_area_information;
gps_tag(16#001d) -> gps_date_stamp;
gps_tag(16#001e) -> gps_differential;
gps_tag(_)       -> unknown.

tag_value(planar_configuration, 0) -> <<"Chunky format">>;
tag_value(planar_configuration, 1) -> <<"Planar format">>;

tag_value(sensing_method, 1) -> <<"Not defined">>;
tag_value(sensing_method, 2) -> <<"One-chip color area sensor">>;
tag_value(sensing_method, 3) -> <<"Two-chip color area sensor">>;
tag_value(sensing_method, 4) -> <<"Three-chip color area sensor">>;
tag_value(sensing_method, 5) -> <<"Color sequential area sensor">>;
tag_value(sensing_method, 6) -> <<"Trilinear sensor">>;
tag_value(sensing_method, 7) -> <<"Color sequential linear sensor">>;

tag_value(orientation, 1) -> <<"Top-left">>;
tag_value(orientation, 2) -> <<"Top-right">>;
tag_value(orientation, 3) -> <<"Bottom-right">>;
tag_value(orientation, 4) -> <<"Bottom-left">>;
tag_value(orientation, 5) -> <<"Left-top">>;
tag_value(orientation, 6) -> <<"Right-top">>;
tag_value(orientation, 7) -> <<"Top-left">>;
tag_value(orientation, 8) -> <<"Top-left">>;

tag_value(ycb_cr_positioning, 1) -> <<"Centered">>;
tag_value(ycb_cr_positioning, 2) -> <<"Co-sited">>;

tag_value(photometric_interpretation, 0) -> <<"Reversed mono">>;
tag_value(photometric_interpretation, 1) -> <<"Normal mono">>;
tag_value(photometric_interpretation, 2) -> <<"RGB">>;
tag_value(photometric_interpretation, 3) -> <<"Palette">>;
tag_value(photometric_interpretation, 5) -> <<"CMYK">>;
tag_value(photometric_interpretation, 6) -> <<"YCbCr">>;
tag_value(photometric_interpretation, 8) -> <<"CieLAB">>;

tag_value(custom_rendered, 0) -> <<"Normal process">>;
tag_value(custom_rendered, 1) -> <<"Custom process">>;

tag_value(exposure_mode, 0) -> <<"Auto exposure">>;
tag_value(exposure_mode, 1) -> <<"Manual exposure">>;
tag_value(exposure_mode, 2) -> <<"Auto bracket">>;

tag_value(white_balance, 0) -> <<"Auto white balance">>;
tag_value(white_balance, 1) -> <<"Manual white balance">>;

tag_value(scene_capture_type, 0) -> <<"Standard">>;
tag_value(scene_capture_type, 1) -> <<"Landscape">>;
tag_value(scene_capture_type, 2) -> <<"Portrait">>;
tag_value(scene_capture_type, 3) -> <<"Night scene">>;

tag_value(gain_control, 0) -> <<"Normal">>;
tag_value(gain_control, 1) -> <<"Low gain up">>;
tag_value(gain_control, 2) -> <<"High gain up">>;
tag_value(gain_control, 3) -> <<"Low gain down">>;
tag_value(gain_control, 4) -> <<"High gain down">>;

tag_value(saturation, 0) -> <<"Normal">>;
tag_value(saturation, 1) -> <<"Low saturation">>;
tag_value(saturation, 2) -> <<"High saturation">>;

tag_value(contrast, 0) -> <<"Normal">>;
tag_value(contrast, 1) -> <<"Soft">>;
tag_value(contrast, 2) -> <<"Hard">>;

tag_value(sharpness, 0) -> <<"Normal">>;
tag_value(sharpness, 1) -> <<"Soft">>;
tag_value(sharpness, 2) -> <<"Hard">>;

tag_value(exif_version, "0110") -> <<"Exif Version 1.1">>;
tag_value(exif_version, "0120") -> <<"Exif Version 1.2">>;
tag_value(exif_version, "0200") -> <<"Exif Version 2.0">>;
tag_value(exif_version, "0210") -> <<"Exif Version 2.1">>;
tag_value(exif_version, "0220") -> <<"Exif Version 2.1">>;
tag_value(exif_version, "0221") -> <<"Exif Version 2.21">>;
tag_value(exif_version, _)      -> <<"Unknown Exif Version">>;

tag_value(flashpix_version, "0100") -> <<"FlashPix Version 1.0">>;
tag_value(flashpix_version, "0101") -> <<"FlashPix Version 1.01">>;
tag_value(flashpix_version, _)      -> <<"Unknown FlashPix version">>;

tag_value(file_source, 3) -> <<"DSC">>;

% TODO: copyright, components_configuration, scene_type, ycb_cr_sub_sampling,
%       subject_area, gps_altitude_ref

tag_value(_Name, Value) -> Value.

uread(Bin, End) ->
  binary:decode_unsigned(Bin, End).

uread_many(<<>>, _Size, _End) ->
  [];
uread_many(Bin, Size, End) ->
  << Num:Size/binary, Rest/binary >> = Bin,
  [uread(Num, End) | uread_many(Rest, Size, End)].
