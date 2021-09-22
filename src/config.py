class CONFIG:
    # Only try with zlib settings that so far produced best results. In
    # particular don't try compressing with uncompressed level. Setting
    # this to False will try all settings and assert if there's one
    # producting better results than the optimal settings.
    zlib_try_optimal_settings_only = True

    # Overridable by --depth
    max_id_depth = 3

    # Default max chars (can be dynamic)
    max_char_depth = 10

    # Default is True
    # use_threads = False

    # Bah (used for single pass unconditionally)
    # use_match_probe = True
    # use_zlib_probe = True
    # use_zopfli_probe = True

    start_slot = 0

    # [TODO] Usually produces better results. Should be dynamic.
    replace_ids_outside_of_search_range_with_match_hints = True
