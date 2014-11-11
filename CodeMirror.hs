{-# LANGUAGE CPP #-}
module CodeMirror where

import Import

codeMirror :: Widget
codeMirror = do
    addStylesheet $ StaticR css_codemirror_css
    addStylesheet $ StaticR css_codemirror_custom_css
    addStylesheet $ StaticR js_addon_dialog_dialog_css
    -- addScript $ StaticR js_codemirror_js
    -- addScript $ StaticR js_mode_xml_js
    -- addScript $ StaticR js_mode_css_js
    -- addScript $ StaticR js_mode_javascript_js
    -- addScript $ StaticR js_mode_htmlmixed_js
    -- addScript $ StaticR js_addon_dialog_dialog_js
    -- addScript $ StaticR js_addon_edit_trailingspace_js
    -- addScript $ StaticR js_addon_search_search_js
    -- addScript $ StaticR js_addon_search_match_highlighter_js
    -- addScript $ StaticR js_addon_search_searchcursor_js
    addScript $ StaticR js_codemirror_min_js
    toWidget [julius|
      function cm_areas() {
          CodeMirror.fromTextArea(
              $('textarea').prop('required', false).get(0),
              {
                  lineNumbers: true,
                  mode: "text/html",
                  showTrailingSpace: true,
                  highlightSelectionMatches: true,
                  lineWrapping: true
              }
          );
      }
      $( document ).ready(function() {
          if ($('textarea').length) {
              cm_areas();
          }
      });
    |]
