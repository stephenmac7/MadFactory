{-# LANGUAGE CPP #-}
module CodeMirror where

import Import

codeMirror :: Widget
codeMirror = do
    addStylesheet $ StaticR css_codemirror_css
    addStylesheet $ StaticR css_codemirror_custom_css
    addStylesheet $ StaticR js_addon_dialog_dialog_css
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
