\version "2.20.0"

% Use oll-core from the 'properties' branch
\include "oll-core/package.ily"

% Define a property set
% Properties hold a name, a type predicate and a default value.
% The default value (as well as later assignments) are type-checked
% against the predicate
\definePropertySet demo.props
#`((text ,string? "bar")
   (color ,color? ,red)
   (index ,integer? 4)
   (label ,symbol? foo-symbol)
   ;(use-case ,symbol? "fail") ; fails on typecheck
   )

%\setProperty OLL.presets use-presets ##t

% Retrieve the property
\markup \getProperty demo.props text

% Set a property, this will be the new "current" value of the property
\setProperty demo.props text "baz"
\markup \getProperty demo.props text

% Set a property with wrong type -> no change, will be skipped
%\setProperty demo.props text #green
\markup \getProperty demo.props text

% Define a named preset (for a specific propset).
% When used the included overrides will take precedence
% over the current property values.
% (type checking is active too
\definePropertyConfiguration \with {
  text = boo
  color = #blue
%  index = invalid % fails type-check
} demo.props.my-preset

% Define a function with the propset
% - Due to the optional \with block at least one mandatory
%   argument is required.
% - Within the function all properties are accessible
%   through the local (property '<prop-name>) function
% - If validation is necessary the effective properties
%   (after merging) can be accessed through the
%   props variable
testFunc =
#(with-property-set define-scheme-function (dummy)(boolean?)
   `(demo props)
   (let*
    ((do-use (use-by-configuration?))
     (text (property 'text))
     (content
          (if do-use
              (format #f "~a. ~a" (property 'index) text)
              (format #f "~a" text))))
    (if do-use
        (markup #:with-color (property 'color) content)
        (markup content)
      )))

% Invoke function with currently active properties
%\testFunc ##t

% Invoke function with a preset
\testFunc \with {
  configuration = #'my-preset
} ##t

% Invoke function with a preset plus individual override
\testFunc \with {
  configuration = #'my-preset
  %index = ##t %fails due to type check
  color = #magenta
  label = "heyho-symbol" % (implicitly converted to symbol)
} ##t



\testFunc ##t
