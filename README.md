<a name='x-28MGL-PAX-WORLD-3A-40MGL-PAX-WORLD-MANUAL-20MGL-PAX-3ASECTION-29'></a>

# PAX World Manual

## Table of Contents

- [1 mgl-pax-world ASDF System Details][2272]
- [2 Basics][861c]

###### \[in package MGL-PAX-WORLD\]
<a name='x-28-22mgl-pax-world-22-20ASDF-2FSYSTEM-3ASYSTEM-29'></a>

## 1 mgl-pax-world ASDF System Details

- Version: 0.0.1
- Description: Cross-linked HTML documentation for systems known to
  use MGL-PAX.
- Licence: MIT, see COPYING.
- Author: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)
- Homepage: [http://quotenil.com](http://quotenil.com)

<a name='x-28MGL-PAX-WORLD-3A-40MGL-PAX-WORLD-BASICS-20MGL-PAX-3ASECTION-29'></a>

## 2 Basics

Not that there is room here to be non-basic in any way.

<a name='x-28MGL-PAX-WORLD-3AUPDATE-PAX-WORLD-20FUNCTION-29'></a>

- [function] **UPDATE-PAX-WORLD** *&KEY (SECTIONS (DEFAULT-SECTIONS)) (PAGE-SPECS (DEFAULT-PAGE-SPECS))*

    Delete all HTML files from the root of the [`MGL-PAX-WORLD`][2272] asdf
    system and generate documentation for all `SECTIONS`.

<a name='x-28MGL-PAX-WORLD-3ADEFAULT-SECTIONS-20FUNCTION-29'></a>

- [function] **DEFAULT-SECTIONS** 

    Returns the default list of sections for which [`UPDATE-PAX-WORLD`][9e20]
    generates documentation.

<a name='x-28MGL-PAX-WORLD-3ADEFAULT-PAGE-SPECS-20FUNCTION-29'></a>

- [function] **DEFAULT-PAGE-SPECS** 

    Returns the default list of page specification for
    [`UPDATE-PAX-WORLD`][9e20]. This must be suitable as the PAGES argument for
    `DOCUMENT`.

  [2272]: #x-28-22mgl-pax-world-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "(\"mgl-pax-world\" ASDF/SYSTEM:SYSTEM)"
  [861c]: #x-28MGL-PAX-WORLD-3A-40MGL-PAX-WORLD-BASICS-20MGL-PAX-3ASECTION-29 "(MGL-PAX-WORLD:@MGL-PAX-WORLD-BASICS MGL-PAX:SECTION)"
  [9e20]: #x-28MGL-PAX-WORLD-3AUPDATE-PAX-WORLD-20FUNCTION-29 "(MGL-PAX-WORLD:UPDATE-PAX-WORLD FUNCTION)"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
