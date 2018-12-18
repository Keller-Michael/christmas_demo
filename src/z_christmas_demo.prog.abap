*&---------------------------------------------------------------------*
*& Report Z_CHRISTMAS_DEMO
*&---------------------------------------------------------------------*
*& Christmas greetings to anyone who likes to work with ABAP. See
*& report "SHOWCOLO" for colors.
*&---------------------------------------------------------------------*
REPORT z_christmas_demo NO STANDARD PAGE HEADING.

CLASS lcl_christmas_demo DEFINITION.

  PUBLIC SECTION.
    METHODS hourglass_expired FOR EVENT finished OF cl_gui_timer.

    METHODS let_the_elves_do_their_work.

    METHODS send_elves_back_to_work.

  PRIVATE SECTION.
    CONSTANTS: mc_max_y           TYPE i VALUE 30,
               mc_fir_needle_char TYPE char1 VALUE '^',
               mc_tree_trunk_char TYPE char1 VALUE '"',
               mc_bauble          TYPE char1 VALUE 'o'.

    TYPES: BEGIN OF baubles_memory,
             position_y TYPE i,
             position_x TYPE i,
           END OF baubles_memory.

    DATA: mv_current_y      TYPE i,
          mt_baubles_memory TYPE TABLE OF baubles_memory,
          mr_hourglass      TYPE REF TO cl_gui_timer.

    METHODS elves_at_work.

    METHODS output_fir_needles
      IMPORTING
        iv_start_x TYPE i
        iv_width   TYPE i.

    METHODS output_tree_trunk
      IMPORTING
        iv_start_x TYPE i
        iv_width   TYPE i.

    METHODS output_snowflakes.

    METHODS output_christmas_baubles
      IMPORTING
        iv_start_x TYPE i
        iv_width   TYPE i.

    METHODS output_star.

    METHODS output_frames.

    METHODS output_ground.

    METHODS output_banner.

    METHODS output_snowman
      IMPORTING
        iv_start_x TYPE i
        iv_width   TYPE i.

    METHODS output_present
      IMPORTING
        iv_start_x TYPE i.
ENDCLASS.


CLASS lcl_christmas_demo IMPLEMENTATION.
  METHOD let_the_elves_do_their_work.
    CREATE OBJECT mr_hourglass.
    SET HANDLER me->hourglass_expired FOR mr_hourglass.
    mr_hourglass->interval = 1.
    mr_hourglass->run( ).
    elves_at_work( ).
  ENDMETHOD.


  METHOD send_elves_back_to_work.
    elves_at_work( ).
    mr_hourglass->run( ).
  ENDMETHOD.


  METHOD elves_at_work.
    DATA(lv_fir_needle_width) = 1 .
    DATA(lv_fir_needle_start_pos) = 40.
    DATA(lv_tree_trunk_start_pos) = 38.
    DATA(lv_tree_trunk_width) = 5.

    " main loop to build graphics by christmas elves
    DO mc_max_y TIMES.
      mv_current_y = sy-index.

      output_frames( ).

      IF mv_current_y >= 2 AND mv_current_y <= 27.
        output_snowflakes( ).
      ENDIF.

*      IF mv_current_y >= 18 AND mv_current_y <= 26.
*        output_snowman(
*          EXPORTING
*            iv_start_x = 65
*            iv_width   = 5
*        ).
*      ENDIF.

      IF mv_current_y >= 3 AND mv_current_y <= 6.
        output_star( ).
      ENDIF.

      IF mv_current_y >= 7 AND mv_current_y <= 21.
        " output fir needles
        output_fir_needles(
          EXPORTING
            iv_start_x = lv_fir_needle_start_pos
            iv_width   = lv_fir_needle_width
        ).

        " christmas baubles to decorate
        me->output_christmas_baubles(
          EXPORTING
            iv_start_x = lv_fir_needle_start_pos
            iv_width   = lv_fir_needle_width
        ).

        lv_fir_needle_start_pos = lv_fir_needle_start_pos - 1.
        lv_fir_needle_width = lv_fir_needle_width + 2.
      ENDIF.

      IF mv_current_y >= 22 AND mv_current_y <= 26.
        " output tree trunk
        output_tree_trunk(
          EXPORTING
            iv_start_x = lv_tree_trunk_start_pos
            iv_width   = lv_tree_trunk_width
        ).
      ENDIF.

*      IF mv_current_y >= 24 AND mv_current_y <= 26.
*        output_present( iv_start_x = 17 ).
*      ENDIF.

      IF mv_current_y = 27.
        output_ground( ).
      ENDIF.

      IF mv_current_y = 29.
        output_banner( ).
      ENDIF.

      NEW-LINE.
    ENDDO.
  ENDMETHOD.


  METHOD output_present.
    IF mv_current_y = 24.
      DATA(lv_position_x) = iv_start_x.
      WRITE AT lv_position_x(2) ' \' COLOR COL_NEGATIVE INTENSIFIED ON INVERSE OFF.
      lv_position_x = iv_start_x + 2.
      WRITE AT lv_position_x(1) 'o' COLOR COL_TOTAL INTENSIFIED ON INVERSE OFF FRAMES OFF.
      lv_position_x = iv_start_x + 3.
      WRITE AT lv_position_x(2) '/ ' COLOR COL_NEGATIVE INTENSIFIED ON INVERSE OFF.
      lv_position_x = iv_start_x + 2.
      WRITE AT lv_position_x(1) 'o' COLOR COL_TOTAL INTENSIFIED ON INVERSE OFF FRAMES OFF.
    ELSEIF mv_current_y = 26.
      lv_position_x = iv_start_x.
      WRITE AT lv_position_x(2) ' /' COLOR COL_NEGATIVE INTENSIFIED ON INVERSE OFF FRAMES OFF.
      lv_position_x = iv_start_x + 2.
      WRITE AT lv_position_x(1) 'o' COLOR COL_TOTAL INTENSIFIED ON INVERSE OFF FRAMES OFF.
      lv_position_x = iv_start_x + 3.
      WRITE AT lv_position_x(2) '\ ' COLOR COL_NEGATIVE INTENSIFIED ON INVERSE OFF.
      lv_position_x = iv_start_x + 2.
      WRITE AT lv_position_x(1) 'o' COLOR COL_TOTAL INTENSIFIED ON INVERSE OFF FRAMES OFF.
    ELSEIF mv_current_y = 25.
      lv_position_x = iv_start_x.
      WRITE AT lv_position_x(5) 'ooxoo' COLOR COL_TOTAL INTENSIFIED ON INVERSE OFF FRAMES OFF.
    ENDIF.
  ENDMETHOD.


  METHOD output_snowman.
    IF mv_current_y = 18 OR mv_current_y = 21.
      DATA(lv_count) = iv_width.
      DATA(lv_position_x) = iv_start_x.
    ELSEIF mv_current_y = 19 OR mv_current_y = 20.
      lv_count = iv_width + 2.
      lv_position_x = iv_start_x - 1.
    ELSEIF mv_current_y = 22 OR mv_current_y = 25.
      lv_count = iv_width + 4.
      lv_position_x = iv_start_x - 2.
    ELSEIF mv_current_y = 23 OR mv_current_y = 24.
      lv_count = iv_width + 6.
      lv_position_x = iv_start_x - 3.
    ELSEIF mv_current_y = 26.
      lv_count = iv_width + 2.
      lv_position_x = iv_start_x - 1.
    ENDIF.

    DO lv_count TIMES.
      WRITE AT lv_position_x(1) '*' COLOR COL_HEADING INTENSIFIED ON INVERSE OFF.
      lv_position_x = lv_position_x + 1.
    ENDDO.

    IF mv_current_y = 19. " eyes
      lv_position_x = iv_start_x + 1.
      WRITE AT lv_position_x(1) '0' COLOR COL_HEADING INTENSIFIED ON INVERSE OFF.
      lv_position_x = iv_start_x + 3.
      WRITE AT lv_position_x(1) '0' COLOR COL_HEADING INTENSIFIED ON INVERSE OFF.
    ELSEIF mv_current_y = 20. " nose
      lv_position_x = iv_start_x + 2.
      WRITE AT lv_position_x(1) 'o' COLOR COL_HEADING INTENSIFIED ON INVERSE OFF.
    ELSEIF mv_current_y = 21. " mouth and parts of arms
      lv_position_x = iv_start_x + 2.
      WRITE AT lv_position_x(1) '-' COLOR COL_HEADING INTENSIFIED ON INVERSE OFF FRAMES OFF.
      lv_position_x = iv_start_x - 5.
      WRITE AT lv_position_x(1) '\' COLOR COL_HEADING INTENSIFIED ON INVERSE OFF FRAMES OFF.
      lv_position_x = iv_start_x + 9.
      WRITE AT lv_position_x(1) '/' COLOR COL_HEADING INTENSIFIED ON INVERSE OFF FRAMES OFF.
    ELSEIF mv_current_y = 22. " arms
      lv_position_x = iv_start_x - 4.
      WRITE AT lv_position_x(1) '\' COLOR COL_HEADING INTENSIFIED ON INVERSE OFF FRAMES OFF.
      lv_position_x = iv_start_x + 8.
      WRITE AT lv_position_x(1) '/' COLOR COL_HEADING INTENSIFIED ON INVERSE OFF FRAMES OFF.
    ELSEIF mv_current_y = 23. " arms
      lv_position_x = iv_start_x - 3.
      WRITE AT lv_position_x(1) '\' COLOR COL_HEADING INTENSIFIED ON INVERSE OFF FRAMES OFF.
      lv_position_x = iv_start_x + 7.
      WRITE AT lv_position_x(1) '/' COLOR COL_HEADING INTENSIFIED ON INVERSE OFF FRAMES OFF.
    ENDIF.
  ENDMETHOD.


  METHOD output_banner.
    WRITE: AT 2(7)  space COLOR COL_GROUP INTENSIFIED OFF,
           AT 8(7)  space COLOR COL_NEGATIVE INTENSIFIED OFF,
           AT 15(7) space COLOR COL_NEGATIVE,
           AT 59(7) space COLOR COL_NEGATIVE,
           AT 66(7) space COLOR COL_NEGATIVE INTENSIFIED OFF,
           AT 73(7) space COLOR COL_GROUP INTENSIFIED OFF.

    WRITE AT 23(35) 'Merry Christmas and Happy New Year!'  COLOR COL_NEGATIVE INTENSIFIED OFF INVERSE ON.
  ENDMETHOD.


  METHOD output_ground.
    DATA(lv_current_x) = 2.
    DO 78 TIMES.
      WRITE AT lv_current_x(1) '"' COLOR COL_POSITIVE INTENSIFIED ON INVERSE OFF.
      lv_current_x = lv_current_x + 1.
    ENDDO.
  ENDMETHOD.


  METHOD output_frames.
    IF mv_current_y = 1 OR mv_current_y = 28 OR mv_current_y = 30.
      DO 80 TIMES.
        WRITE AT sy-index(1) '-'.
      ENDDO.
    ELSE.
      WRITE: AT 1(1) '|', AT 80(1) '|'.
    ENDIF.
  ENDMETHOD.


  METHOD output_star.
    IF mv_current_y = 3.
      WRITE AT 39(3) '\|/' COLOR COL_NEGATIVE INTENSIFIED OFF INVERSE ON FRAMES OFF.
    ELSEIF mv_current_y = 4.
      WRITE AT 39(3) '-+-' COLOR COL_NEGATIVE INTENSIFIED OFF INVERSE ON.
    ELSEIF mv_current_y = 5.
      WRITE AT 39(3) '/|\' COLOR COL_NEGATIVE INTENSIFIED OFF INVERSE ON FRAMES OFF.
    ENDIF.
  ENDMETHOD.


  METHOD output_fir_needles.
    DATA(lv_current_x) = iv_start_x.
    DO iv_width TIMES.
      WRITE AT lv_current_x(1) mc_fir_needle_char COLOR COL_POSITIVE INTENSIFIED ON INVERSE OFF.
      lv_current_x = lv_current_x + 1.
    ENDDO.
  ENDMETHOD.


  METHOD output_tree_trunk.
    DATA(lv_current_x) = iv_start_x.
    DO iv_width TIMES.
      WRITE AT lv_current_x(1) mc_tree_trunk_char COLOR COL_GROUP INTENSIFIED ON INVERSE OFF.
      lv_current_x = lv_current_x + 1.
    ENDDO.
  ENDMETHOD.


  METHOD output_snowflakes.
    DATA(lr_random_int) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) min = 1 max = 25 ).
    DATA(lv_count) = lr_random_int->get_next( ).
    DO lv_count TIMES.
      lr_random_int = cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) min = 2 max = 79 ).
      DATA(lv_position_x) = lr_random_int->get_next( ).
      IF lv_position_x MOD 2 = 0.
        WRITE AT lv_position_x(1) '*' COLOR COL_NORMAL INTENSIFIED OFF INVERSE ON.
      ELSE.
        WRITE AT lv_position_x(1) '*' COLOR COL_BACKGROUND INTENSIFIED OFF INVERSE ON.
      ENDIF.
    ENDDO.

    " snow on the ground
    IF mv_current_y = 26.
      DO 78 TIMES.
        lv_position_x = sy-index + 1.
        lr_random_int = cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) min = 1 max = 2 ).
        DATA(lv_color) = lr_random_int->get_next( ).
        IF lv_color = 1.
          WRITE AT lv_position_x(1) '*' COLOR COL_BACKGROUND INTENSIFIED OFF INVERSE ON.
        ELSE.
          WRITE AT lv_position_x(1) '*' COLOR COL_NORMAL INTENSIFIED OFF INVERSE ON.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDMETHOD.


  METHOD output_christmas_baubles.
    FIELD-SYMBOLS <baubles_memory> LIKE LINE OF mt_baubles_memory.

    IF iv_width < 4.
      RETURN.
    ENDIF.

    " only every second row
    IF mv_current_y MOD 2 = 0.
      RETURN.
    ENDIF.

    IF line_exists( mt_baubles_memory[ position_y = mv_current_y ] ).
      LOOP AT mt_baubles_memory ASSIGNING <baubles_memory> WHERE position_y = mv_current_y.
        WRITE AT <baubles_memory>-position_x(1) mc_bauble COLOR COL_POSITIVE INTENSIFIED ON INVERSE OFF.
      ENDLOOP.
    ELSE.
      DATA(lv_max) = iv_width - ( iv_width DIV 2 ).

      DATA(lr_random_int) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) min = 1 max = lv_max ).
      DATA(lv_count) = lr_random_int->get_next( ).

      " not too close to the edge
      DATA(lv_min) = iv_start_x + 1.
      lv_max = iv_start_x + iv_width - 2.

      DO lv_count TIMES.
        lr_random_int = cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) min = lv_min max = lv_max ).
        DATA(lv_position_x) = lr_random_int->get_next( ).
        WRITE AT lv_position_x(1) mc_bauble COLOR COL_POSITIVE INTENSIFIED ON INVERSE OFF.
        APPEND INITIAL LINE TO mt_baubles_memory ASSIGNING <baubles_memory>.
        <baubles_memory>-position_x = lv_position_x.
        <baubles_memory>-position_y = mv_current_y.
      ENDDO.
    ENDIF.
  ENDMETHOD.

  METHOD hourglass_expired.
    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = 'REFR'
      EXCEPTIONS
        function_not_supported = 1
        OTHERS                 = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


DATA gr_christmas_demo TYPE REF TO lcl_christmas_demo.


AT USER-COMMAND.
  sy-lsind = 0.
  gr_christmas_demo->let_the_elves_do_their_work( ).


START-OF-SELECTION.
  gr_christmas_demo = NEW lcl_christmas_demo( ).
  gr_christmas_demo->let_the_elves_do_their_work( ).
