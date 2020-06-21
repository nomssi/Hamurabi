*&---------------------------------------------------------------------*
*& Report zhamurabi
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhamurabi.

CLASS lcl_hamurabi_stats DEFINITION.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS: get_year_of_regency RETURNING VALUE(r_result) TYPE numc2,
      get_population RETURNING VALUE(r_result) TYPE i,
      get_amount_stored_bushels RETURNING VALUE(r_result) TYPE i,
      get_amount_acres RETURNING VALUE(r_result) TYPE i,
      get_amount_starved_people RETURNING VALUE(r_result) TYPE i,
      get_amount_bushels_eaten_by_ra RETURNING VALUE(r_result) TYPE i,
      get_amount_cropped_bushels RETURNING VALUE(r_result) TYPE i,
      get_amount_new_citizens RETURNING VALUE(r_result) TYPE i,
      get_current_acre_price RETURNING VALUE(r_result) TYPE i,
      get_game_phase RETURNING VALUE(r_result) TYPE i,
      set_population IMPORTING i_population TYPE i,
      set_amount_stored_bushels IMPORTING i_amount_stored_bushels TYPE i,
      set_amount_acres IMPORTING i_amount_acres TYPE i,
      set_amount_starved_people IMPORTING i_amount_starved_people TYPE i,
      get_amount_bushels_eaten_by_1 RETURNING VALUE(r_result) TYPE i,
      set_amount_cropped_bushels IMPORTING i_amount_cropped_bushels TYPE i,
      set_amount_new_citizens IMPORTING i_amount_new_citizens TYPE i,
      set_current_acre_price IMPORTING i_current_acre_price TYPE i,
      set_game_phase IMPORTING i_game_phase TYPE i,
      set_year_of_regency IMPORTING i_year_of_regency TYPE numc2,
      get_amount_bushels_fed_to RETURNING VALUE(r_result) TYPE i,
      set_amount_bushels_fed_to IMPORTING i_amount_bushels_fed_to_people TYPE i.

  PRIVATE SECTION.
    DATA year_of_regency TYPE numc2.
    DATA population TYPE i.
    DATA amount_stored_bushels TYPE i.
    DATA amount_acres TYPE i.
    DATA amount_starved_people TYPE i.
    DATA amount_bushels_eaten_by_rats TYPE i.
    DATA amount_bushels_fed_to_people TYPE i.
    DATA amount_cropped_bushels TYPE i.
    DATA amount_new_citizens TYPE i.
    DATA current_acre_price TYPE i.
    DATA game_phase TYPE i.

    METHODS set_start_conditions.
ENDCLASS.

**********************************************************************

CLASS lcl_hamurabi_stats IMPLEMENTATION.
  METHOD constructor.
    set_start_conditions( ).
  ENDMETHOD.

  METHOD set_start_conditions.
    year_of_regency = 1.
    population = 95.
    amount_new_citizens = 5.
    amount_starved_people = 0.
    amount_stored_bushels = 2800.
    amount_cropped_bushels = 3000.
    amount_bushels_eaten_by_rats = amount_cropped_bushels - amount_stored_bushels.
    current_acre_price = 3.
    amount_acres = amount_cropped_bushels / current_acre_price.
  ENDMETHOD.

  METHOD get_year_of_regency.
    r_result = me->year_of_regency.
  ENDMETHOD.

  METHOD get_population.
    r_result = me->population.
  ENDMETHOD.

  METHOD get_amount_stored_bushels.
    r_result = me->amount_stored_bushels.
  ENDMETHOD.

  METHOD get_amount_acres.
    r_result = me->amount_acres.
  ENDMETHOD.

  METHOD get_amount_starved_people.
    r_result = me->amount_starved_people.
  ENDMETHOD.

  METHOD get_amount_bushels_eaten_by_ra.
    r_result = me->amount_bushels_eaten_by_rats.
  ENDMETHOD.

  METHOD get_amount_cropped_bushels.
    r_result = me->amount_cropped_bushels.
  ENDMETHOD.

  METHOD get_amount_new_citizens.
    r_result = me->amount_new_citizens.
  ENDMETHOD.

  METHOD get_current_acre_price.
    r_result = me->current_acre_price.
  ENDMETHOD.

  METHOD get_game_phase.
    r_result = me->game_phase.
  ENDMETHOD.

  METHOD set_population.
    me->population = i_population.
  ENDMETHOD.

  METHOD set_amount_stored_bushels.
    me->amount_stored_bushels = i_amount_stored_bushels.
  ENDMETHOD.

  METHOD set_amount_acres.
    me->amount_acres = i_amount_acres.
  ENDMETHOD.

  METHOD set_amount_starved_people.
    me->amount_starved_people = i_amount_starved_people.
  ENDMETHOD.

  METHOD get_amount_bushels_eaten_by_1.
    r_result = me->amount_bushels_eaten_by_rats.
  ENDMETHOD.

  METHOD set_amount_cropped_bushels.
    me->amount_cropped_bushels = i_amount_cropped_bushels.
  ENDMETHOD.

  METHOD set_amount_new_citizens.
    me->amount_new_citizens = i_amount_new_citizens.
  ENDMETHOD.

  METHOD set_current_acre_price.
    me->current_acre_price = i_current_acre_price.
  ENDMETHOD.

  METHOD set_game_phase.
    me->game_phase = i_game_phase.
  ENDMETHOD.

  METHOD set_year_of_regency.
    me->year_of_regency = i_year_of_regency.
  ENDMETHOD.
  METHOD get_amount_bushels_fed_to.
    r_result = me->amount_bushels_fed_to_people.
  ENDMETHOD.

  METHOD set_amount_bushels_fed_to.
    me->amount_bushels_fed_to_people = i_amount_bushels_fed_to_people.
  ENDMETHOD.

ENDCLASS.

**********************************************************************

CLASS lcl_hamurabi_logic DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_stats TYPE REF TO lcl_hamurabi_stats.

    METHODS calculate_new_acre_price.

    METHODS proceed_one_year_in_regency.

    METHODS enough_bushels_to_buy
      IMPORTING
        amount_acres_to_buy TYPE i
      RETURNING
        VALUE(result)       TYPE abap_bool.

    METHODS buy_acres
      IMPORTING
        amount_acres_to_buy TYPE i.

    METHODS enough_acres_to_sell
      IMPORTING
        amount_acres_to_sell TYPE i
      RETURNING
        VALUE(result)        TYPE abap_bool.

    METHODS sell_acres
      IMPORTING
        amount_acres_to_sell TYPE i.

    METHODS enough_bushels_to_feed_people
      IMPORTING
        amount_bushels TYPE i
      RETURNING
        VALUE(result)  TYPE abap_bool.

    METHODS feed_people
      IMPORTING
        amount_bushels TYPE i.

    METHODS enough_acres_to_plant
      IMPORTING
        amount_acres_to_plant TYPE i
      RETURNING
        VALUE(result)         TYPE abap_bool.

    METHODS enough_grain_to_seed
      IMPORTING
        amount_acres_to_plant TYPE i
      RETURNING
        VALUE(result)         TYPE abap_bool.

    METHODS enough_people_to_tend_crops
      IMPORTING
        amount_acres_to_plant TYPE i
      RETURNING
        VALUE(result)         TYPE abap_bool.

    METHODS plant_acres
      IMPORTING
        amount_acres_to_plant TYPE i.

    METHODS harvest
      IMPORTING
        amount_acres_to_plant TYPE i.

    METHODS rat_plague_occured.

    METHODS calculate_new_citizens.

    METHODS calculate_population.

  PRIVATE SECTION.
    DATA mo_stats TYPE REF TO lcl_hamurabi_stats.
ENDCLASS.

**********************************************************************

CLASS lcl_hamurabi_logic IMPLEMENTATION.
  METHOD constructor.
    mo_stats = io_stats.
  ENDMETHOD.

  METHOD calculate_population.
    DATA(full_people) = mo_stats->get_amount_bushels_fed_to( ) / 20.

    IF mo_stats->get_population( ) > full_people.
      mo_stats->set_amount_starved_people( mo_stats->get_population( ) - full_people ).
      mo_stats->set_population( full_people ).
    ENDIF.

    " Plage erst nach der Berechnung der Bevölkerung
    DATA(random_value) = cl_abap_random_int=>create( seed = CONV i( sy-uzeit )
                                               min  = 1
                                               max  = 100 )->get_next( ).

    IF random_value <= 15.
      mo_stats->set_population( mo_stats->get_population( ) / 2 ).
    ENDIF.

  ENDMETHOD.

  METHOD calculate_new_citizens.
    DATA(random_value) = cl_abap_random_int=>create( seed = CONV i( sy-uzeit )
                                                     min  = 1
                                                     max  = 6 )->get_next( ).

    mo_stats->set_amount_new_citizens( random_value * ( 20 * mo_stats->get_amount_acres( ) + mo_stats->get_amount_stored_bushels( ) ) / mo_stats->get_population( ) / ( 100 + 1 ) ).
    mo_stats->set_population( mo_stats->get_population( ) + mo_stats->get_amount_new_citizens( ) ).
  ENDMETHOD.

  METHOD rat_plague_occured.
    DATA(random_value) = cl_abap_random_int=>create( seed = CONV i( sy-uzeit )
                                                     min  = 1
                                                     max  = 6 ).

*    IF ( random_value DIV 2 ) = ( random_value / 2 ).
*      " amount_bushels_eaten_by_rats = amount_stored_bushels / random_value.
*    ENDIF.
  ENDMETHOD.

  METHOD harvest.
    DATA(harvest_per_acre) = cl_abap_random_int=>create( seed = CONV i( sy-uzeit )
                                                     min  = 1
                                                     max  = 6 ).

    DATA(amount_cropped_bushels) = amount_acres_to_plant * harvest_per_acre->get_next( ).
    mo_stats->set_amount_cropped_bushels( amount_cropped_bushels ).

    DATA(new_amount_stored_bushels) = mo_stats->get_amount_stored_bushels( ) - mo_stats->get_amount_bushels_eaten_by_ra( ) + amount_cropped_bushels.
    mo_stats->set_amount_stored_bushels( new_amount_stored_bushels ).
  ENDMETHOD.

  METHOD plant_acres.
    DATA(new_amount_stored_bushels) = mo_stats->get_amount_stored_bushels( ) - ( amount_acres_to_plant / 2 ).
    mo_stats->set_amount_stored_bushels( new_amount_stored_bushels ).
  ENDMETHOD.

  METHOD enough_people_to_tend_crops.
    IF amount_acres_to_plant < ( 10 * mo_stats->get_population( ) ).
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD enough_grain_to_seed.
    IF ( amount_acres_to_plant / 2 ) <= mo_stats->get_amount_stored_bushels( ).
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD enough_acres_to_plant.
    IF amount_acres_to_plant <= mo_stats->get_amount_acres( ).
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD feed_people.
    mo_stats->set_amount_bushels_fed_to( amount_bushels ).
    DATA(new_amount_stored_bushels) = mo_stats->get_amount_stored_bushels( ) - amount_bushels.
    mo_stats->set_amount_stored_bushels( new_amount_stored_bushels ).
  ENDMETHOD.

  METHOD enough_bushels_to_buy.
    IF amount_acres_to_buy * mo_stats->get_current_acre_price( ) <= mo_stats->get_amount_stored_bushels( ).
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD buy_acres.
    DATA(new_amount_acres) = mo_stats->get_amount_acres( ) + amount_acres_to_buy.
    mo_stats->set_amount_acres( new_amount_acres ).

    DATA(new_amount_stored_bushels) = mo_stats->get_amount_stored_bushels( ) - ( mo_stats->get_current_acre_price( ) * amount_acres_to_buy ).
    mo_stats->set_amount_stored_bushels( new_amount_stored_bushels ).
  ENDMETHOD.

  METHOD enough_acres_to_sell.
    IF amount_acres_to_sell < mo_stats->get_amount_acres( ).
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD sell_acres.
    DATA(new_amount_acres) = mo_stats->get_amount_acres( ) - amount_acres_to_sell.
    mo_stats->set_amount_acres( new_amount_acres ).

    DATA(new_amount_stored_bushels) = mo_stats->get_amount_stored_bushels( ) + ( mo_stats->get_current_acre_price( ) * amount_acres_to_sell ).
    mo_stats->set_amount_stored_bushels( new_amount_stored_bushels ).
  ENDMETHOD.

  METHOD enough_bushels_to_feed_people.
    IF amount_bushels <= mo_stats->get_amount_stored_bushels( ).
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD proceed_one_year_in_regency.
    mo_stats->set_year_of_regency( CONV numc2( mo_stats->get_year_of_regency( ) + 1 ) ).
    calculate_population( ).
    calculate_new_citizens( ).
    calculate_new_acre_price( ).
  ENDMETHOD.

  METHOD calculate_new_acre_price.
    mo_stats->set_current_acre_price( cl_abap_random_int=>create( seed = CONV i( sy-uzeit ) min = 17 max = 27 )->get_next( ) ).
  ENDMETHOD.

ENDCLASS.

**********************************************************************

CLASS lcl_hamurabi_classic_ui DEFINITION.
  PUBLIC SECTION.
    METHODS start_game.

    METHODS constructor.

    METHODS handle_user_action
      IMPORTING
        user_input TYPE i
      CHANGING
        screen     LIKE sy-lsind.

    METHODS output_title.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF screen_enum,
                 report    TYPE i VALUE 0,
                 buy       TYPE i VALUE 1,
                 sell      TYPE i VALUE 2,
                 feed      TYPE i VALUE 3,
                 plant     TYPE i VALUE 4,
                 round_end TYPE i VALUE 5,
               END OF screen_enum.

    DATA mo_stats TYPE REF TO lcl_hamurabi_stats.
    DATA mo_logic TYPE REF TO lcl_hamurabi_logic.

    METHODS output_title_screen.

    METHODS output_statistics.

    METHODS output_report.

    METHODS handle_buy_phase
      IMPORTING
        user_input TYPE i.

    METHODS handle_sell_phase
      IMPORTING
        user_input TYPE i
      CHANGING
        screen     LIKE sy-lsind.

    METHODS handle_feed_phase
      IMPORTING
        user_input TYPE i
      CHANGING
        screen     TYPE syst-lsind.

    METHODS handle_plant_phase
      IMPORTING
        user_input TYPE i
      CHANGING
        screen     TYPE syst-lsind.

    METHODS handle_round_end
      IMPORTING
        user_input TYPE i
      CHANGING
        screen     TYPE syst-lsind.
ENDCLASS.

**********************************************************************

CLASS lcl_hamurabi_classic_ui IMPLEMENTATION.
  METHOD constructor.
    mo_stats = NEW lcl_hamurabi_stats( ).
    mo_logic = NEW lcl_hamurabi_logic( mo_stats ).
  ENDMETHOD.

  METHOD start_game.
    output_title_screen( ).
  ENDMETHOD.

  METHOD output_title.
    WRITE 'Hamurabi' COLOR COL_POSITIVE.
    ULINE.
  ENDMETHOD.

  METHOD output_title_screen.
    output_statistics( ).
    output_report( ).
    mo_stats->set_game_phase( 2 ).
  ENDMETHOD.

  METHOD handle_user_action.
    CASE screen.
      WHEN screen_enum-buy.
        handle_buy_phase( user_input ).

      WHEN screen_enum-sell.
        handle_sell_phase(
          EXPORTING
            user_input = user_input
          CHANGING
            screen = screen ).

      WHEN screen_enum-feed.
        handle_feed_phase(
          EXPORTING
            user_input = user_input
          CHANGING
            screen = screen ).

      WHEN screen_enum-plant.
        handle_plant_phase(
        EXPORTING
          user_input = user_input
        CHANGING
          screen = screen ).

      WHEN screen_enum-round_end.
        handle_round_end(
          EXPORTING
            user_input = user_input
          CHANGING
            screen = screen ).
    ENDCASE.
  ENDMETHOD.

  METHOD handle_round_end.
    IF mo_logic->enough_acres_to_plant( user_input ).
      IF mo_logic->enough_grain_to_seed( user_input ).
        IF mo_logic->enough_people_to_tend_crops( user_input ).
          mo_logic->plant_acres( user_input ).
          mo_stats->set_game_phase( 6 ).
        ELSE.
          MESSAGE 'Not enough people to tend the crops.' TYPE 'S'.
        ENDIF.
      ELSE.
        MESSAGE 'Not enough grain for seed.' TYPE 'S'.
      ENDIF.
    ELSE.
      MESSAGE 'Not enough acres to plant.' TYPE 'S'.
    ENDIF.

    IF mo_stats->get_game_phase( ) = 6.
      screen = 0.
      mo_stats->set_game_phase( 2 ).
      mo_logic->proceed_one_year_in_regency( ).
      output_report( ).
    ELSE.
      screen = screen_enum-plant.
    ENDIF.
  ENDMETHOD.

  METHOD handle_plant_phase.
    IF mo_logic->enough_bushels_to_feed_people( user_input ).
      mo_logic->feed_people( user_input ).
      mo_stats->set_game_phase( 5 ).
    ELSE.
      MESSAGE 'Not enough bushels to feed.' TYPE 'S'.
    ENDIF.

    IF mo_stats->get_game_phase( ) = 5.
      output_statistics( ).
      WRITE: / 'How many acres do you wish to plant with seed?', user_input LEFT-JUSTIFIED COLOR COL_POSITIVE INPUT ON, icon_okay AS ICON.
    ELSE.
      screen = screen_enum-feed.
    ENDIF.
  ENDMETHOD.

  METHOD handle_feed_phase.
    IF mo_logic->enough_acres_to_sell( user_input ).
      mo_logic->sell_acres( user_input ).
      mo_stats->set_game_phase( 4 ).
    ELSE.
      MESSAGE 'Not enough acres to sell.' TYPE 'S'.
    ENDIF.

    IF mo_stats->get_game_phase( ) = 4.
      output_statistics( ).
      WRITE: / 'How many bushels do you wish to feed your people (20 per citizen)?', user_input LEFT-JUSTIFIED COLOR COL_POSITIVE INPUT ON, icon_okay AS ICON.
    ELSE.
      screen = screen_enum-sell.
    ENDIF.
  ENDMETHOD.

  METHOD handle_sell_phase.
    IF mo_logic->enough_bushels_to_buy( user_input ).
      mo_logic->buy_acres( user_input ).
      mo_stats->set_game_phase( 3 ).
    ELSE.
      MESSAGE 'You have not enough bushels to buy.' TYPE 'S'.
    ENDIF.

    IF mo_stats->get_game_phase( ) = 3.
      output_statistics( ).
      WRITE: / 'Land is trading at ', mo_stats->get_current_acre_price( ) LEFT-JUSTIFIED, ' bushels'.
      WRITE: / 'How many acres do you wish to sell?', user_input LEFT-JUSTIFIED COLOR COL_POSITIVE INPUT ON, icon_okay AS ICON.
    ELSE.
      screen = screen_enum-buy.
    ENDIF.
  ENDMETHOD.

  METHOD handle_buy_phase.
    IF mo_stats->get_game_phase( ) = 2.
      mo_stats->set_game_phase( 2 ).
      mo_logic->calculate_new_acre_price( ).

      output_statistics( ).
      WRITE: / 'Land is trading at ', mo_stats->get_current_acre_price( ) LEFT-JUSTIFIED, ' bushels'.
      WRITE: / 'How many acres do you wish to buy? ', user_input LEFT-JUSTIFIED COLOR COL_POSITIVE INPUT ON, icon_okay AS ICON.
    ENDIF.
  ENDMETHOD.

  METHOD output_statistics.
    WRITE: / icon_date AS ICON, 'Year: ', mo_stats->get_year_of_regency( ) LEFT-JUSTIFIED,
             icon_overview AS ICON, 'Acres: ', mo_stats->get_amount_acres( ) LEFT-JUSTIFIED,
             icon_stock AS ICON, 'Bushels: ', mo_stats->get_amount_stored_bushels( ) LEFT-JUSTIFIED,
             icon_new_employee AS ICON, 'Population: ', mo_stats->get_population( ) LEFT-JUSTIFIED.
    ULINE.
  ENDMETHOD.

  METHOD output_report.
    WRITE: / 'I beg to report to you, in year ', mo_stats->get_year_of_regency( ) LEFT-JUSTIFIED.
    WRITE: / mo_stats->get_amount_starved_people( ) LEFT-JUSTIFIED, ' people starved.'.
    WRITE: / mo_stats->get_amount_new_citizens( ) LEFT-JUSTIFIED, ' came to the city.'.
    WRITE: / 'Start to rule!', icon_okay AS ICON.
  ENDMETHOD.

ENDCLASS.

**********************************************************************

DATA hamurabi_classic_ui TYPE REF TO lcl_hamurabi_classic_ui.
DATA user_input TYPE i.

**********************************************************************

TOP-OF-PAGE DURING LINE-SELECTION.
  hamurabi_classic_ui->output_title( ).

START-OF-SELECTION.
  hamurabi_classic_ui = NEW lcl_hamurabi_classic_ui( ).
  hamurabi_classic_ui->start_game( ).

AT LINE-SELECTION.
  READ CURRENT LINE FIELD VALUE user_input INTO user_input.
  hamurabi_classic_ui->handle_user_action(
                         EXPORTING
                           user_input = user_input
                         CHANGING
                           screen = sy-lsind ).
