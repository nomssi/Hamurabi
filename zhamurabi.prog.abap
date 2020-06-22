REPORT zz_hamurabi.

TYPES: BEGIN OF state,
         year_of_regency              TYPE numc2,
         game_phase                   TYPE i,

         population                   TYPE i,
         amount_stored_bushels        TYPE i,
         amount_acres                 TYPE i,
         amount_starved_people        TYPE i,
         amount_cropped_bushels       TYPE i,
         amount_new_citizens          TYPE i,
         current_acre_price           TYPE i,

         amount_bushels_fed_to_people TYPE i,
         amount_bushels_eaten_by_rats TYPE i,
       END OF state.

CONSTANTS: BEGIN OF phase,
             init      TYPE i VALUE 0,
             buy       TYPE i VALUE 1,
             sell      TYPE i VALUE 2,
             feed      TYPE i VALUE 3,
             plant     TYPE i VALUE 4,
             round_end TYPE i VALUE 5,
             report    TYPE i VALUE 6,
           END OF phase.

CLASS lcl_regency DEFINITION.

  PUBLIC SECTION.
    DATA stats TYPE state READ-ONLY.

    METHODS constructor IMPORTING is_stats TYPE state.

    METHODS set_phase IMPORTING phase TYPE i.

    METHODS simulate_one_year IMPORTING acres_harvested TYPE i.

    METHODS try_to_sell_land IMPORTING acres TYPE i.

    METHODS try_to_grow_crops IMPORTING acres TYPE i.

    METHODS try_to_feed_people IMPORTING bushels TYPE i.

    METHODS try_to_buy_land IMPORTING acres TYPE i.

    METHODS calculate_new_acre_price.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS calculate_new_citizens.

    METHODS calculate_population_loss.

    METHODS harvest IMPORTING amount_acres_to_plant TYPE i.

    METHODS buy_acres IMPORTING amount_acres_to_buy TYPE i.

    METHODS sell_acres IMPORTING amount_acres_to_sell TYPE i.

    METHODS feed_people IMPORTING amount_bushels TYPE i.

    METHODS plant_acres IMPORTING amount_acres_to_plant TYPE i.

    METHODS enough_bushels_to_feed_people
      IMPORTING amount_bushels TYPE i
      RETURNING VALUE(result)  TYPE abap_bool.

    METHODS enough_acres_to_plant
      IMPORTING amount_acres_to_plant TYPE i
      RETURNING VALUE(result)         TYPE abap_bool.

    METHODS enough_grain_to_seed
      IMPORTING amount_acres_to_plant TYPE i
      RETURNING VALUE(result)         TYPE abap_bool.

    METHODS enough_people_to_tend_crops
      IMPORTING amount_acres_to_plant TYPE i
      RETURNING VALUE(result)         TYPE abap_bool.

    METHODS enough_bushels_to_buy
      IMPORTING amount_acres_to_buy TYPE i
      RETURNING VALUE(result)       TYPE abap_bool.

    METHODS enough_acres_to_sell
      IMPORTING amount_acres_to_sell TYPE i
      RETURNING VALUE(result)        TYPE abap_bool.

    CLASS-METHODS random IMPORTING min           TYPE i DEFAULT 1
                                   max           TYPE i
                         RETURNING VALUE(rv_int) TYPE i.
ENDCLASS.

**********************************************************************

CLASS lcl_regency IMPLEMENTATION.

  METHOD constructor.
    stats = is_stats.
    stats-amount_bushels_eaten_by_rats = stats-amount_cropped_bushels - stats-amount_stored_bushels.
    stats-amount_acres = stats-amount_cropped_bushels / stats-current_acre_price.
  ENDMETHOD.

  METHOD set_phase.
    stats-game_phase = phase.
  ENDMETHOD.

  METHOD calculate_population_loss.
    " How many starved?
    DATA(full_people) = stats-amount_bushels_fed_to_people / 20.

    IF stats-population > full_people.
      stats-amount_starved_people = stats-population - full_people.
      stats-population = full_people.
    ENDIF.

    " If there was a plague, 50% died
    IF random( 100 ) <= 15.
      stats-population = stats-population / 2.
    ENDIF.

  ENDMETHOD.

  METHOD calculate_new_citizens.
    " how many came to the city
    stats-amount_new_citizens = random( 6 ) * ( 20 * stats-amount_acres + stats-amount_stored_bushels ) / stats-population / ( 100 + 1 ).
    stats-population = stats-population + stats-amount_new_citizens.
  ENDMETHOD.

  METHOD harvest.
    stats-amount_cropped_bushels = amount_acres_to_plant * random( 6 ).
    stats-amount_stored_bushels = stats-amount_stored_bushels - stats-amount_bushels_eaten_by_rats + stats-amount_cropped_bushels.
  ENDMETHOD.

  METHOD plant_acres.
    stats-amount_stored_bushels = stats-amount_stored_bushels - ( amount_acres_to_plant / 2 ).
  ENDMETHOD.

  METHOD try_to_grow_crops.
    CHECK enough_acres_to_plant( acres )
      AND enough_grain_to_seed( acres )
      AND enough_people_to_tend_crops( acres ).
    plant_acres( acres ).
    set_phase( phase-report ).
  ENDMETHOD.

  METHOD enough_people_to_tend_crops.
    result = xsdbool( amount_acres_to_plant < ( 10 * stats-population ) ).
    CHECK result EQ abap_false.
    MESSAGE 'Not enough people to tend the crops.' TYPE 'S'.
  ENDMETHOD.

  METHOD enough_grain_to_seed.
    result = xsdbool( ( amount_acres_to_plant / 2 ) <= stats-amount_stored_bushels ).
    CHECK result EQ abap_false.
    MESSAGE 'Not enough grain for seed.' TYPE 'S'.
  ENDMETHOD.

  METHOD enough_acres_to_plant.
    result = xsdbool( amount_acres_to_plant <= stats-amount_acres ).
    CHECK result EQ abap_false.
    MESSAGE 'Not enough acres to plant.' TYPE 'S'.
  ENDMETHOD.

  METHOD feed_people.
    stats-amount_bushels_fed_to_people = amount_bushels.
    stats-amount_stored_bushels = stats-amount_stored_bushels - amount_bushels.
  ENDMETHOD.

  METHOD try_to_buy_land.
    CHECK enough_bushels_to_buy( acres ).
    buy_acres( acres ).
    set_phase( phase-feed ).
  ENDMETHOD.

  METHOD enough_bushels_to_buy.
    result = xsdbool( amount_acres_to_buy * stats-current_acre_price <= stats-amount_stored_bushels ).
    CHECK result EQ abap_false.
    MESSAGE 'You have not enough bushels to buy.' TYPE 'S'.
  ENDMETHOD.

  METHOD buy_acres.
    stats-amount_acres = stats-amount_acres + amount_acres_to_buy.
    stats-amount_stored_bushels = stats-amount_stored_bushels - ( stats-current_acre_price * amount_acres_to_buy ).
  ENDMETHOD.

  METHOD try_to_sell_land.
    CHECK enough_acres_to_sell( acres ).
    sell_acres( acres ).
    set_phase( phase-plant ).
  ENDMETHOD.

  METHOD enough_acres_to_sell.
    result = xsdbool( amount_acres_to_sell < stats-amount_acres ).
    CHECK result EQ abap_false.
    MESSAGE 'Not enough acres to sell.' TYPE 'S'.
  ENDMETHOD.

  METHOD sell_acres.
    SUBTRACT amount_acres_to_sell FROM stats-amount_acres.
    stats-amount_stored_bushels = stats-amount_stored_bushels + ( stats-current_acre_price * amount_acres_to_sell ).
  ENDMETHOD.

  METHOD try_to_feed_people.
    CHECK enough_bushels_to_feed_people( bushels ).
    feed_people( bushels ).
    set_phase( phase-round_end ).
  ENDMETHOD.

  METHOD enough_bushels_to_feed_people.
    result = xsdbool( amount_bushels <= stats-amount_stored_bushels ).
    CHECK result EQ abap_false.
    MESSAGE 'Not enough bushels to feed.' TYPE 'S'.
  ENDMETHOD.

  METHOD simulate_one_year.
    stats-year_of_regency = CONV numc2( stats-year_of_regency + 1 ).
    calculate_population_loss( ).
    calculate_new_citizens( ).
    harvest( acres_harvested ).
    calculate_new_acre_price( ).
  ENDMETHOD.

  METHOD calculate_new_acre_price.
    stats-current_acre_price = random( min = 17 max = 27 ).
  ENDMETHOD.

  METHOD random.
    rv_int = cl_abap_random_int=>create( seed = CONV i( sy-uzeit )
                                         min  = min
                                         max  = max )->get_next( ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************

CLASS lcl_hamurabi_ui DEFINITION.
  PUBLIC SECTION.
    METHODS start_game.

    METHODS constructor.

    METHODS handle_user_action
      IMPORTING user_input TYPE i
      CHANGING screen     LIKE sy-lsind.

    CLASS-METHODS output_title.

  PRIVATE SECTION.
    DATA mo_game TYPE REF TO lcl_regency.

    METHODS output_title_screen IMPORTING stats TYPE state.

    METHODS output_statistics IMPORTING stats TYPE state.
    METHODS output_report IMPORTING stats TYPE state.

    METHODS handle_buy_phase IMPORTING user_input TYPE i.

    METHODS handle_sell_phase IMPORTING user_input TYPE i
                              CHANGING  screen     LIKE sy-lsind.

    METHODS handle_feed_phase IMPORTING user_input TYPE i
                              CHANGING  screen     TYPE syst-lsind.

    METHODS handle_plant_phase IMPORTING user_input TYPE i
                               CHANGING  screen     TYPE syst-lsind.

    METHODS handle_round_end IMPORTING user_input TYPE i
                             CHANGING  screen     TYPE syst-lsind.
ENDCLASS.

**********************************************************************

CLASS lcl_hamurabi_ui IMPLEMENTATION.

  METHOD constructor.
    mo_game = NEW lcl_regency( VALUE #( year_of_regency = 1
                                        population = 95
                                        amount_new_citizens = 5
                                        amount_starved_people = 0
                                        amount_stored_bushels = 2800
                                        amount_cropped_bushels = 3000
                                        current_acre_price = 3 ) ).
  ENDMETHOD.

  METHOD start_game.
    output_title_screen( mo_game->stats ).
  ENDMETHOD.

  METHOD output_title.
    WRITE 'Hamurabi' COLOR COL_POSITIVE.
    ULINE.
  ENDMETHOD.

  METHOD output_title_screen.
    output_statistics( stats ).
    output_report( stats ).
    mo_game->set_phase( phase-sell ).
  ENDMETHOD.

  METHOD handle_user_action.
    CASE screen.
      WHEN phase-buy.
        handle_buy_phase( user_input ).

      WHEN phase-sell.
        handle_sell_phase( EXPORTING user_input = user_input
                           CHANGING screen = screen ).

      WHEN phase-feed.
        handle_feed_phase( EXPORTING user_input = user_input
                           CHANGING screen = screen ).

      WHEN phase-plant.
        handle_plant_phase( EXPORTING user_input = user_input
                            CHANGING screen = screen ).

      WHEN phase-round_end.
        handle_round_end( EXPORTING user_input = user_input
                          CHANGING screen = screen ).
    ENDCASE.
  ENDMETHOD.

  METHOD handle_round_end.
    mo_game->try_to_grow_crops( user_input ).

    IF mo_game->stats-game_phase = phase-report.
      screen = phase-init.
      mo_game->set_phase( phase-sell ).

      mo_game->simulate_one_year( user_input ).
      output_report( mo_game->stats ).
    ELSE.
      screen = phase-plant.
    ENDIF.
  ENDMETHOD.

  METHOD handle_plant_phase.
    mo_game->try_to_feed_people( user_input ).

    IF mo_game->stats-game_phase = phase-round_end.
      output_statistics( mo_game->stats ).
      WRITE: / 'How many acres do you wish to plant with seed?', user_input LEFT-JUSTIFIED COLOR COL_POSITIVE INPUT ON, icon_okay AS ICON.
    ELSE.
      screen = phase-feed.
    ENDIF.
  ENDMETHOD.

  METHOD handle_feed_phase.
    mo_game->try_to_sell_land( user_input ).

    IF mo_game->stats-game_phase = phase-plant.
      output_statistics( mo_game->stats ).
      WRITE: / 'How many bushels do you wish to feed your people (20 per citizen)?', user_input LEFT-JUSTIFIED COLOR COL_POSITIVE INPUT ON, icon_okay AS ICON.
    ELSE.
      screen = phase-sell.
    ENDIF.
  ENDMETHOD.

  METHOD handle_sell_phase.
    mo_game->try_to_buy_land( user_input ).

    IF mo_game->stats-game_phase = phase-feed.
      output_statistics( mo_game->stats ).
      WRITE: / 'Land is trading at ', mo_game->stats-current_acre_price LEFT-JUSTIFIED, ' bushels'.
      WRITE: / 'How many acres do you wish to sell?', user_input LEFT-JUSTIFIED COLOR COL_POSITIVE INPUT ON, icon_okay AS ICON.
    ELSE.
      screen = phase-buy.
    ENDIF.
  ENDMETHOD.

  METHOD handle_buy_phase.
    IF mo_game->stats-game_phase = phase-sell.
      mo_game->set_phase( phase-sell ).
      mo_game->calculate_new_acre_price( ).

      output_statistics( mo_game->stats ).
      WRITE: / 'Land is trading at ', mo_game->stats-current_acre_price LEFT-JUSTIFIED, ' bushels'.
      WRITE: / 'How many acres do you wish to buy? ', user_input LEFT-JUSTIFIED COLOR COL_POSITIVE INPUT ON, icon_okay AS ICON.
    ENDIF.
  ENDMETHOD.

  METHOD output_statistics.
    WRITE: / icon_date AS ICON, 'Year: ', stats-year_of_regency LEFT-JUSTIFIED,
             icon_overview AS ICON, 'Acres: ', stats-amount_acres LEFT-JUSTIFIED,
             icon_stock AS ICON, 'Bushels: ', stats-amount_stored_bushels LEFT-JUSTIFIED,
             icon_new_employee AS ICON, 'Population: ', stats-population LEFT-JUSTIFIED.
    ULINE.
  ENDMETHOD.

  METHOD output_report.
    WRITE: / 'I beg to report to you, in year ', stats-year_of_regency LEFT-JUSTIFIED.
    WRITE: / stats-amount_starved_people LEFT-JUSTIFIED, ' people starved.'.
    WRITE: / stats-amount_new_citizens LEFT-JUSTIFIED, ' came to the city.'.
    WRITE: / 'Start to rule!', icon_okay AS ICON.
  ENDMETHOD.

ENDCLASS.

**********************************************************************

DATA hamurabi_classic_ui TYPE REF TO lcl_hamurabi_ui.
DATA user_input TYPE i.

**********************************************************************

TOP-OF-PAGE DURING LINE-SELECTION.
  lcl_hamurabi_ui=>output_title( ).

START-OF-SELECTION.
  hamurabi_classic_ui = NEW lcl_hamurabi_ui( ).
  hamurabi_classic_ui->start_game( ).

AT LINE-SELECTION.
  READ CURRENT LINE FIELD VALUE user_input INTO user_input.
  hamurabi_classic_ui->handle_user_action(
                         EXPORTING user_input = user_input
                         CHANGING screen = sy-lsind ).
